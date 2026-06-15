#!/usr/bin/env python3
"""
swat_calltree.py — Parse SWAT+ Fortran source and produce a call tree in
Markdown, per-subroutine flow charts, or verify an existing Markdown call tree
against the source.

Usage (run from project root):
  python analysis/swat_calltree.py --generate doc/generated_call_tree.md
  python analysis/swat_calltree.py --verify   doc/swatplus_call_tree.md
  python analysis/swat_calltree.py --flow     doc/swatplus_sub_flow.md
  python analysis/swat_calltree.py --flow     doc/swatplus_sub_flow.md \\
                                    --update-links doc/swatplus_call_tree.md

Options:
  --src DIR            Fortran source directory  (default: ./src)
  --start SUB          Root subroutine           (default: main)
  --generate FILE      Write generated call tree Markdown to FILE
  --verify FILE        Compare source call graph against existing Markdown FILE
                       and print a discrepancy report
  --flow FILE          Write per-subroutine flow chart Markdown to FILE
  --update-links FILE  Patch an existing call tree Markdown to add flow links
                       (use together with --flow)
  --leaves             Include leaf-node sections in generated output
                       (default: omit — they have no sub-calls to show)
"""

import re
import os
import sys
import glob
import argparse
from collections import defaultdict, deque

# ---------------------------------------------------------------------------
# Fortran intrinsics / system routines to exclude from the call graph
# ---------------------------------------------------------------------------
INTRINSICS = frozenset({
    'date_and_time', 'cpu_time', 'exit', 'iargc', 'getarg',
    'system_clock', 'get_command_argument', 'command_argument_count',
    'flush', 'mvbits', 'random_number', 'random_seed', 'system',
    'get_environment_variable',
    # numeric / string intrinsics (sometimes appear after "call" in SWAT+)
    'allocated', 'associated', 'present',
})

# Headings in the existing doc that are NOT subroutine names
NON_SUB_HEADINGS = frozenset({
    'carbon pool flow in `cbn_zhang2`',
    'tillage mixing — `mgt_newtillmix_cswat1`',
    'hru output files controlled by `print.prt`',
    'subsystem subroutines',
})

# Short tokens that appear in tree lines but are not subroutine names
NON_SUB_TOKENS = frozenset({
    'the', 'and', 'for', 'not', 'end', 'all', 'set', 'get', 'new',
    'use', 'via', 'per', 'day', 'mon', 'yr', 'aa', 'cb', 'if', 'cs',
    'no3', 'nh4', 'co2', 'sut', 'cdg', 'ox', 'bsn', 'hru', 'ch',
    'aqu', 'res', 'ru', 'lsu', 'sb', 'dr', 'ob', 'pco', 'sp',
    'this', 'from', 'that', 'with', 'into', 'out', 'sub', 'add',
    'run', 'read', 'write', 'open', 'close', 'print', 'stop',
})


# ===========================================================================
# STEP 1: Parse Fortran source
# ===========================================================================

def _join_continuations(raw_lines):
    """Merge Fortran continuation lines (trailing &) into single logical lines."""
    result = []
    buf = ''
    for line in raw_lines:
        stripped = line.rstrip('\n').rstrip()
        if stripped.endswith('&'):
            buf += stripped[:-1].rstrip() + ' '
        else:
            result.append(buf + stripped)
            buf = ''
    if buf:
        result.append(buf)
    return result


def _strip_comment(line):
    """Remove everything from the first ! that is not inside a string literal."""
    in_str, qch = False, ''
    for i, ch in enumerate(line):
        if ch in ('"', "'") and not in_str:
            in_str, qch = True, ch
        elif in_str and ch == qch:
            in_str = False
        elif ch == '!' and not in_str:
            return line[:i]
    return line


def _extract_description(raw_lines):
    """
    Return a one-line description from SWAT+ Fortran comments.
    Handles the '!! ~ ~ ~ PURPOSE ~ ~ ~' convention and plain ! comments.
    """
    in_purpose = False
    for line in raw_lines[:60]:
        s = line.strip()
        if '~ ~ ~ PURPOSE ~ ~ ~' in s or '~purpose~' in s.lower():
            in_purpose = True
            continue
        if in_purpose:
            if s.startswith('!!') or s.startswith('!'):
                desc = s.lstrip('!').strip()
                if desc and '~' not in desc and len(desc) > 3:
                    return desc[:120]
            elif s:
                in_purpose = False
    # Fallback: first plain comment line after the subroutine declaration
    past_sub = False
    for line in raw_lines[:15]:
        s = line.strip()
        if re.match(r'(subroutine|program)\b', s, re.IGNORECASE):
            past_sub = True
            continue
        if past_sub and s.startswith('!'):
            desc = s.lstrip('!').strip()
            if desc and len(desc) > 3:
                return desc[:120]
        elif past_sub and s:
            break
    return ''


def _find_condition(line):
    """
    If `line` is an inline conditional call — "if (cond) call foo" — return
    (condition_str, callee).  Otherwise return (None, None).
    Handles nested parentheses correctly.
    """
    m = re.search(r'\bif\s*\(', line, re.IGNORECASE)
    if not m:
        return None, None
    start = m.end() - 1          # index of opening '('
    depth = 0
    for i in range(start, len(line)):
        c = line[i]
        if c == '(':
            depth += 1
        elif c == ')':
            depth -= 1
            if depth == 0:
                rest = line[i + 1:].strip()
                cm = re.match(r'call\s+(\w+)', rest, re.IGNORECASE)
                if cm:
                    cond = line[start + 1:i].strip()
                    if len(cond) > 65:
                        cond = cond[:62] + '...'
                    return cond, cm.group(1).lower()
                return None, None
    return None, None


def parse_source(src_dir):
    """
    Scan every .f90 / .F90 / .f95 / .F95 file in *src_dir*.

    Returns
    -------
    sub_info : dict
        { sub_name_lower : {
              'file'        : 'filename.f90',
              'description' : 'one-line description',
              'calls'       : [(callee_lower, condition_or_None), ...]
          } }
    """
    pattern = os.path.join(src_dir, '*.[fF]9[05]')
    sub_info = {}

    SUB_START = re.compile(r'^\s*(subroutine|program)\s+(\w+)', re.IGNORECASE)
    SUB_END   = re.compile(r'^\s*end\s*(subroutine|program)', re.IGNORECASE)
    CALL_ONLY = re.compile(r'^\s+call\s+(\w+)', re.IGNORECASE)

    for filepath in sorted(glob.glob(pattern)):
        fname = os.path.basename(filepath)
        try:
            with open(filepath, errors='ignore') as fh:
                raw = fh.readlines()
        except OSError:
            continue

        logical = _join_continuations(raw)
        clean   = [_strip_comment(l) for l in logical]

        current_sub = None

        for line in clean:
            # ---- subroutine / program start --------------------------------
            m = SUB_START.match(line)
            if m:
                current_sub = m.group(2).lower()
                if current_sub not in sub_info:
                    sub_info[current_sub] = {
                        'file':        fname,
                        'description': _extract_description(raw),
                        'calls':       [],
                    }
                continue

            # ---- end of subroutine -----------------------------------------
            if SUB_END.match(line):
                current_sub = None
                continue

            if current_sub is None:
                continue

            # ---- inline conditional call: if (cond) call foo ---------------
            cond, callee = _find_condition(line)
            if callee and callee not in INTRINSICS:
                sub_info[current_sub]['calls'].append((callee, cond))
                continue

            # ---- direct call -----------------------------------------------
            m2 = CALL_ONLY.match(line)
            if m2:
                callee = m2.group(1).lower()
                if callee not in INTRINSICS:
                    sub_info[current_sub]['calls'].append((callee, None))

    # Deduplicate while preserving first-seen order
    for info in sub_info.values():
        seen = {}
        deduped = []
        for callee, cond in info['calls']:
            if callee not in seen:
                seen[callee] = cond
                deduped.append((callee, cond))
        info['calls'] = deduped

    return sub_info


# ===========================================================================
# STEP 2: Graph utilities
# ===========================================================================

def build_reverse(sub_info, known):
    """Return reverse graph: {callee: set(callers)} restricted to known subs."""
    rev = defaultdict(set)
    for sub, info in sub_info.items():
        for callee, _ in info['calls']:
            if callee in known:
                rev[callee].add(sub)
    return rev


def bfs(start, sub_info, known):
    """BFS from *start*; only traverse edges to subroutines in *known*."""
    visited, order = set(), []
    queue = deque([start])
    while queue:
        sub = queue.popleft()
        if sub in visited:
            continue
        visited.add(sub)
        order.append(sub)
        for callee, _ in sub_info.get(sub, {}).get('calls', []):
            if callee in known and callee not in visited:
                queue.append(callee)
    return order, visited


# ===========================================================================
# STEP 3: Markdown generation
# ===========================================================================

def _anchor(name):
    """GitHub-style anchor for a heading."""
    a = name.lower()
    a = re.sub(r'[^\w\s\-]', '', a)
    a = re.sub(r'\s+', '-', a.strip())
    return re.sub(r'-+', '-', a)


def _html_escape(s):
    """Escape HTML special characters for use inside <pre> blocks."""
    return s.replace('&', '&amp;').replace('<', '&lt;').replace('>', '&gt;')


def _tree_block(sub, calls, known):
    """Render the <pre> call-tree block for *sub*."""
    lines = [sub]
    known_calls = [(c, cond) for c, cond in calls if c in known]
    extern_calls = [(c, cond) for c, cond in calls if c not in known]
    all_calls = known_calls + extern_calls
    for i, (callee, cond) in enumerate(all_calls):
        last   = (i == len(all_calls) - 1)
        prefix = '└── ' if last else '├── '
        cond_s = f'[if {cond}]  ' if cond else ''
        if callee in known:
            name_s = f'<a href="#{_anchor(callee)}">{callee}</a>'
        else:
            name_s = callee
        lines.append(f'{prefix}{cond_s}{name_s}')
    if extern_calls:
        n = len(extern_calls)
        lines.append(f'[+ {n} call{"s" if n>1 else ""} to external/library routines]')
    return lines


def generate_markdown(sub_info, start='main', include_leaves=False):
    """
    Generate a complete Markdown call tree document.

    Parameters
    ----------
    sub_info       : output of parse_source()
    start          : root subroutine name
    include_leaves : if True, also emit sections for leaf subroutines
    """
    known = set(sub_info)
    rev   = build_reverse(sub_info, known)
    order, reachable = bfs(start, sub_info, known)

    out = []
    out.append('# SWAT+ Call Tree from `main.f90`\n')
    out.append('Auto-generated from source by `analysis/swat_calltree.py`.')
    out.append('Branches prefixed with `[if ...]` are conditional.')
    out.append("Click any linked name to jump to that routine's section.\n")
    out.append('---\n')

    for sub in order:
        info  = sub_info[sub]
        calls = info['calls']
        known_calls = [(c, cond) for c, cond in calls if c in known]

        if not known_calls and not include_leaves:
            continue   # skip leaf nodes unless requested

        out.append(f'## {sub}\n')

        if info['description']:
            out.append(info['description'] + '\n')

        # Called from
        parents = sorted(rev.get(sub, set()) & reachable)
        if parents:
            links = ', '.join(f'[`{p}`](#{_anchor(p)})' for p in parents)
            out.append(f'**Called from:** {links}\n')

        out.append(f'Source: `{info["file"]}`\n')

        if calls:
            out.append('<pre>')
            out.extend(_tree_block(sub, calls, known))
            out.append('</pre>')
        else:
            out.append('*Leaf node — no calls to other subroutines.*')

        out.append('\n---\n')

    return '\n'.join(out)


# ===========================================================================
# STEP 4: Flow chart generation
# ===========================================================================

def generate_flow_markdown(sub_info, start='main',
                           call_tree_name='swatplus_call_tree.md'):
    """
    Generate swatplus_sub_flow.md with per-subroutine sequential flow diagrams.

    Each non-leaf subroutine reachable from *start* gets a section showing
    the ordered call sequence, conditional branches, and back-links to the
    call tree.

    Parameters
    ----------
    sub_info        : output of parse_source()
    start           : root subroutine name
    call_tree_name  : basename of the call tree file (for back-links)
    """
    known = set(sub_info)
    rev   = build_reverse(sub_info, known)
    order, reachable = bfs(start, sub_info, known)

    # Which subs get a flow section (non-leaf subs in BFS order)
    has_flow = set()
    for sub in order:
        known_calls = [c for c, _ in sub_info[sub]['calls'] if c in known]
        if known_calls:
            has_flow.add(sub)

    out = []
    out.append('# SWAT+ Subroutine Flow Charts\n')
    out.append('Auto-generated by `analysis/swat_calltree.py`.')
    out.append('Each section shows the step-by-step call sequence within one subroutine.')
    out.append('Step numbers show execution order.  `[if ...]` marks conditional calls.')
    out.append("Links in each step navigate to the callee's own flow section.")
    out.append('The **↑ Call Tree** link at the top of each section returns to the'
               ' full call tree.\n')
    out.append('---\n')

    for sub in order:
        info  = sub_info[sub]
        calls = info['calls']

        if sub not in has_flow:
            continue   # leaf — skip

        parents = sorted(rev.get(sub, set()) & reachable)

        # ----- Section header ------------------------------------------------
        out.append(f'## {sub}\n')
        out.append(f'**[↑ Call Tree]({call_tree_name}#{_anchor(sub)})**\n')

        if info['description']:
            out.append(info['description'] + '\n')
        out.append(f'Source: `{info["file"]}`\n')

        # ----- <pre> flow block ----------------------------------------------
        out.append('<pre>')

        # Top box — centred subroutine name
        box_inner = f'  {sub}  '
        box_width = max(len(box_inner), 46)
        box_inner = box_inner.center(box_width)
        out.append('┌' + '─' * box_width + '┐')
        out.append('│' + box_inner + '│')
        out.append('└' + '─' * box_width + '┘')

        n_total = len(calls)
        for i, (callee, cond) in enumerate(calls, 1):
            is_last = (i == n_total)
            out.append('│')

            prefix = '└' if is_last else '├'
            step   = f'({i:02d})'
            cond_s = (f'[if {_html_escape(cond)}]  ') if cond else ''

            if callee in has_flow:
                # Link to callee's section in the same flow file
                name_s = f'<a href="#{_anchor(callee)}">{callee}</a>'
            else:
                name_s = callee   # leaf or external — plain text

            out.append(f'{prefix}─{step}─ {cond_s}{name_s}')

        # Return footer
        out.append('│')
        out.append('▼')
        if parents:
            parent_links = ', '.join(
                f'<a href="{call_tree_name}#{_anchor(p)}">{p}</a>'
                for p in parents
            )
            out.append(f'◄─ RETURN  (called from: {parent_links})')
        else:
            out.append('◄─ RETURN  (entry point)')

        out.append('</pre>')
        out.append('\n---\n')

    return '\n'.join(out)


def add_flow_links_to_call_tree(call_tree_path, flow_filename, documented_flow_subs):
    """
    Insert **Flow:** links into an existing call tree Markdown file.

    For each ## section whose normalised name is in *documented_flow_subs*,
    inserts:
        **Flow:** [<name> flow chart](<flow_filename>#<anchor>)
    after any **Called from:** line (or right after description / heading if
    there is no Called from line).

    Idempotent: skips sections that already contain a **Flow:** line.

    Parameters
    ----------
    call_tree_path       : path to doc/swatplus_call_tree.md
    flow_filename        : basename of the flow file, e.g. 'swatplus_sub_flow.md'
    documented_flow_subs : set of lowercase subroutine names with flow sections
    """
    HEADING_RE     = re.compile(r'^## (.+)$')
    FLOW_RE        = re.compile(r'^\*\*Flow:\*\*')
    CALLED_FROM_RE = re.compile(r'^\*\*Called from:\*\*')

    with open(call_tree_path) as f:
        lines = f.readlines()

    # Collect (line_index, sub_name) for matching headings
    heading_positions = []
    for i, line in enumerate(lines):
        m = HEADING_RE.match(line.strip())
        if not m:
            continue
        heading = m.group(1)
        bq = re.search(r'`(\w+)`', heading)
        if bq:
            norm = bq.group(1).lower()
        else:
            norm = heading.lower().split()[0].strip('`').strip()
        if norm in documented_flow_subs:
            heading_positions.append((i, norm))

    # Process in reverse so insertions don't shift earlier indices
    new_lines = list(lines)
    updated = 0

    for heading_idx, sub_name in reversed(heading_positions):
        pos = heading_idx + 1

        # Skip blank lines after heading
        while pos < len(new_lines) and new_lines[pos].strip() == '':
            pos += 1

        # Skip optional description paragraph
        if pos < len(new_lines):
            s = new_lines[pos].strip()
            if (s and not s.startswith('**') and not s.startswith('<pre>')
                    and not s.startswith('```') and not s.startswith('#')
                    and not s.startswith('---') and not s.startswith('|')):
                while pos < len(new_lines):
                    s2 = new_lines[pos].strip()
                    if (s2 == '' or s2.startswith('**') or s2.startswith('<pre>')
                            or s2.startswith('#') or s2.startswith('---')):
                        break
                    pos += 1
                while pos < len(new_lines) and new_lines[pos].strip() == '':
                    pos += 1

        # Skip **Called from:** line
        if pos < len(new_lines) and CALLED_FROM_RE.match(new_lines[pos].strip()):
            pos += 1
            while pos < len(new_lines) and new_lines[pos].strip() == '':
                pos += 1

        # Already has flow link — skip
        if pos < len(new_lines) and FLOW_RE.match(new_lines[pos].strip()):
            continue

        flow_line = (f'**Flow:** [{sub_name} flow chart]'
                     f'({flow_filename}#{_anchor(sub_name)})\n')
        new_lines.insert(pos, '\n')
        new_lines.insert(pos, flow_line)
        updated += 1

    with open(call_tree_path, 'w') as f:
        f.writelines(new_lines)

    print(f'  Added flow links in {updated} sections of {call_tree_path}',
          file=sys.stderr)


# ===========================================================================
# STEP 5: Verification against existing Markdown
# ===========================================================================

def _parse_doc_sections(md_file, known):
    """
    Parse an existing call-tree Markdown file.

    Returns
    -------
    doc_subs   : set of normalised subroutine names that have a ## section
    doc_children : { sub_name : set(child_names) }  — names seen in <pre> blocks
    """
    HEADING = re.compile(r'^## (.+)$')
    NAME    = re.compile(r'(?:<a[^>]*>)?([a-z_][a-z0-9_]{2,})(?:</a>)?', re.IGNORECASE)

    doc_subs     = set()
    doc_children = defaultdict(set)
    current      = None
    in_pre       = False

    with open(md_file) as fh:
        for line in fh:
            stripped = line.strip()

            m = HEADING.match(stripped)
            if m:
                heading = m.group(1)
                if heading.lower() in NON_SUB_HEADINGS:
                    current, in_pre = None, False
                    continue
                # Derive normalised name
                norm = heading.lower().split()[0].strip('`')
                bq   = re.search(r'`(\w+)`', heading)
                if bq:
                    norm = bq.group(1).lower()
                current = norm
                doc_subs.add(norm)
                in_pre = False
                continue

            if stripped == '<pre>':
                in_pre = True
                continue
            if stripped == '</pre>':
                in_pre = False
                continue

            if in_pre and current:
                for m2 in NAME.finditer(stripped):
                    tok = m2.group(1).lower()
                    if (tok in known
                            and tok != current
                            and tok not in NON_SUB_TOKENS
                            and len(tok) > 2):
                        doc_children[current].add(tok)

    return doc_subs, doc_children


def verify(sub_info, md_file):
    """
    Print a discrepancy report comparing the source call graph against *md_file*.
    """
    known  = set(sub_info)
    rev    = build_reverse(sub_info, known)
    _, reachable = bfs('main', sub_info, known)

    doc_subs, doc_children = _parse_doc_sections(md_file, known)

    true_calls = {                          # sub -> set of known callees
        s: set(c for c, _ in info['calls'] if c in known)
        for s, info in sub_info.items()
    }

    # ------------------------------------------------------------------ A
    missing_sections = [
        (s, sorted(true_calls.get(s, set())))
        for s in sorted(reachable)
        if s not in doc_subs and true_calls.get(s)
    ]

    # ------------------------------------------------------------------ B
    missing_children = [
        (s, sorted(true_calls.get(s, set()) - doc_children.get(s, set())))
        for s in sorted(doc_subs & reachable)
        if true_calls.get(s, set()) - doc_children.get(s, set())
    ]

    # ------------------------------------------------------------------ C
    phantom_children = [
        (s, sorted(doc_children.get(s, set()) - true_calls.get(s, set())))
        for s in sorted(doc_subs & reachable)
        if doc_children.get(s, set()) - true_calls.get(s, set())
    ]
    # Only report phantoms where the phantom name is a real known sub
    phantom_children = [
        (s, [p for p in kids if p in known])
        for s, kids in phantom_children
    ]
    phantom_children = [(s, kids) for s, kids in phantom_children if kids]

    # ------------------------------------------------------------------ D
    # Sections that exist in doc but have no section for any of their parents
    # (i.e., "Called from" is missing or points to undocumented parent)
    no_parent_link = [
        s for s in sorted(doc_subs & reachable)
        if rev.get(s) and not (rev[s] & doc_subs)
        and s != 'main'
    ]

    # ------------------------------------------------------------------ Report
    W = 70
    def header(title):
        print(f'\n{"=" * W}')
        print(title)
        print('=' * W)

    header('[A] Non-leaf reachable subs with NO section in doc')
    if missing_sections:
        for sub, kids in missing_sections:
            print(f'  {sub}  →  {", ".join(kids[:6])}{"..." if len(kids)>6 else ""}')
    else:
        print('  (none)')

    header('[B] Documented sections MISSING child calls (source calls sub, doc does not list it)')
    if missing_children:
        for sub, kids in missing_children:
            print(f'  {sub}:  {", ".join(kids)}')
    else:
        print('  (none)')

    header('[C] Phantom children in doc (listed but source does NOT call them)')
    if phantom_children:
        for sub, kids in phantom_children:
            print(f'  {sub}:  {", ".join(kids)}')
    else:
        print('  (none)')

    header('[D] Sections whose only callers have no doc section (back-link destination missing)')
    if no_parent_link:
        for s in no_parent_link:
            callers = sorted(rev.get(s, set()))
            print(f'  {s}  ←  called by: {", ".join(callers[:4])}')
    else:
        print('  (none)')

    header('SUMMARY')
    print(f'  Source subroutines parsed:              {len(known)}')
    print(f'  Reachable from main:                    {len(reachable)}')
    print(f'  Sections in doc:                        {len(doc_subs)}')
    print(f'  [A] Non-leaf subs missing a section:    {len(missing_sections)}')
    print(f'  [B] Sections with missing child calls:  {len(missing_children)}')
    print(f'  [C] Sections with phantom child calls:  {len(phantom_children)}')
    print(f'  [D] Subs with no documented parent:     {len(no_parent_link)}')
    print()


# ===========================================================================
# Entry point
# ===========================================================================

def main():
    ap = argparse.ArgumentParser(
        description='SWAT+ Fortran call tree — generate Markdown, flow charts, or verify',
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog=__doc__,
    )
    ap.add_argument('--src',          default='./src',
                    help='Fortran source directory (default: ./src)')
    ap.add_argument('--start',        default='main',
                    help='Root subroutine (default: main)')
    ap.add_argument('--generate',     metavar='FILE',
                    help='Write generated call tree Markdown to FILE')
    ap.add_argument('--verify',       metavar='FILE',
                    help='Verify against existing Markdown FILE')
    ap.add_argument('--flow',         metavar='FILE',
                    help='Write per-subroutine flow chart Markdown to FILE')
    ap.add_argument('--update-links', metavar='FILE', dest='update_links',
                    help='Patch an existing call tree Markdown with flow links '
                         '(use together with --flow)')
    ap.add_argument('--leaves',       action='store_true',
                    help='Include leaf-node sections in generated output')
    args = ap.parse_args()

    if not args.generate and not args.verify and not args.flow:
        ap.print_help()
        sys.exit(0)

    print(f'Parsing {args.src} ...', file=sys.stderr)
    sub_info = parse_source(args.src)
    print(f'  {len(sub_info)} subroutines found', file=sys.stderr)

    if args.verify:
        print(f'Verifying against {args.verify} ...', file=sys.stderr)
        verify(sub_info, args.verify)

    if args.generate:
        print(f'Generating call tree {args.generate} ...', file=sys.stderr)
        md = generate_markdown(sub_info, start=args.start,
                               include_leaves=args.leaves)
        with open(args.generate, 'w') as fh:
            fh.write(md)
        print(f'  Written to {args.generate}', file=sys.stderr)

    if args.flow:
        ct_name = (os.path.basename(args.update_links)
                   if args.update_links else 'swatplus_call_tree.md')

        print(f'Generating flow charts {args.flow} ...', file=sys.stderr)
        flow_md = generate_flow_markdown(sub_info, start=args.start,
                                         call_tree_name=ct_name)
        with open(args.flow, 'w') as fh:
            fh.write(flow_md)
        print(f'  Written to {args.flow}', file=sys.stderr)

        if args.update_links:
            known = set(sub_info)
            _, reachable = bfs(args.start, sub_info, known)
            has_flow = {
                sub for sub in reachable
                if any(c in known for c, _ in sub_info[sub]['calls'])
            }
            flow_basename = os.path.basename(args.flow)
            print(f'Patching {args.update_links} with flow links ...',
                  file=sys.stderr)
            add_flow_links_to_call_tree(args.update_links, flow_basename, has_flow)


if __name__ == '__main__':
    main()
