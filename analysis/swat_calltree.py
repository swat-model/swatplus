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


# ---------------------------------------------------------------------------
# Body annotation helpers (for enhanced flow charts)
# ---------------------------------------------------------------------------

def _format_context(block_type, text):
    """Format a structural context entry for display inside <pre> blocks."""
    text = text.strip()
    if block_type == 'do':
        return f'DO {text}' if text else 'DO loop'
    elif block_type == 'do_while':
        return f'DO WHILE ({text})' if text else 'DO WHILE loop'
    elif block_type == 'if':
        return f'IF ({text}) THEN'
    elif block_type == 'elif':
        return f'ELSE IF ({text}) THEN'
    elif block_type == 'else':
        return 'ELSE branch'
    elif block_type == 'select':
        return f'SELECT CASE ({text})'
    elif block_type == 'case':
        return f'CASE ({text})'
    else:
        return text


def _parse_call_annotations(logical_lines, sub_name):
    """
    Walk the body of *sub_name* within *logical_lines* (output of
    _join_continuations) and return per-call annotation data:

        [(callee_lower, comment_or_None, context_list), ...]

    *context_list* is a list of at most 3 human-readable strings describing
    the structural blocks (DO, IF, SELECT CASE) that enclose the call at that
    point in the source.

    *comment_or_None* is the text of the last ``!!`` comment line that
    appeared immediately before the call (resetting on any intervening
    non-comment, non-blank line).
    """
    SUB_START = re.compile(
        r'^\s*(subroutine|program)\s+' + re.escape(sub_name) + r'\b',
        re.IGNORECASE)
    SUB_END = re.compile(r'^\s*end\s*(subroutine|program)', re.IGNORECASE)

    in_sub = False
    body_pairs = []   # (log_stripped, clean_stripped)

    for log_line in logical_lines:
        log_stripped = log_line.strip()
        if not in_sub:
            if SUB_START.match(log_stripped):
                in_sub = True
            continue
        if SUB_END.match(log_stripped):
            break
        clean = _strip_comment(log_line).strip()
        body_pairs.append((log_stripped, clean))

    struct_stack = []   # list of (type_str, summary_str)
    recent_comment = None
    result = []

    for log_stripped, clean in body_pairs:
        s = clean.lower()

        # !! comment lines — capture as pending annotation
        if log_stripped.startswith('!!'):
            text = log_stripped.lstrip('!').strip()
            if text and '~' not in text and len(text) > 3:
                recent_comment = text[:100]
            continue

        # Single ! comment — ignore but do not reset pending !! comment
        if log_stripped.startswith('!'):
            continue

        # Blank line — preserve pending comment
        if not clean:
            continue

        # DO loop start
        m = re.match(r'do\s+(\w+\s*=\s*.+)', s)
        if m:
            struct_stack.append(('do', m.group(1).strip()[:45]))
            recent_comment = None
            continue
        m = re.match(r'do\s+while\s*\((.+)\)', s)
        if m:
            struct_stack.append(('do_while', m.group(1).strip()[:45]))
            recent_comment = None
            continue
        if re.match(r'do\s*$', s) or re.match(r'do\s+\d+\b', s):
            struct_stack.append(('do', ''))
            recent_comment = None
            continue

        # END DO
        if re.match(r'end\s*do\b', s):
            if struct_stack and struct_stack[-1][0] in ('do', 'do_while'):
                struct_stack.pop()
            recent_comment = None
            continue

        # SELECT CASE
        m = re.match(r'select\s+case\s*\((.+)\)', s)
        if m:
            struct_stack.append(('select', m.group(1).strip()[:30]))
            recent_comment = None
            continue

        # CASE branch
        m = re.match(r'case\s*\((.+)\)', s)
        if m:
            if struct_stack and struct_stack[-1][0] in ('select', 'case'):
                struct_stack[-1] = ('case', m.group(1).strip()[:30])
            recent_comment = None
            continue
        if re.match(r'case\s+default\b', s):
            if struct_stack and struct_stack[-1][0] in ('select', 'case'):
                struct_stack[-1] = ('case', 'default')
            recent_comment = None
            continue

        # END SELECT
        if re.match(r'end\s*select\b', s):
            if struct_stack and struct_stack[-1][0] in ('select', 'case'):
                struct_stack.pop()
            recent_comment = None
            continue

        # IF ... THEN  (block form, not inline)
        m = re.match(r'if\s*\(.+\)\s*then\b', s)
        if m and not re.search(r'\bcall\b', s):
            # Extract condition text between the outer parens
            depth = 0
            start_i = s.index('(')
            end_i = start_i
            for ci, ch in enumerate(s[start_i:], start_i):
                if ch == '(':
                    depth += 1
                elif ch == ')':
                    depth -= 1
                    if depth == 0:
                        end_i = ci
                        break
            cond = clean[start_i + 1:end_i].strip()[:50]
            struct_stack.append(('if', cond))
            recent_comment = None
            continue

        # ELSE IF ... THEN
        m = re.match(r'else\s*if\s*\(.+\)\s*then\b', s)
        if m:
            depth = 0
            start_i = s.index('(')
            end_i = start_i
            for ci, ch in enumerate(s[start_i:], start_i):
                if ch == '(':
                    depth += 1
                elif ch == ')':
                    depth -= 1
                    if depth == 0:
                        end_i = ci
                        break
            cond = clean[start_i + 1:end_i].strip()[:50]
            if struct_stack and struct_stack[-1][0] in ('if', 'elif', 'else'):
                struct_stack[-1] = ('elif', cond)
            recent_comment = None
            continue

        # ELSE
        if re.match(r'^else\s*$', s) or re.match(r'^else\s+!', s):
            if struct_stack and struct_stack[-1][0] in ('if', 'elif'):
                struct_stack[-1] = ('else', '')
            recent_comment = None
            continue

        # END IF
        if re.match(r'end\s*if\b', s) or re.match(r'endif\b', s):
            if struct_stack and struct_stack[-1][0] in ('if', 'elif', 'else'):
                struct_stack.pop()
            recent_comment = None
            continue

        # Inline conditional: if (...) call foo
        cond_inline, callee = _find_condition(clean)
        if callee and callee not in INTRINSICS:
            ctx = [_format_context(t, v) for t, v in struct_stack[:3]]
            result.append((callee.lower(), recent_comment, ctx))
            recent_comment = None
            continue

        # Direct call: call foo(...)
        m2 = re.match(r'call\s+(\w+)', s)
        if m2:
            callee = m2.group(1).lower()
            if callee not in INTRINSICS:
                ctx = [_format_context(t, v) for t, v in struct_stack[:3]]
                result.append((callee, recent_comment, ctx))
                recent_comment = None
            continue

        # Any other executable line: reset pending comment
        if clean and not clean.lower().startswith(('implicit ', 'use ', 'integer ',
                                                    'real ', 'logical ', 'character ',
                                                    'type(', 'external ', 'intrinsic ')):
            recent_comment = None

    return result


def compute_call_meta(sub_info, src_dir):
    """
    Enrich each entry in *sub_info* with ``'call_meta'``: a list of
    ``{'comment': str|None, 'context': list[str]}`` dicts, one per entry in
    ``info['calls']``, providing the ``!!`` comment immediately before each
    call and the structural context (DO/IF/SELECT) that surrounds it.

    Reads each source file once, annotating all subroutines in that file.
    """
    by_file = defaultdict(list)
    for sub_name, info in sub_info.items():
        by_file[info['file']].append(sub_name)

    for fname, subs in by_file.items():
        filepath = os.path.join(src_dir, fname)
        try:
            with open(filepath, errors='ignore') as fh:
                raw = fh.readlines()
        except OSError:
            for sub_name in subs:
                info = sub_info[sub_name]
                info['call_meta'] = [{'comment': None, 'context': []}
                                     for _ in info['calls']]
            continue

        logical = _join_continuations(raw)

        for sub_name in subs:
            info = sub_info[sub_name]

            # Get per-call annotations in source order (pre-dedup)
            all_anns = _parse_call_annotations(logical, sub_name)

            # Keep only first occurrence of each callee (matches dedup in parse_source)
            first_ann = {}
            for callee, comment, context in all_anns:
                if callee not in first_ann:
                    first_ann[callee] = (comment, context)

            # Build call_meta parallel to info['calls']
            call_meta = []
            for callee, _cond in info['calls']:
                ann = first_ann.get(callee, (None, []))
                call_meta.append({'comment': ann[0], 'context': ann[1]})

            info['call_meta'] = call_meta


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
               ' full call tree.')
    out.append('`← description` after each step summarises what the callee does.')
    out.append('`!! note` lines show key source comments preceding a call.')
    out.append('`┌─ context` lines mark entry into DO loops, IF blocks, or SELECT CASE.\n')
    out.append('---\n')

    for sub in order:
        info  = sub_info[sub]
        calls = info['calls']

        if sub not in has_flow:
            continue   # leaf — skip

        parents = sorted(rev.get(sub, set()) & reachable)
        call_meta = info.get('call_meta', [])

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
        prev_ctx_top = None   # track outermost structural context for transitions

        for i, (callee, cond) in enumerate(calls, 1):
            is_last = (i == n_total)
            meta = call_meta[i - 1] if i - 1 < len(call_meta) else {}
            context  = meta.get('context', [])
            comment  = meta.get('comment')

            out.append('│')

            # Structural context — show outermost block when it changes
            curr_ctx_top = context[0] if context else None
            if curr_ctx_top != prev_ctx_top and curr_ctx_top is not None:
                ctx_escaped = _html_escape(curr_ctx_top)
                out.append(f'│  ┌─ {ctx_escaped}')
            prev_ctx_top = curr_ctx_top

            # !! comment annotation preceding this call in source
            if comment:
                out.append(f'│  !! {_html_escape(comment[:90])}')

            prefix = '└' if is_last else '├'
            step   = f'({i:02d})'
            cond_s = (f'[if {_html_escape(cond)}]  ') if cond else ''

            if callee in has_flow:
                name_s = f'<a href="#{_anchor(callee)}">{callee}</a>'
            else:
                name_s = callee

            # Callee description from its own PURPOSE comment
            callee_desc = ''
            if callee in sub_info:
                desc = sub_info[callee].get('description', '')
                if desc:
                    desc = desc[:62] if len(desc) <= 62 else desc[:59] + '...'
                    callee_desc = f'  ← {_html_escape(desc)}'

            out.append(f'{prefix}─{step}─ {cond_s}{name_s}{callee_desc}')

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

        print(f'Computing call annotations ...', file=sys.stderr)
        compute_call_meta(sub_info, args.src)

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
