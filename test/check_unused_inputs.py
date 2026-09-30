#!/usr/bin/env python3
"""
check_unused_inputs.py -- find every variable/field that is read as input from a SWAT+
data file but never used in any executable line of code (only declared and/or populated
by a read statement).

Reuses check_inputs.py's source-parsing primitives (derived-type flattening,
module/local variable tracking, read-statement parsing) so this stays as
version-adaptive as the rest of the checker suite -- nothing about field names or file
layouts is hardcoded, only re-derived from src/ each run.

METHOD
------
1. Walk every subroutine in src/*.f90. For every list-directed `read (unit,*) ...`
   statement, split the variable list into items (expanding implied-do repeat groups).
   For each item, resolve what it actually is:
     a. a LOCAL scalar (declared inside this subroutine)        -> subroutine-scoped search
     b. a bare MODULE-level scalar (visible via `use`)           -> whole-codebase search
     c. a bare derived-type INSTANCE (e.g. `read (u,*) bsn, sp_ob`) -> expand to every
        leaf field of its type, each treated like (d)
     d. a dotted derived-type FIELD access (e.g. `hru_db(i)%dbsc%land_use_mgt`)
                                                                   -> whole-codebase search,
        keyed by the field's own name (since the same type can be accessed through many
        different local variable/loop-index names across different subroutines)
2. For (a): search the REST of the same subroutine's body (excluding its own
   declaration line and the read line itself) for another textual occurrence of the
   variable name. None found => flagged.
3. For (b)/(c)/(d): a single pass builds a codebase-wide index of every `name` and
   `%name` occurrence, tagged with whether that occurrence's line is itself a
   list-directed read statement (i.e. just another populating read, not a use). A field
   is flagged only if EVERY occurrence anywhere in src/ is on a read-statement line.

CAVEATS (read before trusting a "flagged" entry)
--------------------------------------------------
  - Field-name matching is by NAME only, not by fully type-checked reference -- if an
    unrelated derived type happens to reuse the same field name (e.g. many types have a
    `name` field), a genuine use of the OTHER type's field will make this type's field
    look "used" too. This makes the tool UNDER-report (safe direction: it will not
    falsely accuse something of being unused), never over-report a field as unused due
    to name collision.
  - An occurrence counts as "use" even if it's only ever assigned TO again elsewhere
    (not read FROM) -- this is a single-hop analysis, not full dataflow; a field that's
    only ever copied around without its value ever influencing a computation, condition,
    or output would not be caught here.
  - `titldum` / `header` (every input-reading routine's boilerplate title+header-line
    reads) are excluded on purpose -- they are unused BY CONVENTION, not by defect.
  - Local variables that are read into and then immediately consumed only inside the
    SAME read-triggering `if(eof<0)` guard, or only as a loop bound one line later, are
    correctly NOT flagged (the search covers the whole subroutine body).
  - Every finding here is a LEAD, not an automatic verdict -- spot-check anything
    surprising against the raw source (case-insensitive, continuation-line aware, since
    a Fortran `&`-continued read can split a field name's line from its `read (...)`
    across several physical lines) before acting on it.

USAGE
-----
Every path this script touches -- `src/*.f90` -- is resolved relative to `--repo`
(default: `.`, the current directory), NOT relative to where this script file lives.
There is no requirement to run it from `test/`, or from the repo root either, as long as
`--repo` points at the repo:

    cd swatplus_fg_fork && test/check_unused_inputs.py        # --repo defaults to
                                                                # "." = the repo root
    test/check_unused_inputs.py --repo /path/to/swatplus_fg_fork   # run from anywhere
    test/check_unused_inputs.py -v               # also report unresolved read items

OUTPUT AND EXIT CODE
---------------------
Every finding and the `[index]`/`[src]`/`[summary]` lines all go to stdout --
`test/check_unused_inputs.py > report.txt` captures the whole report. The only stderr
output is a single fatal setup error (no source files found under `--repo`), paired with
exit code 2. Exit code is 0 if no unused-input findings were reported, 1 if any were
(local/module scalar or derived-type field), 2 on a setup error before any scanning
could happen.
"""

import argparse
import glob
import os
import re
import sys

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import check_inputs as ci  # noqa: E402

BOILERPLATE_LOCAL_NAMES = {"titldum", "header"}


def is_bare_identifier(item):
    return bool(re.match(r'^[A-Za-z_]\w*$', item.strip()))


def find_unused_inputs(src_files, types, module_vars, verbose=False):
    """Returns (local_findings, field_findings, unresolved_count). local_findings is a
       list of (srcfile, subname, lineno, varname); field_findings is
       {fieldname: [(srcfile, subname, lineno, expr_text), ...]}."""
    # -----------------------------------------------------------------
    # Pass 1: codebase-wide "%fieldname" and bare-identifier occurrence index, so
    # global usage lookups (module vars, derived-type fields) are O(1) after this.
    # -----------------------------------------------------------------
    field_occ = {}   # fieldname(lower) -> list of (is_read_line: bool)
    bare_occ = {}    # identifier(lower) -> list of (is_read_line: bool)
    all_lines_cache = {}  # path -> logical lines (reused in pass 2)

    FIELD_TOKEN_RE = re.compile(r'%\s*([A-Za-z_]\w*)', re.IGNORECASE)
    BARE_TOKEN_RE = re.compile(r'\b([A-Za-z_]\w*)\b')

    for path in src_files:
        lines = ci.read_logical_lines(path)
        all_lines_cache[path] = lines
        for lineno, text in lines:
            s = text.strip()
            is_read = bool(ci.parse_read_statement(s))
            for m in FIELD_TOKEN_RE.finditer(text):
                fn = m.group(1).lower()
                field_occ.setdefault(fn, []).append(is_read)
            for m in BARE_TOKEN_RE.finditer(text):
                bare_occ.setdefault(m.group(1).lower(), []).append(is_read)

    def field_is_used_elsewhere(fieldname):
        occs = field_occ.get(fieldname.lower(), [])
        return any(not is_read for is_read in occs)

    def bare_is_used_elsewhere(name):
        occs = bare_occ.get(name.lower(), [])
        return any(not is_read for is_read in occs)

    # -----------------------------------------------------------------
    # Pass 2: walk every subroutine, find read-sourced identifiers, classify, check use.
    # -----------------------------------------------------------------
    local_findings = []
    field_findings = {}
    unresolved_count = 0

    for path in src_files:
        lines = all_lines_cache[path]
        n = len(lines)
        i = 0
        while i < n:
            lineno, text = lines[i]
            msub = ci.SUBROUTINE_START.match(text.strip())
            if not msub:
                i += 1
                continue
            subname = msub.group(1)
            i += 1
            used_modules = []
            local_vars = {}
            sub_body = []  # (lineno, text) excluding decl lines, for local-var search
            read_items = []  # (lineno, item_text)

            while i < n and not ci.SUBROUTINE_END.match(lines[i][1].strip()):
                l2, t2 = lines[i]
                s2 = t2.strip()
                mu = ci.USE_RE.match(s2)
                if mu:
                    used_modules.append(mu.group(1).lower())
                    i += 1
                    continue
                decl = ci.parse_declaration_line(t2)
                if decl and not ci.READ_HEAD_RE.match(s2):
                    base, names = decl
                    for nm in names:
                        local_vars[nm] = base
                    i += 1
                    continue
                sub_body.append((l2, t2))
                mr = ci.parse_read_statement(s2)
                if mr:
                    unit, is_ld, varlist = mr
                    if is_ld:
                        for item in ci.split_top_level(varlist, ','):
                            item = item.strip()
                            if not item:
                                continue
                            repeat_exprs, is_repeat = ci.split_implied_do(item)
                            if is_repeat:
                                for rexpr in repeat_exprs:
                                    read_items.append((l2, rexpr.strip()))
                            else:
                                read_items.append((l2, item))
                i += 1

            for (l2, item) in read_items:
                if is_bare_identifier(item):
                    name = item.lower()
                    if name in BOILERPLATE_LOCAL_NAMES:
                        continue
                    if name in local_vars:
                        # local scalar -- search rest of subroutine body
                        used = False
                        for (ll, tt) in sub_body:
                            if ll == l2:
                                continue
                            if re.search(r'\b' + re.escape(name) + r'\b', tt, re.IGNORECASE):
                                used = True
                                break
                        if not used:
                            local_findings.append((os.path.basename(path), subname, l2, item))
                        continue
                    # not a local decl -- try module var or bare derived-type instance
                    t = ci.resolve_expr_type(item, local_vars, used_modules, module_vars, types)
                    if t is None:
                        unresolved_count += 1
                        continue
                    if isinstance(t, tuple) and t[0] == 'derived':
                        for dotted_name, leaf in ci.flatten_type_leaf_names(types, t):
                            if dotted_name is None:
                                continue
                            final_field = dotted_name.split('%')[-1]
                            if not field_is_used_elsewhere(final_field):
                                field_findings.setdefault(final_field, []).append(
                                    (os.path.basename(path), subname, l2, f"{item}%{dotted_name}"))
                    else:
                        # bare module-level scalar
                        if not bare_is_used_elsewhere(name):
                            local_findings.append((os.path.basename(path), subname, l2,
                                                    item + "  [module-scope]"))
                else:
                    base, fields = ci.parse_dotted_expr(item)
                    if not base or not fields:
                        unresolved_count += 1
                        continue
                    final_field = fields[-1]
                    if not field_is_used_elsewhere(final_field):
                        field_findings.setdefault(final_field, []).append(
                            (os.path.basename(path), subname, l2, item))

    return local_findings, field_findings, unresolved_count


def main():
    ap = argparse.ArgumentParser(
        description="Find SWAT+ input variables/fields that are read from a data file "
                    "but never used anywhere else in the source.",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog=__doc__,
    )
    ap.add_argument("--repo", default=".", help="path to the swatplus repo root (default: .)")
    ap.add_argument("-v", "--verbose", action="store_true",
                     help="also report the count of unresolved/skipped read items")
    args = ap.parse_args()

    src_files = sorted(glob.glob(os.path.join(args.repo, "src", "*.f90")))
    if not src_files:
        print(f"no source files found under {os.path.join(args.repo, 'src', '*.f90')}",
              file=sys.stderr)
        sys.exit(2)

    try:
        import subprocess
        rev = subprocess.run(["git", "-C", args.repo, "describe", "--always", "--dirty"],
                              capture_output=True, text=True, timeout=5).stdout.strip()
    except Exception:
        rev = None
    if rev:
        print(f"[src] deriving schema from {os.path.abspath(args.repo)} @ {rev}")

    types, module_vars = ci.parse_all_types_and_module_vars(src_files)
    local_findings, field_findings, unresolved_count = find_unused_inputs(
        src_files, types, module_vars, verbose=args.verbose)

    print(f"[index] {len(src_files)} source files")
    if args.verbose:
        print(f"[index] {unresolved_count} read item(s) could not be resolved/classified "
              f"(skipped, not flagged)")

    print(f"\n=== Local/module scalars read but never used elsewhere "
          f"({len(local_findings)}) ===")
    for (srcfile, subname, lineno, varname) in local_findings:
        print(f"  {srcfile} ({subname}), line {lineno}: {varname}")

    print(f"\n=== Derived-type fields read but never used elsewhere "
          f"({len(field_findings)} distinct field name(s)) ===")
    for fieldname in sorted(field_findings):
        sites = field_findings[fieldname]
        first = sites[0]
        extra = f" (+{len(sites) - 1} more read site(s))" if len(sites) > 1 else ""
        print(f"  {fieldname}: {first[0]} ({first[1]}), line {first[2]}: {first[3]}{extra}")

    total = len(local_findings) + len(field_findings)
    print(f"\n[summary] {total} finding(s) -- {len(local_findings)} local/module "
          f"scalar(s), {len(field_findings)} derived-type field name(s)")
    if total:
        sys.exit(1)


if __name__ == "__main__":
    main()
