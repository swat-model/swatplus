#!/usr/bin/env python3
"""
check_inputs.py -- static + data cross-check for SWAT+ list-directed READ statements.

THE BUG CLASS THIS CATCHES
---------------------------
Fortran list-directed input (`read (unit,*) var1, var2, ...`) requires each input token's
apparent type to match the receiving variable's declared type. gfortran enforces this
strictly: the first token that doesn't match aborts the READ with a nonzero, non-EOF iostat
-- and every variable from that point on (including the offending one) silently keeps
whatever value it already had (its compile-time default, the first time the read runs).
Intel ifx is more permissive and will happily read a float-looking token into an INTEGER
variable, so data exported from a workflow that was only ever validated against ifx can
carry this defect for years without anyone noticing on that compiler -- and then silently
corrupt every run on gfortran, with no error message, ever (unless the caller happens to
check iostat for positive values, which most of this codebase's read routines do not).

This script finds every "read an input file into these variables" subroutine in `src/`,
works out what Fortran type each variable in the read list actually is (walking into
derived types, module `use` chains, and local declarations), and then -- given a dataset
directory -- reads the real input file and checks every token's apparent type against what
the source code expects. It flags the same class of defect that took real debugging time
to track down in `parameters.bsn`'s `day_lag_max` column, before you build and run.

TYPE COMPATIBILITY RULES (list-directed input)
------------------------------------------------
  - CHARACTER : any token is accepted (a bare word, quoted string, or a number -- Fortran
                will happily read "3.14" into a character variable as the literal text).
  - INTEGER   : token must be an integer literal (optional sign, digits only). A token
                with a decimal point or an exponent is REJECTED -- this is the day_lag_max
                bug's exact shape.
  - REAL      : token must be a valid real literal. Integer-looking tokens ARE accepted
                (Fortran promotes "5" to 5.0 for a real variable without complaint) as are
                decimal/exponential forms (1.5, .5, 5., 1.5E3, 1.5D3). Alphabetic
                characters other than a single E/D exponent marker are rejected. An
                integer-looking token in a REAL field is valid, not a mismatch, and never
                counted as an issue or toward the exit code -- but pass --int_to_float to
                get a per-file summary of which columns show this pattern (see OUTPUT AND
                EXIT CODE below); off by default, since printing every occurrence proved
                far too noisy on a real dataset.
  - LOGICAL   : T/F/.TRUE./.FALSE. and abbreviations, optionally preceded by a sign.

WHY --int_to_float IS WORTH RUNNING DESPITE BEING "JUST AN FYI"
-------------------------------------------------------------------
INTEGER <-> REAL compatibility is NOT symmetric, and that asymmetry is exactly what
makes an integer-looking value in a REAL field worth a second look even though it isn't
a defect by itself: gfortran silently promotes an integer-looking token ("5") into a
REAL variable (5.0) with no complaint, but the reverse -- a decimal-looking token ("5.0")
into an INTEGER variable -- is REJECTED outright (the day_lag_max bug's exact shape,
see THE BUG CLASS THIS CATCHES above). Someone who has only ever seen the REAL-accepts-
integer direction work fine can reasonably (and wrongly) conclude the relationship goes
both ways -- that it "doesn't matter" whether a column holds whole numbers or decimals --
and then be caught out the one time it's actually an INTEGER field on the receiving end.
--int_to_float doesn't flag anything wrong with the columns it lists; it exists so a
column that's ALWAYS been fed whole numbers (and so has never revealed whether the
underlying field is REAL or INTEGER from the data alone) can be checked against the
source's actual declared type before that assumption gets tested somewhere it matters.

ASSUMPTIONS / SCOPE (read this before trusting a clean report)
-----------------------------------------------------------------
  - One `read` statement is assumed to consume exactly one physical line of the data file.
    This matches every input-reading routine surveyed in this codebase (title line, header
    line, then one data row per line) but is not a Fortran guarantee in general -- a read
    whose variable list isn't satisfied by one line's tokens will spill onto the next line
    at runtime, which this tool does not model. The missing-column check below inherits
    this same blind spot: a row it flags as short could in principle be genuine multi-line
    spillover rather than a defect, though nothing surveyed in this codebase does that.
  - A `do ... read ... end do` loop is treated as "the last read statement in the loop body
    repeats for every remaining data line in the file" (the common `read peek; backspace;
    read full row` idiom is handled by keeping only the read after a `backspace`).
  - Implied-do read items, e.g. `(ob(i)%obtyp_out(k), ob(i)%frac_out(k), k=1,nout)`, are
    treated as a repeating group that consumes the rest of the line's tokens in cycles of
    the group's field count.
  - Only the file named as the argument to the `open` statement guarding the read (resolved
    via that variable's default in `input_file_module.f90`) is checked; if a dataset
    overrides a filename to something nonstandard, point --file at it directly. One
    exception: `hyd_read_connect.f90` (every `.con` connect file -- hru.con, channel.con,
    chandeg.con, aqu.con, res.con, rec.con, exco.con, dr.con, outlet.con, gwflow.con, ...)
    opens a dummy argument, not a resolvable expression, so its schema is instead cloned
    once per real filename discovered from its call sites in hyd_connect.f90 (same
    call-site scan the HRU/channel object-count check already used).
  - This is a static analyzer over a real but idiosyncratic codebase, not a general Fortran
    front end. It is deliberately conservative: subroutines/derived types it can't fully
    resolve are skipped and reported under --verbose rather than guessed at.

VERSION-ADAPTIVE, ALWAYS
------------------------
Every run re-derives the whole schema (derived types, read statements, expected column
counts) from whatever `src/` it's pointed at (`--repo`, default `.`) -- there is no cached
or pinned schema. Point it at a different checkout/branch/commit and it reflects that
source automatically. Each run prints `[src] deriving schema from <path> @ <git describe>`
so you always know which version of the model the expected types came from.

OTHER CHECKS (source- and dataset-level, beyond per-field types)
------------------------------------------------------------------
  - Unit-number reuse without a `close()`: reopening a Fortran unit that's still connected
    to an earlier file relies on the standard's implicit-close-before-reopen rule (an
    `OPEN` on an already-connected unit is executed as if a `CLOSE` with no `STATUS=` ran
    first -- this is well-defined, standard-conforming Fortran, not compiler-specific or
    undefined) rather than an explicit close. Flagged anyway as a source finding (runs
    even without a dataset_dir) because relying on it is a readability/maintainability
    smell, not a correctness bug: it hides "this reopens a DIFFERENT file" as an implicit
    side effect of a second `open()` line instead of stating it, which is easy to misread
    and easy to get wrong if a future edit ever needs to reopen the SAME file mid-routine
    (which the implicit close would silently and correctly discard on its own). Excludes
    the case where the two opens are in mutually-exclusive if/else or select-case branches
    (only one ever executes, so there's nothing to close between them).
  - Missing-column detection: if a row has fewer tokens than its schema's known hard-minimum
    fixed-field count, that's flagged as a real issue ("row has only N token(s) but the
    schema requires at least M field(s)"), reported against the first missing field. Applies
    to schema.prefix_rows (title/header lines and single-shot data rows read outside any
    loop, e.g. parameters.bsn's one big `bsn_prm` line -- always a single, unambiguous
    physical read, so its full field count IS the minimum) and to repeat_row/counted_block's
    header_row (per-object-per-line rows, e.g. hru.con's one row per HRU) -- but the minimum
    used for repeat_row/header_row is the SHORTEST of every candidate read actually observed
    merging into that schema, not necessarily the schema's full length: repeat_row is built
    by taking the FULLEST of several read-statement candidates (e.g. hyd_read_connect's
    nout==0 vs nout>0 branches, which share an identical leading field sequence and only
    differ in a trailing implied-do group), so a row between that minimum and the full
    length is a legitimate shorter branch, not a defect, and stays silent. That source-level
    minimum is then cross-checked against what the file's OWN rows actually, consistently
    provide (the dominant token count across every row/header in the file, capped at the
    schema's field count) and capped down to whichever is smaller -- a static single-
    candidate read is still only a NECESSARY guarantee, not a sufficient one: a derived
    type's field count can drift ahead of what real files ever populate (a field added to
    the type that the file-generating tool never started writing, its default just never
    overwritten -- pesticide.pes's `pl_uptake` is consistently absent from every row in
    every dataset surveyed), and without this cross-check every row of such a file would
    get flagged as "missing a column", which is noise, not a defect. Only a row shorter
    than what the rest of the file itself demonstrates is flagged. NOT applied to
    counted_block's body_row -- that schema is picked as merely the LONGEST read found
    anywhere in its loop span, with no prefix validation against shorter reads, so its true
    minimum isn't reliably known even as a starting point.
  - Extra-data-line detection: for a file whose schema is prefix_rows only (no repeat_row,
    no counted_block -- e.g. parameters.bsn: title, header, one data line, and the read
    routine unconditionally stops there, never mind what else is in the file), any
    non-blank line still left after every prefix row has been consumed is flagged --
    unlike the missing-column check above, this isn't drift-prone: every prefix-only file
    across 5 real datasets surveyed matches its schema's line count exactly, so there's no
    "some files legitimately have more" case to guard against the way pesticide.pes did.
    Reported once per file, against the first extra line, with the total extra count in
    the message (a duplicated data line -- pasted twice by accident -- is the common
    real-world cause).
  - Extra-column detection: the mirror image of missing-column detection -- a row with
    MORE tokens than the schema's fixed-field count is flagged, since anything past that
    count is never read by the code (the read statement only ever consumes that many
    fields) and is silently discarded at runtime, not an error but not intentional either.
    Applies wherever missing-column detection applies (schema.prefix_rows,
    repeat_row, counted_block's header_row), using the same source-derived-length-vs-
    dominant-observed-length cross-check as missing-column detection, just mirrored: the
    ceiling is raised to whatever length the file's own rows/headers dominantly,
    consistently reach, so a field genuinely present in (but not yet declared for) every
    row -- the flip side of pesticide.pes's drift -- doesn't get flagged on every row,
    only a row with even more than that. Never applied when the row has a trailing repeat
    group (an implied-do list): extra tokens there are legitimately consumed by the loop,
    not "extra" at all. In practice this has caught real, if easy-to-miss, defects rather
    than drift noise: fertilizer.frt/manure_om.frt description fields containing an
    unquoted embedded space (e.g. "Northern Plains_Beef_Liquid" instead of
    "Northern_Plains_Beef_Liquid") split into two tokens under list-directed I/O's normal
    whitespace-delimited rules, silently truncating the description to just "Northern" at
    runtime -- exactly the kind of silent data loss this check exists to surface.
  - Record-count validation for "header row declares N, then N body rows follow" repeating
    structures (management.sch's NUMB_OPS, soils.sol's per-soil layer count, ...): the
    header field's ACTUAL VALUE is read from the real data and checked against how many body
    records actually follow before the next header or EOF -- not just that each token has
    the right type, but that the file has as many records as the code will try to read.
    Recognized automatically by finding do-loop bounds anywhere in the codebase that
    resolve back to a named field in some file's header row (the count and the loop that
    consumes it are often in different subroutines, e.g. management.sch: one subroutine
    reads NUMB_OPS, a second one it calls loops on it). Left as "ambiguous, not modeled"
    when a file's structure is richer than one header + one body shape (e.g. lum.dtl's
    decision tables, with a conditions sub-block AND an actions sub-block per table).
  - Missing/unexpected input files, if a dataset_dir is given: `file.cio` is treated as the
    per-dataset manifest of which optional subsystems are actually switched on (most SWAT+
    features are 'null'-disabled for any given dataset, so a fixed required-file list would
    false-positive constantly) -- a referenced-but-absent file is a hard failure; a file
    present but never referenced by file.cio, never opened anywhere in the source (checked
    globally, including the `open_output_file` output-writing helper), and not shaped like
    a known output filename is flagged as possibly stray.
  - HRU/channel object-count cross-check, if a dataset_dir is given: hru.con, channel.con,
    and chandeg.con each list objects with a `props` column pointing into a companion
    properties file (hru-data.hru, channel.cha, channel-lte.cha respectively). If the
    connect file's object count, or any individual `props` reference, doesn't match what's
    actually present in the properties file, that object silently runs with a
    compile-time-default properties record -- no runtime error. Both the connect-file
    names and the `props` column position are discovered from source (hyd_read_connect.f90,
    shared by every object type); only the object-type <-> properties-file pairing is a
    fixed, documented fact about this codebase (hru_read.f90/ch_read.f90/sd_channel_read.f90
    each hardcode which file they open, so this tool does too).
  - Name-referenced cross-file check, if a dataset_dir is given: most of hru-data.hru's
    columns (and several other files' -- channel.cha's init/hyd/sed/nut columns,
    hru-lte.hru's plant column, reservoir.res's init column, ...) aren't ids, they're
    NAMES that have to exactly match a row in some other database file (hru-data.hru's
    `soil` column has to name a real row in soils.sol, `lu_mgt` a real row in
    landuse.lum, etc.). This codebase already performs exactly this lookup at runtime and
    reports failures to a warning file (unit 9001) -- this check finds every occurrence of
    that same lookup-then-warn idiom in the source (not hardcoded to hru-data.hru's seven
    columns; whatever file uses the idiom is picked up), resolves which real file each
    lookup reads from and which real file it's checked against, and verifies the actual
    referenced names in the real dataset really exist in the real target file -- catching
    the same class of bug the model would otherwise only report after a run, buried in a
    warning file nobody necessarily reads.

USAGE
-----
Every path this script touches -- `src/*.f90`, `input_file_module.f90`, `file.cio`, the
dataset directory itself -- is resolved relative to `--repo` (default: `.`, the current
directory), NOT relative to where this script file lives. There is no requirement to run
it from `test/`, or from the repo root either, as long as `--repo` points at the repo:

    cd swatplus_fg_fork && test/check_inputs.py <dataset_dir>       # --repo defaults to
                                                                     # "." = the repo root
    test/check_inputs.py --repo /path/to/swatplus_fg_fork <dataset_dir>   # run from anywhere
    test/check_inputs.py <dataset_dir> --file parameters.bsn   # check just one file
    test/check_inputs.py <dataset_dir> -v         # also show skipped/unresolved routines
    test/check_inputs.py <dataset_dir> --int_to_float   # also list REAL columns whose data
                                                          # has no decimal point/exponent
                                                          # (informational only, see OUTPUT
                                                          # AND EXIT CODE below)

`dataset_dir` is an OPTIONAL positional argument -- there is no separate "--index" flag
for this. Leaving it off entirely switches the whole run to "index mode": the script
still re-derives the full schema from `--repo`'s `src/`, prints the `[index] N source
files, N derived types, ...` summary line and the git-revision stamp, and still runs
every check that doesn't need a real dataset on disk (currently: unit-number reuse
without a `close()`) -- it just skips every check that needs actual input-file data to
compare against:

    test/check_inputs.py                       # index mode against --repo="." (the repo root)
    test/check_inputs.py --repo /path/to/swatplus_fg_fork    # index mode against another checkout

OUTPUT AND EXIT CODE
---------------------
Every finding, the `[index]`/`[src]` summary lines, and the final `[summary]` line all go
to stdout -- `test/check_inputs.py <dataset_dir> > report.txt` captures the whole report.
The only stderr output is two fatal setup errors (no source files found under `--repo`;
`--file` given a name with no resolved schema), each paired with exit code 2. Exit code
is 0 if nothing was found, 1 if any finding was reported (type mismatch, missing/
unexpected file, unclosed unit reuse, record-count mismatch, object-count mismatch, or
name-reference mismatch), 2 on a setup error before any checking could happen.
`--int_to_float` (currently: a REAL field's token has no decimal point/exponent -- valid,
not a mismatch) prints an `[int_to_float]` summary section to stdout, one line per input
file listing each affected column and how many times it showed the pattern -- neither an
issue nor a warning, never counted toward `[summary]`'s issue total, never affects the
exit code. Without the flag, a hint still appears in `[summary]` if any such column
exists ("N REAL-looks-like-integer notice(s) ... re-run with --int_to_float to list
them") so a clean run's exit code is never the only signal something is there to see.
"""

import argparse
import glob
import itertools
import os
import re
import sys
from dataclasses import dataclass, field as dc_field, replace as dc_replace

SRC_GLOB = "src/*.f90"


# ============================================================================
# Source line normalization: strip comments, join `&` continuations
# ============================================================================

def strip_comment(line):
    in_quote = None
    for i, ch in enumerate(line):
        if in_quote:
            if ch == in_quote:
                in_quote = None
        elif ch in ("'", '"'):
            in_quote = ch
        elif ch == '!':
            return line[:i]
    return line


def read_logical_lines(path):
    """Yield (start_lineno, text) per logical line: comments stripped, & continuations joined."""
    with open(path, errors="replace") as f:
        raw = f.readlines()
    out = []
    buf = []
    start = None
    cont_expected = False
    for i, line in enumerate(raw, 1):
        code = strip_comment(line).rstrip("\n").rstrip()
        if cont_expected:
            code = code.lstrip()
            if code.startswith("&"):
                code = code[1:]
        if start is None:
            start = i
        cont_expected = code.rstrip().endswith("&")
        if cont_expected:
            code = code.rstrip()[:-1].rstrip()
        buf.append(code)
        if not cont_expected:
            text = " ".join(b for b in buf if b)
            if text.strip():
                out.append((start, text))
            buf = []
            start = None
    if buf:
        text = " ".join(b for b in buf if b)
        if text.strip():
            out.append((start, text))
    return out


# ============================================================================
# Declaration parsing (shared by type components and module/local variables)
# ============================================================================

BASE_TYPE_RE = re.compile(
    r'^(real|integer|character|logical|double\s+precision|type)\b\s*(\(([^()]*(\([^()]*\))?[^()]*)\))?',
    re.IGNORECASE,
)


def split_top_level(s, sep=','):
    """Split on `sep` but not inside parens or quotes."""
    parts = []
    depth = 0
    in_quote = None
    cur = []
    for ch in s:
        if in_quote:
            cur.append(ch)
            if ch == in_quote:
                in_quote = None
            continue
        if ch in ("'", '"'):
            in_quote = ch
            cur.append(ch)
        elif ch in '([':
            depth += 1
            cur.append(ch)
        elif ch in ')]':
            depth -= 1
            cur.append(ch)
        elif ch == sep and depth == 0:
            parts.append(''.join(cur))
            cur = []
        else:
            cur.append(ch)
    parts.append(''.join(cur))
    return [p.strip() for p in parts]


def normalize_base_type(kw, paren_arg):
    kw = re.sub(r'\s+', ' ', kw.strip().lower())
    if kw == 'double precision':
        return 'real'
    if kw == 'type':
        name = (paren_arg or '').strip().lower()
        return ('derived', name)
    return kw  # real / integer / character / logical


def parse_declaration_line(text):
    """
    Parse a single (comment-stripped, continuation-joined) declaration line.
    Returns (base_type, [var_names]) or None if this isn't a declaration line.
    base_type is 'real' | 'integer' | 'character' | 'logical' | ('derived', typename)
    """
    m = BASE_TYPE_RE.match(text.strip())
    if not m:
        return None
    kw = m.group(1)
    paren_arg = m.group(3)
    base = normalize_base_type(kw, paren_arg)
    rest = text[m.end():]

    if '::' in rest:
        varlist_str = rest.split('::', 1)[1]
    else:
        # old-style "type var1, var2" with no attributes and no ::
        # only accept if rest has no other declaration-attribute keywords
        if re.search(r'\b(dimension|allocatable|parameter|pointer|intent|target|save)\b',
                      rest, re.IGNORECASE):
            return None
        varlist_str = rest

    names = []
    for item in split_top_level(varlist_str, ','):
        item = item.strip()
        if not item:
            continue
        # strip default value
        item = split_top_level(item, '=')[0].strip()
        # strip array-dimension suffix "name(dims)"
        nm = re.match(r'^([A-Za-z_]\w*)', item)
        if nm:
            names.append(nm.group(1).lower())
    if not names:
        return None
    return base, names


# ============================================================================
# Pass 1: collect derived-type definitions and module-level variable decls
# ============================================================================

TYPE_DEF_START = re.compile(r'^type\s*(?:,[^:]*)?(?:::)?\s*([A-Za-z_]\w*)\s*$', re.IGNORECASE)
TYPE_DEF_END = re.compile(r'^end\s*type\b', re.IGNORECASE)
MODULE_START = re.compile(r'^module\s+([A-Za-z_]\w*)\s*$', re.IGNORECASE)
MODULE_END = re.compile(r'^end\s*module\b', re.IGNORECASE)
CONTAINS_RE = re.compile(r'^contains\s*$', re.IGNORECASE)
SUBROUTINE_START = re.compile(r'^(?:recursive\s+)?subroutine\s+([A-Za-z_]\w*)', re.IGNORECASE)
SUBROUTINE_END = re.compile(r'^end\s*subroutine\b', re.IGNORECASE)
USE_RE = re.compile(r'^use\s+([A-Za-z_]\w*)', re.IGNORECASE)


@dataclass
class TypeDef:
    name: str
    # ordered list of (field_name, base_type)  base_type: 'real'|'integer'|'character'|'logical'|('derived',name)
    fields: list = dc_field(default_factory=list)


def parse_all_types_and_module_vars(files):
    """Returns (types: {name: TypeDef}, module_vars: {module_name: {varname: base_type}})"""
    types = {}
    module_vars = {}

    for path in files:
        lines = read_logical_lines(path)
        i = 0
        n = len(lines)
        cur_module = None
        in_contains = False
        while i < n:
            lineno, text = lines[i]
            t = text.strip()
            mm = MODULE_START.match(t)
            if mm:
                cur_module = mm.group(1).lower()
                module_vars.setdefault(cur_module, {})
                in_contains = False
                i += 1
                continue
            if MODULE_END.match(t):
                cur_module = None
                in_contains = False
                i += 1
                continue
            if CONTAINS_RE.match(t):
                in_contains = True
                i += 1
                continue

            tdm = TYPE_DEF_START.match(t)
            if tdm and not t.lower().startswith('type('):
                tname = tdm.group(1).lower()
                tdef = TypeDef(name=tname)
                i += 1
                while i < n and not TYPE_DEF_END.match(lines[i][1].strip()):
                    decl = parse_declaration_line(lines[i][1])
                    if decl:
                        base, names = decl
                        for nm in names:
                            tdef.fields.append((nm, base))
                    i += 1
                if i < n:
                    i += 1  # consume 'end type'
                types[tname] = tdef
                continue

            # module-level variable declaration (only before CONTAINS, outside a type block)
            if cur_module and not in_contains:
                decl = parse_declaration_line(text)
                if decl:
                    base, names = decl
                    for nm in names:
                        module_vars[cur_module][nm] = base

            i += 1
    return types, module_vars


def flatten_type(types, base_type, prefix_seen=None):
    """Expand a ('derived', name) type into an ordered list of leaf base types
       ('real'/'integer'/'character'/'logical'). Returns [] if unresolvable."""
    if prefix_seen is None:
        prefix_seen = set()
    if not (isinstance(base_type, tuple) and base_type[0] == 'derived'):
        return [base_type]
    tname = base_type[1]
    if tname in prefix_seen or tname not in types:
        return []
    prefix_seen = prefix_seen | {tname}
    out = []
    for fname, ftype in types[tname].fields:
        out.extend(flatten_type(types, ftype, prefix_seen))
    return out


INPUT_TYPE_INSTANCE_RE = re.compile(
    r'^type\s*\(\s*([A-Za-z_]\w*)\s*\)\s*::\s*([A-Za-z_]\w*)', re.IGNORECASE)
DEFAULT_STR_FIELD_RE = re.compile(
    r'''character\s*\(len\s*=\s*\d+\)\s*::\s*([A-Za-z_]\w*)\s*=\s*(['"])(.*?)\2''', re.IGNORECASE)


def parse_default_filenames(input_file_module_path):
    """input_file_module.f90 declares ~20 small types (input_basin, input_cli, ...), each with
       character fields defaulting to a filename, then instantiates one variable per type
       (e.g. `type (input_basin) :: in_basin`). Several DIFFERENT types reuse the same leaf
       field name with DIFFERENT defaults (e.g. `pest` defaults to "exco_pest.exc" in one type,
       "dr_pest.del" in another, "pesticide.pes" in a third) -- so resolving `in_delr%pest`
       correctly requires going through in_delr's own type, not a flat field-name lookup.
       Returns (type_defaults: {type_name: {field_name: default_str}}, var_to_type: {var_name: type_name})."""
    type_defaults = {}
    var_to_type = {}
    cur_type = None
    for _, text in read_logical_lines(input_file_module_path):
        t = text.strip()
        tdm = TYPE_DEF_START.match(t)
        if tdm and not t.lower().startswith('type('):
            cur_type = tdm.group(1).lower()
            type_defaults[cur_type] = {}
            continue
        if TYPE_DEF_END.match(t):
            cur_type = None
            continue
        if cur_type:
            m = DEFAULT_STR_FIELD_RE.search(text)
            if m:
                type_defaults[cur_type][m.group(1).lower()] = m.group(3)
            continue
        mi = INPUT_TYPE_INSTANCE_RE.match(t)
        if mi:
            var_to_type[mi.group(2).lower()] = mi.group(1).lower()
    return type_defaults, var_to_type


def resolve_default_filename(file_expr, type_defaults, var_to_type):
    """file_expr is a lowercased open(...,file=EXPR) argument, e.g. "in_delr%pest" or a quoted
       literal. Returns the resolved default filename, or None if unresolvable."""
    lit_m = re.match(r'''^(['"])(.*)\1$''', file_expr)
    if lit_m:
        return lit_m.group(2)
    if '%' not in file_expr:
        return None
    base, field = file_expr.split('%', 1)
    base, field = base.strip(), field.strip()
    tname = var_to_type.get(base)
    if tname is None:
        return None
    return type_defaults.get(tname, {}).get(field)


def parse_file_cio(path):
    """file.cio is the per-dataset manifest: each line after the title is
       `section_name  filename_or_null  filename_or_null  ...`. Returns the set of
       non-null filename tokens actually referenced by THIS dataset -- the authoritative
       answer to "which optional input files does this particular dataset need", since
       the same codebase supports many features (gwflow, salts, constituents, water
       rights, ...) that are simply switched off via 'null' slots for most datasets."""
    referenced = set()
    if not os.path.isfile(path):
        return referenced
    with open(path, errors="replace") as f:
        raw_lines = f.readlines()
    for line in raw_lines[1:]:
        tokens = tokenize_line(line)
        if not tokens:
            continue
        for tok in tokens[1:]:  # skip the leading section name
            if tok.lower() != "null":
                referenced.add(tok.lower())
    return referenced


OUTPUT_LIKE_RE = re.compile(
    r'(_yr|_mon|_day|_aa|_yr_local)\.(txt|csv)$|\.(out|fin|csv)$', re.IGNORECASE)


def is_output_like(filename):
    """Naming-convention fallback for 'this is probably output, not input' -- used only for
       files scan_all_opens() can't resolve (e.g. fort.NNNN default-unit-number files this
       tool doesn't trace to a literal name). Every output family observed in this codebase
       follows one of these daily/monthly/yearly/avann-suffixed or .out/.csv/.fin
       conventions, or the compiler-default 'fort.<unit>' name; no *input* file does."""
    return bool(OUTPUT_LIKE_RE.search(filename)) or bool(re.match(r'^fort\.\d+$', filename))


OPEN_OUTPUT_FILE_RE = re.compile(
    r'''call\s+open_output_file\s*\(\s*[^,]+,\s*(['"])(.*?)\1''', re.IGNORECASE)


def scan_all_opens(files, type_defaults, var_to_type):
    """Every open(unit, file=EXPR) in the codebase, resolved to a filename, regardless of
       whether that unit is ever read from. Read-schema filenames come from scan_subroutines
       (which only sees units WITH a read); anything opened here but absent from that set is
       opened for WRITING only -- i.e. it's a model output, and should never be flagged as an
       unexpected or missing input file. Also picks up this codebase's output-writing helper,
       `call open_output_file(unit, "name.txt", reclen)`, which most output files go through
       instead of a bare Fortran `open`."""
    out = set()
    for path in files:
        for _, text in read_logical_lines(path):
            t = text.strip()
            m = OPEN_RE.match(t)
            if m:
                fn = resolve_default_filename(m.group(2).strip().lower(), type_defaults, var_to_type)
                if fn:
                    out.add(fn.lower())
                continue
            mo = OPEN_OUTPUT_FILE_RE.search(t)
            if mo:
                out.add(mo.group(2).strip().lower())
    return out


CLI_FILENAME_RE = re.compile(r'FILENAME\s*\n\s*(\S+)', re.IGNORECASE)


def resolve_cli_indirection(referenced, dataset_dir):
    """A handful of file.cio slots (w_pcp.cli, w_tmp.cli, ...) are themselves tiny
       indirection files: two lines, 'FILENAME' then the real data filename (e.g. wth.pcp).
       Peek inside every referenced .cli file present on disk and add what it points to."""
    extra = set()
    for fn in referenced:
        if not fn.endswith('.cli'):
            continue
        path = os.path.join(dataset_dir, fn)
        if not os.path.isfile(path):
            continue
        try:
            with open(path, errors="replace") as f:
                content = f.read()
        except OSError:
            continue
        m = CLI_FILENAME_RE.search(content)
        if m:
            extra.add(m.group(1).strip().lower())
    return extra


# ============================================================================
# Pass 2: per-subroutine scan for use/decl/open/read/do-nesting
# ============================================================================

OPEN_RE = re.compile(r'^open\s*\(\s*([^,]+?)\s*,\s*file\s*=\s*([^,)]+)', re.IGNORECASE)
INQUIRE_RE = re.compile(r'^inquire\s*\(\s*file\s*=\s*([^,)]+)', re.IGNORECASE)
READ_HEAD_RE = re.compile(r'^read\s*\(', re.IGNORECASE)
DO_RE = re.compile(r'^do\b', re.IGNORECASE)
BARE_DO_RE = re.compile(r'^do\s*$', re.IGNORECASE)  # 'do' with no control clause: one-shot
                                                      # exit-based idiom, not a real repeating loop
END_DO_RE = re.compile(r'^end\s*do\b', re.IGNORECASE)
CONTINUE_RE = re.compile(r'^\d+\s+continue\s*$', re.IGNORECASE)
BACKSPACE_RE = re.compile(r'^backspace\s*\(?\s*([^,)\s]+)', re.IGNORECASE)  # 'backspace (107)' or bare 'backspace 107'
REWIND_RE = re.compile(r'^rewind\s*\(\s*([^,)]+)', re.IGNORECASE)
CLOSE_RE = re.compile(r'^close\s*\(\s*([^,)]+)', re.IGNORECASE)
IF_RE = re.compile(r'^if\b.*\bthen\s*$', re.IGNORECASE)  # block-if opener (not a one-line if)
END_IF_RE = re.compile(r'^end\s*if\b', re.IGNORECASE)
ELSE_RE = re.compile(r'^else\b', re.IGNORECASE)  # 'else', 'elseif (...) then', 'else if (...) then'
CASE_RE = re.compile(r'^case\s*[(\w]', re.IGNORECASE)  # 'case (...)' or 'case default'
SELECT_CASE_RE = re.compile(r'^select\s*case\b', re.IGNORECASE)
END_SELECT_RE = re.compile(r'^end\s*select\b', re.IGNORECASE)
DO_BOUNDS_RE = re.compile(r'^do\s+[A-Za-z_]\w*\s*=\s*(.+)$', re.IGNORECASE)
DOTTED_FIELD_RE = re.compile(
    r'^([A-Za-z_]\w*)\s*(?:\([^()]*\))?'
    r'(?:\s*%\s*[A-Za-z_]\w*(?:\([^()]*\))?)*'
    r'\s*%\s*([A-Za-z_]\w*)\s*$', re.IGNORECASE)
ASSIGN_RE = re.compile(r'^([A-Za-z_]\w*)\s*=\s*(.+)$', re.IGNORECASE)


def find_matching_paren(text, open_idx):
    depth = 0
    for i in range(open_idx, len(text)):
        if text[i] == '(':
            depth += 1
        elif text[i] == ')':
            depth -= 1
            if depth == 0:
                return i
    return -1


def parse_read_statement(text):
    """Given a logical line starting with 'read (', return (unit, is_list_directed, varlist_text)."""
    m = READ_HEAD_RE.match(text.strip())
    if not m:
        return None
    open_idx = text.index('(', m.start())
    close_idx = find_matching_paren(text, open_idx)
    if close_idx < 0:
        return None
    control = text[open_idx + 1:close_idx]
    varlist = text[close_idx + 1:].strip()
    parts = split_top_level(control, ',')
    if len(parts) < 2:
        return None
    unit = parts[0].strip().lower()
    is_list_directed = parts[1].strip() == '*'
    return unit, is_list_directed, varlist


def branches_mutually_exclusive(sig1, sig2):
    """sig1/sig2 are branch_sig() snapshots: tuples of (construct_id, branch_index) for every
       if/select-case construct active when each event was recorded. If both snapshots agree
       on branch_index at every construct they share, one event is nested inside (or
       sequential with, at the same level as) the other -- not exclusive. If they share a
       construct but recorded a DIFFERENT branch_index for it, that construct only ever
       executes one of those branches at runtime -- the events are mutually exclusive."""
    for (id1, b1), (id2, b2) in zip(sig1, sig2):
        if id1 == id2 and b1 != b2:
            return True
        if id1 != id2:
            break
    return False


def split_implied_do(item):
    """If item is an implied-do group '(expr, expr, ivar = a, b[, c])', return
       (repeat_field_exprs, True); else (None, False)."""
    item = item.strip()
    if not (item.startswith('(') and item.endswith(')')):
        return None, False
    inner = item[1:-1]
    pieces = split_top_level(inner, ',')
    # find the loop-control piece: the first piece (scanning from the end) containing a bare '='
    ctrl_idx = None
    for idx in range(len(pieces) - 1, -1, -1):
        if re.match(r'^\s*[A-Za-z_]\w*\s*=\s*[^=]', pieces[idx]) and '==' not in pieces[idx]:
            ctrl_idx = idx
            break
    if ctrl_idx is None or ctrl_idx == 0:
        return None, False
    return pieces[:ctrl_idx], True


def resolve_expr_type(expr, local_vars, used_modules, module_vars, types):
    expr = expr.strip()
    # drop a leading repeat-count / plain numeric literal or string literal used as a read target (shouldn't happen)
    segments = expr.split('%')
    clean_segments = []
    for seg in segments:
        seg = seg.strip()
        seg = re.sub(r'\([^()]*(\([^()]*\))?[^()]*\)', '', seg).strip()
        nm = re.match(r'^([A-Za-z_]\w*)', seg)
        if not nm:
            return None
        clean_segments.append(nm.group(1).lower())

    base = clean_segments[0]
    base_type = local_vars.get(base)
    if base_type is None:
        for mod in used_modules:
            if base in module_vars.get(mod, {}):
                base_type = module_vars[mod][base]
                break
    if base_type is None:
        return None

    cur = base_type
    for seg in clean_segments[1:]:
        if not (isinstance(cur, tuple) and cur[0] == 'derived'):
            return None
        tname = cur[1]
        if tname not in types:
            return None
        fmap = {}
        for fname, ftype in types[tname].fields:
            fmap.setdefault(fname, ftype)
        if seg not in fmap:
            return None
        cur = fmap[seg]
    return cur


def resolve_varlist(varlist_text, local_vars, used_modules, module_vars, types):
    """Returns (flat_field_types, ok) where flat_field_types is a list of
       'real'/'integer'/'character'/'logical'/'repeat', or (None, False) if unresolvable."""
    items = split_top_level(varlist_text, ',')
    flat = []
    for item in items:
        item = item.strip()
        if not item:
            continue
        repeat_exprs, is_repeat = split_implied_do(item)
        if is_repeat:
            group = []
            for rexpr in repeat_exprs:
                t = resolve_expr_type(rexpr, local_vars, used_modules, module_vars, types)
                if t is None:
                    return None, False
                leaves = flatten_type(types, t) if isinstance(t, tuple) else [t]
                if not leaves:
                    return None, False
                group.extend(leaves)
            flat.append(('repeat', group))
            continue
        t = resolve_expr_type(item, local_vars, used_modules, module_vars, types)
        if t is None:
            return None, False
        leaves = flatten_type(types, t) if isinstance(t, tuple) else [t]
        if not leaves:
            return None, False
        flat.extend(leaves)
    return flat, True


def find_count_field_position(varlist_text, target_array, target_field,
                               local_vars, used_modules, module_vars, types):
    """Return the 0-based flattened-field offset of `target_array(...)%...%target_field`
       within varlist_text's items, or None if it isn't there. Locates a count field's
       position inside an already-resolved header row so its actual runtime value can be
       read from real data and used as a repeating body's line count during validation."""
    items = split_top_level(varlist_text, ',')
    offset = 0
    for item in items:
        item = item.strip()
        if not item:
            continue
        _, is_repeat = split_implied_do(item)
        if is_repeat:
            return None  # count fields are simple scalars; can't see past a variable-length group
        dm = DOTTED_FIELD_RE.match(item)
        if dm and dm.group(1).lower() == target_array and dm.group(2).lower() == target_field:
            return offset
        t = resolve_expr_type(item, local_vars, used_modules, module_vars, types)
        if t is None:
            return None
        leaves = flatten_type(types, t) if isinstance(t, tuple) else [t]
        offset += len(leaves)
    return None


@dataclass
class ReadEvent:
    lineno: int
    unit: str
    loop_depth: int
    fields: list  # resolved flat field list, or None if unresolved
    n_leaf: int


@dataclass
class SubSchema:
    subname: str
    srcfile: str
    unit: str
    file_expr: str
    default_filename: str
    prefix_rows: list       # list of field-lists (each a fixed single line), in order
    repeat_row: list        # field-list that repeats for all remaining lines, or None
    unresolved_reason: str  # None if fully resolved
    guarded: bool = False   # True if an `inquire(file=X, exist=...)` precedes the open
    counted_block: dict = None  # {'header_field_index': int, 'body_row': flat-type-list} if
                                 # this file is a repeating "header row declares N, N body rows
                                 # follow" structure (management.sch, soils.sol, ...) instead of
                                 # a flat single-shape repeat_row
    repeat_row_min_fixed: int = None  # smallest fixed-field count seen across every candidate
                                 # read merged into repeat_row (see the all_prefixes merge in
                                 # scan_subroutines) -- every branch reads at LEAST this many
                                 # fields, so it's a safe hard minimum for missing-column
                                 # detection even though repeat_row itself may be longer
                                 # (the fullest candidate, with trailing fields some branches
                                 # legitimately omit, e.g. hyd_read_connect's nout==0 case)


def scan_count_registry(files, types, module_vars):
    """First pass: find every do-loop whose bound resolves -- directly, or through one simple
       local scalar assignment just above it -- to a derived-type field, e.g. `do j = 1, mlyr`
       where `mlyr = soildb(isol)%s%nly` was assigned just above, or the more direct `do iop =
       1, sched(isched)%num_ops`. Returns {(array_name, field_name): flat_type_list} for
       whatever read is found inside that loop's span (richest-resolved wins, same heuristic
       as the main scan) -- the "body row" of what the main pass will recognize as a
       count-prefixed repeating block once it finds (array_name, field_name) as a named field
       in some file's header row. Deliberately keyed by name only, not by subroutine: SWAT+
       often reads the count in one subroutine and consumes it in a `call`ed one still using
       the same open unit (management.sch: mgt_read_mgtops.f90 reads NUMB_OPS, read_mgtops.f90
       -- a separate file -- loops `do iop = 1, sched(isched)%num_ops`), so this has to see the
       whole codebase, not just one subroutine, to make the connection."""
    registry = {}  # (array,field) -> (n_leaf, flat_type_list)
    for path in files:
        lines = read_logical_lines(path)
        n = len(lines)
        i = 0
        while i < n:
            lineno, text = lines[i]
            msub = SUBROUTINE_START.match(text.strip())
            if not msub:
                i += 1
                continue
            i += 1
            used_modules = []
            local_vars = {}
            local_assignments = {}  # varname -> (array, field)
            do_stack = []  # each: {'start': lineno, 'bound': (array,field) or None, 'is_real': bool}
            count_loop_spans = []  # (array, field, start_line, end_line)
            read_texts = []  # (lineno, varlist_text)
            while i < n and not SUBROUTINE_END.match(lines[i][1].strip()):
                l2, t2 = lines[i]
                s2 = t2.strip()
                mu = USE_RE.match(s2)
                if mu:
                    used_modules.append(mu.group(1).lower())
                    i += 1
                    continue
                decl = parse_declaration_line(t2)
                if decl and not READ_HEAD_RE.match(s2):
                    base, names = decl
                    for nm in names:
                        local_vars[nm] = base
                    i += 1
                    continue
                if (OPEN_RE.match(s2) or INQUIRE_RE.match(s2) or SELECT_CASE_RE.match(s2)
                        or END_SELECT_RE.match(s2) or CASE_RE.match(s2) or IF_RE.match(s2)
                        or END_IF_RE.match(s2) or ELSE_RE.match(s2) or REWIND_RE.match(s2)
                        or BACKSPACE_RE.match(s2) or CLOSE_RE.match(s2)):
                    i += 1
                    continue
                if DO_RE.match(s2):
                    is_bare = bool(BARE_DO_RE.match(s2))
                    bound = None
                    if not is_bare:
                        mdo = DO_BOUNDS_RE.match(s2)
                        if mdo:
                            parts = split_top_level(mdo.group(1), ',')
                            if len(parts) > 1:
                                end_expr = parts[1].strip().lower()
                                dm = DOTTED_FIELD_RE.match(end_expr)
                                if dm:
                                    bound = (dm.group(1).lower(), dm.group(2).lower())
                                elif end_expr in local_assignments:
                                    bound = local_assignments[end_expr]
                    do_stack.append({'start': l2, 'bound': bound, 'is_real': not is_bare})
                    i += 1
                    continue
                if END_DO_RE.match(s2) or CONTINUE_RE.match(s2):
                    if do_stack:
                        entry = do_stack.pop()
                        if entry['is_real'] and entry['bound'] is not None:
                            count_loop_spans.append((*entry['bound'], entry['start'], l2))
                    i += 1
                    continue
                mr = parse_read_statement(s2)
                if mr:
                    unit, is_ld, varlist = mr
                    if is_ld:
                        read_texts.append((l2, varlist))
                    i += 1
                    continue
                ma = ASSIGN_RE.match(s2)
                if ma and '==' not in s2 and '/=' not in s2 and '<=' not in s2 and '>=' not in s2:
                    dm = DOTTED_FIELD_RE.match(ma.group(2).strip())
                    if dm:
                        local_assignments[ma.group(1).lower()] = (dm.group(1).lower(), dm.group(2).lower())
                    i += 1
                    continue
                i += 1

            for (array, field, start, end) in count_loop_spans:
                best = None
                for (rl, rv) in read_texts:
                    if not (start < rl < end):
                        continue
                    flat, ok = resolve_varlist(rv, local_vars, used_modules, module_vars, types)
                    if not ok:
                        continue
                    n_leaf = sum(len(x[1]) if isinstance(x, tuple) else 1 for x in flat)
                    if best is None or n_leaf > best[0]:
                        best = (n_leaf, flat)
                if best is not None:
                    key = (array, field)
                    if key not in registry or best[0] > registry[key][0]:
                        registry[key] = best
    return {k: v[1] for k, v in registry.items()}


def scan_subroutines(files, types, module_vars, type_defaults, var_to_type, count_registry=None):
    count_registry = count_registry or {}
    schemas = []
    skipped = []
    unclosed_reuse = []  # (srcfile, subname, unit, first_open_line, reopen_line, first_file, second_file)
    for path in files:
        lines = read_logical_lines(path)
        n = len(lines)
        i = 0
        while i < n:
            lineno, text = lines[i]
            msub = SUBROUTINE_START.match(text.strip())
            if not msub:
                i += 1
                continue
            subname = msub.group(1)
            sub_start = i
            i += 1
            used_modules = []
            local_vars = {}
            opens = []   # (lineno, unit, file_expr) -- a subroutine may reopen the SAME unit
                         # number for a DIFFERENT file later on (open/read/close, then open a
                         # second file on unit 107 again) -- never overwrite, or the earlier
                         # file's read schema silently vanishes.
            guarded_exprs = set()  # file_expr strings seen in an 'inquire(file=X,exist=...)'
            read_events = []  # (lineno, unit, loop_depth, branch_sig, varlist_text)
            backspaces = []  # (lineno, unit)
            rewinds = []  # (lineno, unit)
            closes = []  # (lineno, unit)
            do_stack = []  # True = real repeating loop, False = bare one-shot 'do'
            branch_stack = []  # nested if/elseif/else and select-case/case tracking: each
                                # entry is a mutable [construct_uid, branch_index], bumped in
                                # place at elseif/else/case, so two events recorded while the
                                # SAME entry held DIFFERENT branch_index values are provably
                                # mutually exclusive at runtime (only one branch ever executes)
                                # -- used both to avoid backspace-merging reads across
                                # unrelated select-case branches, and to avoid flagging a
                                # reused-unit-without-close finding on an if/else with one
                                # open() per branch.

            branch_uid_counter = itertools.count()

            def branch_sig():
                return tuple((id(e), e[1]) for e in branch_stack)
            while i < n and not SUBROUTINE_END.match(lines[i][1].strip()):
                l2, t2 = lines[i]
                s2 = t2.strip()
                mu = USE_RE.match(s2)
                if mu:
                    used_modules.append(mu.group(1).lower())
                    i += 1
                    continue
                decl = parse_declaration_line(t2)
                if decl and not READ_HEAD_RE.match(s2):
                    base, names = decl
                    for nm in names:
                        local_vars[nm] = base
                    i += 1
                    continue
                mi = INQUIRE_RE.match(s2)
                if mi:
                    guarded_exprs.add(mi.group(1).strip().lower())
                    i += 1
                    continue
                mo = OPEN_RE.match(s2)
                if mo:
                    unit = mo.group(1).strip().lower()
                    file_expr = mo.group(2).strip().lower()
                    opens.append((l2, unit, file_expr, branch_sig()))
                    i += 1
                    continue
                if SELECT_CASE_RE.match(s2):
                    branch_stack.append([next(branch_uid_counter), 0])
                    i += 1
                    continue
                if END_SELECT_RE.match(s2):
                    if branch_stack:
                        branch_stack.pop()
                    i += 1
                    continue
                if IF_RE.match(s2):
                    branch_stack.append([next(branch_uid_counter), 0])
                    i += 1
                    continue
                if END_IF_RE.match(s2):
                    if branch_stack:
                        branch_stack.pop()
                    i += 1
                    continue
                if ELSE_RE.match(s2):
                    if branch_stack:
                        branch_stack[-1][1] += 1
                    i += 1
                    continue
                if DO_RE.match(s2):
                    do_stack.append(not BARE_DO_RE.match(s2))
                    i += 1
                    continue
                if END_DO_RE.match(s2) or CONTINUE_RE.match(s2):
                    if do_stack:
                        do_stack.pop()
                    i += 1
                    continue
                mw = REWIND_RE.match(s2)
                if mw:
                    rewinds.append((l2, mw.group(1).strip().lower()))
                    i += 1
                    continue
                mb = BACKSPACE_RE.match(s2)
                if mb:
                    backspaces.append((l2, mb.group(1).strip().lower()))
                    i += 1
                    continue
                mc = CLOSE_RE.match(s2)
                if mc:
                    closes.append((l2, mc.group(1).strip().lower()))
                    i += 1
                    continue
                if CASE_RE.match(s2):
                    if branch_stack:
                        branch_stack[-1][1] += 1
                    i += 1
                    continue
                mr = parse_read_statement(s2)
                if mr:
                    unit, is_ld, varlist = mr
                    if is_ld:
                        do_depth = sum(1 for x in do_stack if x)
                        read_events.append((l2, unit, do_depth, branch_sig(), varlist))
                    i += 1
                    continue
                i += 1

            # Check: reusing a unit number for a second open without closing it first.
            # Reopening a connected unit relies on Fortran's implicit-close-before-reopen
            # rule (well-defined, standard-conforming since FORTRAN 77 -- NOT undefined or
            # compiler-dependent) rather than an explicit close -- still worth flagging as
            # a readability/maintainability smell: it hides "this reopens a DIFFERENT file"
            # as a side effect of a second open() rather than stating it. Skip pairs that
            # are in mutually-exclusive if/else or select-case branches (e.g. "if (x)
            # open(u,A) else open(u,B)") -- only one of those ever executes, so there's
            # nothing to close between them.
            for idx in range(len(opens) - 1):
                l1, u1, f1, sig1 = opens[idx]
                for l2b, u2, f2, sig2 in opens[idx + 1:]:
                    if u2 != u1:
                        continue
                    if branches_mutually_exclusive(sig1, sig2):
                        break
                    has_close = any(cl == u1 and l1 < cln < l2b for (cln, cl) in closes)
                    if not has_close:
                        unclosed_reuse.append((os.path.basename(path), subname, u1,
                                                l1, l2b, f1, f2))
                    break  # only check against the NEXT same-unit open, not every later one

            # build a schema per open() call that has read events in its window -- a unit
            # number can be reopened for a different file later in the same subroutine, so
            # each open's associated reads/rewinds/backspaces are only those between it and
            # the NEXT open of the same unit (or end of subroutine).
            for open_idx, (open_lineno, unit, file_expr, _open_sig) in enumerate(opens):
                window_end = min(
                    (l for (l, u, _, _) in opens[open_idx + 1:] if u == unit),
                    default=float('inf'))
                default_fn = resolve_default_filename(file_expr, type_defaults, var_to_type)
                my_reads = [(l, u, d, g, v) for (l, u, d, g, v) in read_events
                            if u == unit and open_lineno < l < window_end]
                # a rewind resets the file position: only what's read AFTER the LAST rewind
                # for this unit determines the real, final on-disk read sequence -- anything
                # before it was a throwaway counting/sizing pre-pass.
                cutoff = max((l for (l, u) in rewinds if u == unit and open_lineno < l < window_end),
                             default=None)
                if cutoff is not None:
                    my_reads = [r for r in my_reads if r[0] > cutoff]
                    my_backspaces = [b for (b, u) in backspaces
                                      if u == unit and cutoff < b < window_end]
                else:
                    my_backspaces = [b for (b, u) in backspaces
                                      if u == unit and open_lineno < b < window_end]
                if not my_reads:
                    continue

                # Sequential row-slot builder: a 'backspace' between two consecutive reads of
                # the SAME unit AND the SAME select-case branch means the later read re-reads
                # the SAME physical line as the one right before it (the common "peek id,
                # backspace, read full row" idiom, used at both prefix depth and inside loops)
                # -- collapse that pair into one slot, keeping the later (fuller) read, instead
                # of counting it as two lines. Reads in DIFFERENT (mutually-exclusive) case
                # branches must never merge this way even if a backspace sits between them
                # textually, since at runtime only one branch's read ever executes per pass.
                slots = []  # each: {'depth': d, 'lineno': l, 'varlist': v}
                for (l, u, d, g, v) in my_reads:
                    superseded = False
                    if slots:
                        prev = slots[-1]
                        if (prev['depth'] == d
                                and not branches_mutually_exclusive(prev['branch_sig'], g)
                                and any(prev['lineno'] < b < l for b in my_backspaces)):
                            superseded = True
                    if superseded:
                        slots[-1] = {'depth': d, 'lineno': l, 'varlist': v, 'branch_sig': g}
                    else:
                        slots.append({'depth': d, 'lineno': l, 'varlist': v, 'branch_sig': g})

                prefix = []
                loop_candidates = []  # (depth, n_leaf, flat, varlist_text)
                unresolved = None
                for slot in slots:
                    l, d, v = slot['lineno'], slot['depth'], slot['varlist']
                    flat, ok = resolve_varlist(v, local_vars, used_modules, module_vars, types)
                    if d == 0:
                        if ok:
                            prefix.append(flat)
                        else:
                            unresolved = f"line {l}: could not resolve prefix read '{v[:60]}'"
                    else:
                        if ok:
                            n_leaf = sum(len(x[1]) if isinstance(x, tuple) else 1 for x in flat)
                            loop_candidates.append((d, n_leaf, flat, v))
                        else:
                            unresolved = f"line {l}: could not resolve loop-body read '{v[:60]}'"
                repeat_row = None
                repeat_row_min_fixed = None
                counted_block = None
                if loop_candidates:
                    # Counted-block check FIRST, on the shallowest-depth candidate(s) -- the
                    # natural "header row" of a repeating header+N-body-rows structure
                    # (management.sch's NUMB_OPS, soils.sol's per-soil layer count, ...). If its
                    # varlist contains a field this codebase somewhere uses as a do-loop bound
                    # (count_registry, built in the first pass over the WHOLE codebase), this is
                    # that pattern, not genuine multi-shape ambiguity -- and the real body-row
                    # shape comes from wherever that loop actually lives, same subroutine or not.
                    # A header row can legitimately contain more than one count field (e.g.
                    # management.sch's row has both NUMB_AUTO, governing a small optional
                    # auto-mgt sub-block, and NUMB_OPS, governing the actual operation rows
                    # that make up the bulk of the file) -- collect every match across the
                    # whole registry and prefer the one with the richer body row, since that's
                    # the file's dominant repeating structure, not an incidental side count.
                    shallowest_depth = min(d for d, _, _, _ in loop_candidates)
                    shallowest_candidates = [c for c in loop_candidates if c[0] == shallowest_depth]
                    # Require the count-row to be the ONLY read at its depth: a decision-table
                    # style file (lum.dtl: header-text, count-row, MORE header-text, a
                    # conditions sub-loop, yet more header-text, an actions sub-loop -- all
                    # texturally at the same nesting depth) has several same-depth candidates,
                    # and this tool's single-header/single-body model can't safely represent
                    # that multi-segment structure. Bail to the ambiguous path rather than
                    # guess at only one of its several counted sub-blocks.
                    best_match = None  # (len(body_row), flat, pos, body_row)
                    if len(shallowest_candidates) == 1:
                        d, n_leaf, flat, v = shallowest_candidates[0]
                        for (carray, cfield), body_row in count_registry.items():
                            pos = find_count_field_position(
                                v, carray, cfield, local_vars, used_modules, module_vars, types)
                            if pos is not None and (best_match is None or len(body_row) > best_match[0]):
                                best_match = (len(body_row), flat, pos, body_row)
                    if best_match is not None:
                        _, header_flat, pos, body_row = best_match
                        counted_block = {'header_row': header_flat, 'header_field_index': pos,
                                          'body_row': body_row}
                        unresolved = None

                    if counted_block is None:
                        distinct_depths = {d for d, _, _, _ in loop_candidates}
                        if len(distinct_depths) > 1:
                            # genuinely different repeating row shapes at different loop-nesting
                            # levels that AREN'T a recognized counted-block -- this tool models a
                            # single repeating row shape, so refuse to guess rather than silently
                            # validate against the wrong shape.
                            unresolved = (unresolved or "") + (
                                " [ambiguous: multiple distinct repeating row shapes at loop "
                                f"depths {sorted(distinct_depths)} -- not modeled, skipping]")
                        else:
                            # same-depth alternatives not linked by backspace are either code-branch
                            # variants of ONE logical row (an if/else on whether an optional trailing
                            # group is present -- e.g. hyd_read_connect's nout==0 vs nout>0 branches,
                            # which share an identical leading field sequence) or genuinely DIFFERENT
                            # rows keyed by a leading keyword (e.g. carb_coefs.cbn's `select case
                            # (var_name)` reads, where a 2-field branch and a 5-field branch share
                            # nothing but the leading character keyword). Only the first case is safe
                            # to resolve by taking the fullest candidate as a superset; distinguish
                            # them by checking every shorter candidate is a true positional prefix of
                            # the longest one's fixed field types.
                            loop_candidates.sort(key=lambda x: -x[1])
                            winner_fixed, winner_repeat = expand_row_schema(loop_candidates[0][2])
                            all_prefixes = True
                            min_fixed_len = len(winner_fixed)
                            for _, _, cand, _ in loop_candidates[1:]:
                                cand_fixed, cand_repeat = expand_row_schema(cand)
                                if cand_fixed != winner_fixed[:len(cand_fixed)]:
                                    all_prefixes = False
                                    break
                                min_fixed_len = min(min_fixed_len, len(cand_fixed))
                            if all_prefixes:
                                repeat_row = loop_candidates[0][2]
                                repeat_row_min_fixed = min_fixed_len
                            else:
                                unresolved = (unresolved or "") + (
                                    " [ambiguous: multiple same-depth repeating reads whose field "
                                    "types don't share a common prefix (likely a keyword/select-case "
                                    "read, not positional rows) -- not modeled, skipping]")

                if default_fn is None:
                    unresolved = (unresolved or "") + f" [unknown default filename for '{file_expr}']"

                schemas.append(SubSchema(
                    subname=subname, srcfile=os.path.basename(path), unit=unit,
                    file_expr=file_expr, default_filename=default_fn,
                    prefix_rows=prefix, repeat_row=repeat_row,
                    unresolved_reason=unresolved,
                    guarded=file_expr in guarded_exprs,
                    counted_block=counted_block,
                    repeat_row_min_fixed=repeat_row_min_fixed,
                ))
            i = max(i, sub_start + 1)
        # advance past this subroutine's END SUBROUTINE line
    return schemas, unclosed_reuse



# ============================================================================
# Pass 3: tokenize a real data file and check types
# ============================================================================

INT_RE = re.compile(r'^[+-]?\d+$')
REAL_RE = re.compile(r'^[+-]?(\d+\.\d*|\.\d+|\d+)([eEdD][+-]?\d+)?$')
LOGICAL_RE = re.compile(r'^\.?(true|false|t|f)\.?$', re.IGNORECASE)

TOKEN_RE = re.compile(r'''"[^"]*"|'[^']*'|[^\s,]+''')
# Stray control bytes (NULs etc., seen at EOF in some shipped files -- a common artifact
# of how some tools pad/truncate fixed-size output) aren't whitespace to \s, so a line
# made up entirely of them tokenizes as one non-empty "word" instead of nothing, defeating
# the blank-line filter that's supposed to treat degenerate trailing bytes as blank (see
# check_dataset_file). \t and \r are left alone -- already \s, and legitimate as line
# content (a trailing \r from CRLF line endings, in particular).
CONTROL_BYTES_RE = re.compile(r'[\x00-\x08\x0b\x0c\x0e-\x1f\x7f]')


def tokenize_line(line):
    return TOKEN_RE.findall(CONTROL_BYTES_RE.sub('', line))


def type_ok(expected, token):
    if expected == 'character':
        return True
    if expected == 'integer':
        return bool(INT_RE.match(token))
    if expected == 'real':
        return bool(REAL_RE.match(token)) or bool(INT_RE.match(token))
    if expected == 'logical':
        return bool(LOGICAL_RE.match(token))
    return True


REAL_LOOKS_LIKE_INT_MSG = (
    "looks like an integer (no decimal point/exponent) but the field is REAL in the "
    "source -- valid, Fortran will read it in as a floating point value; not an error"
)


def why_not(expected, token):
    if expected == 'integer' and REAL_RE.match(token) and not INT_RE.match(token):
        return "has a decimal point/exponent but the field is INTEGER"
    if any(c.isalpha() for c in token) and expected in ('integer', 'real'):
        return f"contains an alphabetic character but the field is {expected.upper()}"
    return f"doesn't parse as {expected.upper()}"


def dominant_capped_length(lengths):
    """Most common value in `lengths` (each a repeating row's token count, capped at the
       schema's own fixed-field count). Used as an empirical, data-driven cross-check on
       top of the schema's own declared minimum, since a derived type's field count can
       drift from what real files actually, consistently provide -- a trailing field added
       to the type that the file-generating tool never started populating, its default
       just never overwritten, is not a genuine per-row defect (pesticide.pes's pl_uptake:
       every row in every dataset surveyed is consistently one field short of pestdb's 16,
       and flagging all of them as "missing a column" would just be noise on top of the
       type mismatch this shortfall already causes one field earlier, description text
       landing in a REAL field). Ties -- unavoidable with only a couple of rows -- go to
       the LARGER length, so a lone short row among few samples still reads as the
       anomaly rather than as equally "normal"."""
    counts = {}
    for l in lengths:
        counts[l] = counts.get(l, 0) + 1
    best_count = max(counts.values())
    return max(l for l, c in counts.items() if c == best_count)


def expand_row_schema(row):
    """Row schema items may include ('repeat', [types]); this just returns the
       fixed-length prefix part and the trailing repeat group (or None)."""
    fixed = []
    repeat_group = None
    for item in row:
        if isinstance(item, tuple) and item[0] == 'repeat':
            repeat_group = item[1]
        else:
            fixed.append(item)
    return fixed, repeat_group


def check_row(fields, dataset_path, filename, lineno, raw_line, min_fields=None, max_fields=None):
    """fields: flat schema for this line. Returns (issues, notices) -- issues are real
       type mismatches; notices are informational-only observations that are valid
       Fortran and not a defect (currently: a REAL field whose token has no decimal
       point or exponent, e.g. '5' instead of '5.0' -- Fortran promotes it to 5.0
       without complaint, so this is neither an error nor a warning, just an FYI that
       the value will be read in as floating point).

       min_fields: if given, a row with fewer than this many tokens is flagged as a
       missing-column issue. None (the default) disables the check entirely -- safest
       for row shapes whose true minimum length isn't reliably known (counted_block's
       body_row, picked as merely the LONGEST of several reads found in a loop span with
       no prefix validation between them). For schema.prefix_rows, callers pass the row's
       own full fixed-field count (a single, unambiguous physical read has no legitimate
       reason to be shorter). For repeat_row, callers pass
       schema.repeat_row_min_fixed -- the shortest fixed-field count seen across every
       candidate read merged into that schema (e.g. hyd_read_connect's nout==0 vs
       nout>0 branches, which both read the same leading fields and only differ in a
       trailing implied-do group) -- so a row between that minimum and the full schema
       length is a legitimate shorter branch and stays silent, same as before, while
       anything short of the minimum can't be explained by any known code path.

       max_fields: if given AND the row has no trailing repeat group, a row with MORE
       tokens than this is flagged as an extra-column issue -- those tokens are never
       read by the code (the read statement only consumes `fixed`'s field count, full
       stop), so they're either a stray/duplicated column or leftover data from an
       edit, either way silently discarded at runtime. Never applied when the schema
       has a repeat group (extra trailing tokens there are legitimately consumed by
       the implied-do loop, not "extra" at all) -- callers are responsible for passing
       None in that case, same convention as min_fields."""
    issues = []
    notices = []
    if len(fields) == 1 and fields[0] == 'character':
        return issues, notices  # whole-line title/header read; always valid
    fixed, repeat_group = expand_row_schema(fields)
    tokens = tokenize_line(raw_line)
    if min_fields is not None and len(tokens) < min_fields:
        idx0 = len(tokens)
        issues.append(dict(
            file=filename, line=lineno, field_index=idx0 + 1,
            expected=fixed[idx0] if idx0 < len(fixed) else fixed[-1], token="(missing)",
            reason=(f"row has only {len(tokens)} token(s) but the schema requires at "
                    f"least {min_fields} field(s) here -- missing column(s)?")))
    if max_fields is not None and repeat_group is None and len(tokens) > max_fields:
        issues.append(dict(
            file=filename, line=lineno, field_index=max_fields + 1,
            expected="end of row", token=f"{len(tokens) - max_fields} extra token(s)",
            reason=(f"row has {len(tokens)} token(s) but the schema only reads "
                    f"{max_fields} field(s) here -- extra column(s) that won't be "
                    f"read in?")))
    for idx, expected in enumerate(fixed):
        if idx >= len(tokens):
            # A row with fewer tokens than the schema's maximum is usually a legitimate
            # optional-trailing-field omission (same graceful-EOF idiom as short files) --
            # this tool's validated strength is catching type mismatches in tokens that
            # ARE present, not judging whether a row "should" have more columns. Exception:
            # min_fields (see docstring above), already handled above.
            break
        tok = tokens[idx]
        if not type_ok(expected, tok):
            issues.append(dict(file=filename, line=lineno, field_index=idx + 1,
                                expected=expected, token=tok, reason=why_not(expected, tok)))
        elif expected == 'real' and INT_RE.match(tok):
            notices.append(dict(file=filename, line=lineno, field_index=idx + 1,
                                 expected=expected, token=tok, reason=REAL_LOOKS_LIKE_INT_MSG))
    if repeat_group:
        rest = tokens[len(fixed):]
        for j, tok in enumerate(rest):
            expected = repeat_group[j % len(repeat_group)]
            if not type_ok(expected, tok):
                issues.append(dict(file=filename, line=lineno, field_index=len(fixed) + j + 1,
                                    expected=expected, token=tok, reason=why_not(expected, tok)))
            elif expected == 'real' and INT_RE.match(tok):
                notices.append(dict(file=filename, line=lineno, field_index=len(fixed) + j + 1,
                                     expected=expected, token=tok, reason=REAL_LOOKS_LIKE_INT_MSG))
    return issues, notices


def check_dataset_file(schema, dataset_dir, verbose=False):
    path = os.path.join(dataset_dir, schema.default_filename)
    if not os.path.isfile(path):
        return None, None, f"file not present in dataset: {schema.default_filename}"
    with open(path, errors="replace") as f:
        raw_lines = [l.rstrip("\n") for l in f]
    # A line that tokenizes to nothing is blank for our purposes -- this also catches
    # degenerate trailing bytes (stray NULs etc. seen at EOF in some shipped files) that
    # str.strip() alone doesn't treat as blank.
    lines = [l for l in raw_lines if tokenize_line(l)]
    # Every one of these files follows title-line-then-header-line-then-data by strong,
    # consistent convention throughout this codebase (verified across dozens of files
    # this session) -- so the real file's own SECOND line is its column-label header,
    # regardless of how the schema itself models title/header as prefix rows. Used only
    # to give --int_to_float's summary a human-readable column name instead of a bare
    # field index; never relied on for anything that affects issues/exit code.
    header_tokens = tokenize_line(lines[1]) if len(lines) > 1 else []

    all_issues = []
    all_notices = []

    def _check(fields, lineno, raw_line, name_offset=0, min_fields=None, max_fields=None):
        # name_offset: for a counted_block's BODY rows, the shared header line's tokens
        # list the header row's own columns FIRST (e.g. management.sch's NAME, NUMB_OPS,
        # NUMB_AUTO) followed by the body row's columns (OP_TYP, MON, DAY, ...) -- but
        # each body-row check_row() call numbers its OWN field_index starting back at 1,
        # so body-row lookups need to skip past the header row's own field count to land
        # on the right header token (e.g. body field #1 = OP_TYP = header_tokens[3], not
        # header_tokens[0] = NAME). 0 for a plain prefix/repeat row or a header row
        # itself, where field_index already lines up with header_tokens directly.
        issues, notices = check_row(fields, dataset_dir, schema.default_filename, lineno, raw_line,
                                     min_fields=min_fields, max_fields=max_fields)
        all_issues.extend(issues)
        for note in notices:
            idx0 = name_offset + note['field_index'] - 1
            note['field_name'] = (header_tokens[idx0] if 0 <= idx0 < len(header_tokens)
                                   else f"field #{note['field_index']}")
        all_notices.extend(notices)

    idx = 0
    for row in schema.prefix_rows:
        if idx >= len(lines):
            # Almost every read in this codebase is immediately followed by
            # `if (eof < 0) exit` -- a file legitimately ending before every optional
            # trailing section is read (e.g. an empty/disabled-feature file, or an older
            # print.prt missing newer object rows) is normal, not a defect. Stop quietly.
            break
        # A prefix row is always a single, unambiguous physical read (no merging of
        # multiple candidate reads happens for d==0 rows) -- its own full fixed-field
        # count is a hard minimum, and (absent a repeat group) a hard maximum too.
        row_fixed, row_repeat = expand_row_schema(row)
        _check(row, idx + 1, lines[idx], min_fields=len(row_fixed),
               max_fields=None if row_repeat else len(row_fixed))
        idx += 1
    else:
        if schema.repeat_row:
            # schema.repeat_row_min_fixed is a source-derived minimum, but the source
            # can't see whether real files actually, consistently reach it -- cap it with
            # what this file's own rows demonstrate (dominant_capped_length) so a
            # field genuinely absent from every row (schema/file drift, not a per-row
            # defect) doesn't get flagged on every single row. max_fields (extra-column
            # detection) gets the SAME empirical treatment in the other direction --
            # computed on raw, uncapped token counts, so a field genuinely present in
            # (but not YET declared for) every row doesn't get flagged either -- but is
            # skipped entirely when the row has a trailing repeat group: those extra
            # tokens are legitimately consumed by the implied-do loop, not "extra" (e.g.
            # hyd_read_connect's nout>0 outgoing-split list).
            repeat_fixed, repeat_group = expand_row_schema(schema.repeat_row)
            repeat_fixed_len = len(repeat_fixed)
            raw_lengths = [len(tokenize_line(l)) for l in lines[idx:]]
            min_fields = schema.repeat_row_min_fixed
            max_fields = None if repeat_group else repeat_fixed_len
            if raw_lengths:
                capped_lengths = [min(l, repeat_fixed_len) for l in raw_lengths]
                if min_fields is not None:
                    min_fields = min(min_fields, dominant_capped_length(capped_lengths))
                if max_fields is not None:
                    max_fields = max(max_fields, dominant_capped_length(raw_lengths))
            while idx < len(lines):
                _check(schema.repeat_row, idx + 1, lines[idx],
                       min_fields=min_fields, max_fields=max_fields)
                idx += 1
        elif schema.counted_block:
            # Repeating "header row declares N, then N body rows follow" structure
            # (management.sch's NUMB_OPS, soils.sol's per-soil layer count, ...). The header
            # row's own type-check already ran above at each iteration; here its count field's
            # ACTUAL VALUE is read from the real data and used to check the file has exactly
            # that many body records before the next header (or EOF) -- not just that each
            # token has the right type, but that the file's record count matches what the
            # code will actually try to read.
            cb = schema.counted_block
            header_row, field_idx, body_row = cb['header_row'], cb['header_field_index'], cb['body_row']
            # Like a prefix row, a counted_block's header_row is only ever built from a
            # single candidate read (find_count_field_position requires it to be the ONLY
            # read at its depth) -- no merging, so its full fixed-field count is a
            # candidate hard minimum (and, absent a repeat group, a candidate hard
            # maximum too), same reasoning as repeat_row above. But it's still only a
            # STATIC guarantee, so cross-check it the same way against what this file's
            # own header rows (there can be many -- one per block) actually, consistently
            # provide: pre-scan every header position first (a header's position depends
            # on the previous block's real body-row count, so this has to walk the same
            # header/body structure as the real pass below, just without checking
            # anything yet). body_row is NOT given min_fields or max_fields: it's picked
            # as merely the longest read observed anywhere in its loop span, with no
            # prefix validation against shorter reads, so neither bound is reliably known.
            header_fixed, header_repeat = expand_row_schema(header_row)
            header_fixed_len = len(header_fixed)
            scan_idx = idx
            header_capped_lengths = []
            header_raw_lengths = []
            while scan_idx < len(lines):
                header_tokens_here = tokenize_line(lines[scan_idx])
                header_capped_lengths.append(min(len(header_tokens_here), header_fixed_len))
                header_raw_lengths.append(len(header_tokens_here))
                scan_count = None
                if field_idx < len(header_tokens_here):
                    tok = header_tokens_here[field_idx]
                    if INT_RE.match(tok):
                        scan_count = int(tok)
                    elif REAL_RE.match(tok):
                        scan_count = int(round(float(tok)))
                scan_idx += 1
                if scan_count is None:
                    break
                scan_idx += min(scan_count, len(lines) - scan_idx)
            header_min_fields = header_fixed_len
            header_max_fields = None if header_repeat else header_fixed_len
            if header_capped_lengths:
                header_min_fields = min(header_fixed_len, dominant_capped_length(header_capped_lengths))
                if header_max_fields is not None:
                    header_max_fields = max(header_max_fields, dominant_capped_length(header_raw_lengths))
            while idx < len(lines):
                header_line = lines[idx]
                _check(header_row, idx + 1, header_line,
                       min_fields=header_min_fields, max_fields=header_max_fields)
                tokens = tokenize_line(header_line)
                count = None
                if field_idx < len(tokens):
                    tok = tokens[field_idx]
                    if INT_RE.match(tok):
                        count = int(tok)
                    elif REAL_RE.match(tok):
                        count = int(round(float(tok)))
                header_lineno = idx + 1
                idx += 1
                if count is None:
                    # count field didn't parse as a number -- already flagged by the header
                    # row's own type check above; can't know how many body rows to expect.
                    break
                available = len(lines) - idx
                if count > available:
                    all_issues.append(dict(
                        file=schema.default_filename, line=header_lineno, field_index=field_idx + 1,
                        expected=f"{count} more record(s)", token=f"only {available} left in file",
                        reason=(f"header on line {header_lineno} declares {count} records to follow "
                                f"but only {available} remain in the file")))
                    count = available
                for _ in range(count):
                    _check(body_row, idx + 1, lines[idx], name_offset=len(header_row))
                    idx += 1
        elif idx < len(lines):
            # No repeat_row, no counted_block -- this file's read routine consumes
            # exactly len(schema.prefix_rows) lines (title/header/one-shot data row(s),
            # e.g. parameters.bsn) and stops for good, unconditionally, so there's no
            # legitimate reason for more non-blank data to follow. Every prefix-only file
            # surveyed across 5 real datasets matches its schema's line count exactly, so
            # this isn't drift-prone the way missing-column detection was (see
            # dominant_capped_length) -- flag every extra line straight away.
            extra = len(lines) - idx
            all_issues.append(dict(
                file=schema.default_filename, line=idx + 1, field_index=1,
                expected="end of file", token=f"{extra} extra line(s)",
                reason=(f"this file's read routine consumes exactly {idx} line(s) "
                        f"(title/header/data) and stops -- {extra} more non-blank "
                        f"line(s) follow starting here, never read; a duplicated or "
                        f"stray extra data line?")))
    return all_issues, all_notices, None


# ============================================================================
# HRU / channel object-count cross-check
#
# Every spatial object type (hru, chan, chandeg, ru, aqu, res, ...) is read in two
# separate places that both have to agree, or the model silently misbehaves:
#   - a "connect" file (hru.con, channel.con, chandeg.con, ...), one row per object,
#     read by hyd_read_connect.f90 -- each row's `props` column is an index into...
#   - a "properties" file (hru-data.hru, channel.cha, channel-lte.cha), read by its
#     own subroutine (hru_read.f90, ch_read.f90, sd_channel_read.f90) using a
#     self-counting "read every id, take the max, allocate, then read again" idiom.
# If a connect-file row's `props` value has no matching id in the properties file
# (a mismatched HRU/channel count between the two files, a typo'd id, a row added to
# one file but not the other, ...), that object silently gets a properties record
# still holding compile-time defaults -- no error, no warning, just wrong results
# for that HRU or channel. This check reads both real files and cross-references them.
# ============================================================================

HYD_CONNECT_CALL_RE = re.compile(
    r'''call\s+hyd_read_connect\s*\(\s*([^,]+),\s*(['"])([^'"]*)\2\s*,\s*[^,]+,\s*sp_ob\s*%\s*([A-Za-z_]\w*)''',
    re.IGNORECASE)

# Which properties file backs each connect-file object type's `props` column -- a
# structural fact about this codebase (hru_read.f90 / ch_read.f90 / sd_channel_read.f90
# each hard-code which type(input_*) field they open), not a per-dataset default, so it's
# the one piece not re-derived from a single call site the way the connect-file names are.
# The actual FILENAMES are still resolved dynamically from input_file_module.f90, same as
# everywhere else in this tool -- only the object-type <-> module-field pairing is fixed here.
PROPERTY_FILE_EXPR_BY_OBJTYPE = {
    'hru': 'in_hru%hru_data',
    'chan': 'in_cha%dat',
    'chandeg': 'in_cha%chan_ez',
}


def find_field_token_position(varlist_text, dotted_field_name):
    """0-based index of a plain `array(...)%FIELD` item within varlist_text's
       top-level comma-separated items (purely syntactic -- no type resolution, since
       every field in this read is a scalar and item position IS token position)."""
    items = split_top_level(varlist_text, ',')
    for idx, item in enumerate(items):
        m = re.match(r'^[A-Za-z_]\w*\s*(?:\([^()]*\))?\s*%\s*([A-Za-z_]\w*)\s*$', item.strip(), re.IGNORECASE)
        if m and m.group(1).lower() == dotted_field_name.lower():
            return idx
    return None


def find_hyd_connect_props_col(files):
    """Locate the 0-based column position of `ob(i)%props` in hyd_read_connect.f90's own
       read statement -- this is the connect-file column every object type's row uses to
       reference its properties-file id, so its position is discovered once, from the one
       subroutine all object types share, rather than assumed."""
    for path in files:
        if os.path.basename(path).lower() != 'hyd_read_connect.f90':
            continue
        for _, text in read_logical_lines(path):
            mr = parse_read_statement(text.strip())
            if mr:
                unit, is_ld, varlist = mr
                pos = find_field_token_position(varlist, 'props')
                if pos is not None:
                    return pos
    return None


def discover_object_types(files, type_defaults, var_to_type):
    """Every spatial object type this checkout's hyd_connect.f90 wires up, e.g.
       {'hru': 'hru.con', 'chan': 'channel.con', 'chandeg': 'chandeg.con', ...} --
       found by scanning for `call hyd_read_connect(FILE, "LABEL", ..., sp_ob%FIELD, ...)`
       call sites rather than hardcoded, so a renamed/added/removed object type is
       reflected automatically."""
    out = {}
    for path in files:
        for _, text in read_logical_lines(path):
            m = HYD_CONNECT_CALL_RE.search(text.strip())
            if not m:
                continue
            file_expr = m.group(1).strip().lower()
            label = m.group(3).strip().lower()
            if not label:
                continue
            fn = resolve_default_filename(file_expr, type_defaults, var_to_type)
            if fn:
                out[label] = fn
    return out


def materialize_hyd_connect_schemas(schemas, src_files, type_defaults, var_to_type):
    """hyd_read_connect.f90 is one shared subroutine that every spatial-object-type's
       connect file (hru.con, channel.con, chandeg.con, aqu.con, res.con, rec.con,
       exco.con, dr.con, outlet.con, gwflow.con, ...) is read through -- but the file it
       opens is a dummy argument (`con_file`), not a derived-type field resolvable to a
       default filename from inside the subroutine itself, so the normal per-subroutine
       schema derivation gives up on it entirely (`unknown default filename for
       'con_file'`) and every one of these files goes completely unchecked, silently.
       The real filename only exists at each call site in hyd_connect.f90 (e.g.
       `call hyd_read_connect(in_con%hru_con, "hru", ...)`) -- already parsed by
       discover_object_types() for the object-count check -- so reuse that mapping here to
       clone hyd_read_connect's already-correctly-derived row schema (prefix_rows /
       repeat_row / counted_block are identical for every object type, since they all run
       through the exact same read statements) once per real target file."""
    base = next((s for s in schemas if s.subname == 'hyd_read_connect'
                 and (s.prefix_rows or s.repeat_row or s.counted_block)), None)
    if base is None:
        return []
    obj_types = discover_object_types(src_files, type_defaults, var_to_type)
    return [
        dc_replace(base, file_expr=f'hyd_read_connect(..., "{label}", ...)',
                   default_filename=filename, unresolved_reason=None)
        for label, filename in obj_types.items()
    ]


def read_column_values(path, col_index, skip_header_lines=2):
    """Ints found at `col_index` (0-based) across every data row of `path`, after
       skipping the title + header lines every one of these files starts with. A row
       that's too short to have a token at col_index, or whose token there doesn't parse
       as an integer, contributes None rather than being silently dropped: the row still
       EXISTS (a real record the model will try to read), so it must still count toward
       the row count a caller compares against another file's row count. Silently
       dropping such rows previously let a malformed row's id vanish from the count
       entirely (e.g. a connect-file row whose `props` token got shifted off its column
       by an unrelated missing field upstream no longer parsed as an integer, and the
       count masked a genuine object-count mismatch instead of catching it). Returns None
       (not a list) if the file doesn't exist."""
    if not os.path.isfile(path):
        return None
    with open(path, errors="replace") as f:
        raw = f.readlines()
    lines = [l for l in raw if tokenize_line(l)]
    vals = []
    for l in lines[skip_header_lines:]:
        tokens = tokenize_line(l)
        if col_index < len(tokens) and INT_RE.match(tokens[col_index]):
            vals.append(int(tokens[col_index]))
        else:
            vals.append(None)
    return vals


def read_string_column_values(path, col_index, skip_header_lines=2):
    """Like read_column_values, but returns the raw (quote-stripped) token text at
       `col_index` regardless of type -- for name-referenced columns rather than
       id-referenced ones. Returns None if the file doesn't exist."""
    if not os.path.isfile(path):
        return None
    with open(path, errors="replace") as f:
        raw = f.readlines()
    lines = [l for l in raw if tokenize_line(l)]
    vals = []
    for l in lines[skip_header_lines:]:
        tokens = tokenize_line(l)
        if col_index < len(tokens):
            vals.append(tokens[col_index].strip('\'"'))
    return vals


def check_object_counts(dataset_dir, src_files, type_defaults, var_to_type):
    """For hru/chan/chandeg: cross-reference the connect file's `props` column (which
       row of the properties file each object actually uses) against the ids real present
       in the properties file. Returns a list of (objtype, con_filename, prop_filename,
       n_con_rows, n_prop_rows, missing_in_props, orphaned_in_props)."""
    findings = []
    props_col = find_hyd_connect_props_col(src_files)
    if props_col is None:
        return findings
    obj_types = discover_object_types(src_files, type_defaults, var_to_type)
    for objtype, prop_expr in PROPERTY_FILE_EXPR_BY_OBJTYPE.items():
        con_filename = obj_types.get(objtype)
        if not con_filename:
            continue
        prop_filename = resolve_default_filename(prop_expr, type_defaults, var_to_type)
        if not prop_filename:
            continue
        con_path = os.path.join(dataset_dir, con_filename)
        prop_path = os.path.join(dataset_dir, prop_filename)
        con_props = read_column_values(con_path, props_col)
        prop_ids = read_column_values(prop_path, 0)
        if con_props is None or prop_ids is None:
            continue  # one or both files not present -- not this check's job (missing-file check covers that)
        prop_id_set = set(prop_ids)
        # key=str: con_props/prop_id_set can now contain None (an unreadable id -- see
        # read_column_values) alongside int ids, and None isn't orderable against int.
        missing_in_props = sorted(set(con_props) - prop_id_set, key=str)
        orphaned_in_props = sorted(prop_id_set - set(con_props), key=str)
        if len(con_props) != len(prop_id_set) or missing_in_props or orphaned_in_props:
            findings.append((objtype, con_filename, prop_filename, len(con_props),
                              len(prop_id_set), missing_in_props, orphaned_in_props))
    return findings


# ============================================================================
# Name-referenced cross-file checks (soils.sol, landuse.lum, snow.sno, field.fld,
# topography.hyd, hydrology.hyd, soil_plant.ini, channel init/hyd/sed/nut, ...)
#
# hru-data.hru (and several other files) don't just reference other files by row id --
# most of their columns are a NAME that has to exactly match a row in some other
# database file (hru-data.hru's `soil` column has to match a name in soils.sol, `lu_mgt`
# has to match landuse.lum, etc.). This codebase already performs exactly this check at
# runtime and reports failures to a warning file: every one of these lookups follows the
# same idiom --
#     do i = 1, n
#       if (SRC_FIELD == TARGET_ARRAY(i)%name) then
#         ...
#         exit
#       end if
#     end do
#     if (... == 0) write (9001,*) SRC_FIELD, "...not found (LABEL)"
# -- so rather than hand-list which columns reference which files, this scans for that
# idiom itself (throughout the whole codebase, not just hru_read.f90 -- the same pattern
# also covers channel.cha's init/hyd/sed/nut columns, reservoir.con's, weather-sta.cli's
# gauge columns, ...) and checks the real referenced name is really present in the real
# target file, ahead of the model run instead of after it in a warning file.
# ============================================================================

LOOKUP_IF_RE = re.compile(
    r'''^if\s*\(\s*(.+?)\s*==\s*([A-Za-z_]\w*)\s*\(\s*[A-Za-z_]\w*\s*\)\s*(?:%\s*[A-Za-z_]\w*)+\s*\)\s*then\s*$''',
    re.IGNORECASE)
NOT_FOUND_WRITE_RE = re.compile(
    r'''write\s*\(\s*9001\s*,.*not\s+found\s*\(([^)]*)\)''', re.IGNORECASE)
ARRAY_ELEM_ITEM_RE = re.compile(r'^([A-Za-z_]\w*)\s*\(\s*[A-Za-z_]\w*\s*\)\s*(?:%|$)', re.IGNORECASE)


def parse_dotted_expr(expr):
    """'hru_db(i)%dbsc%land_use_mgt' -> ('hru_db', ['dbsc', 'land_use_mgt'])."""
    m = re.match(r'^([A-Za-z_]\w*)\s*(?:\([^()]*\))?((?:\s*%\s*[A-Za-z_]\w*(?:\([^()]*\))?)*)\s*$',
                 expr.strip(), re.IGNORECASE)
    if not m:
        return None, None
    base = m.group(1).lower()
    fields = [f.lower() for f in re.findall(r'%\s*([A-Za-z_]\w*)', m.group(2))]
    return base, fields


def flatten_type_leaf_names(types, base_type, prefix_seen=None):
    """Like flatten_type, but keeps each leaf's dotted field-name path (relative to
       base_type) alongside its type, e.g. [('name','character'), ('s%snam','character'),
       ('s%nly','integer'), ...] -- needed to locate a NAMED field's position, not just
       count leaves."""
    if prefix_seen is None:
        prefix_seen = set()
    if not (isinstance(base_type, tuple) and base_type[0] == 'derived'):
        return [(None, base_type)]
    tname = base_type[1]
    if tname in prefix_seen or tname not in types:
        return []
    prefix_seen = prefix_seen | {tname}
    out = []
    for fname, ftype in types[tname].fields:
        for subname, leaf in flatten_type_leaf_names(types, ftype, prefix_seen):
            out.append((fname if subname is None else f"{fname}%{subname}", leaf))
    return out


def find_dotted_field_leaf_position(varlist_text, lhs_base, lhs_fields,
                                     local_vars, used_modules, module_vars, types):
    """0-based leaf offset of `lhs_base%lhs_fields...` within varlist_text's resolved
       reads -- e.g. locating 'land_use_mgt' inside a read of `k, hru_db(i)%dbsc` (which
       reads the WHOLE dbsc struct as one item) by matching 'hru_db' as a prefix of the
       item and then descending into its flattened field names for the remainder."""
    items = split_top_level(varlist_text, ',')
    offset = 0
    for item in items:
        item = item.strip()
        if not item:
            continue
        _, is_repeat = split_implied_do(item)
        if is_repeat:
            return None
        item_base, item_fields = parse_dotted_expr(item)
        if item_base == lhs_base and item_fields is not None and \
                lhs_fields[:len(item_fields)] == item_fields:
            remaining = lhs_fields[len(item_fields):]
            t = resolve_expr_type(item, local_vars, used_modules, module_vars, types)
            if t is None:
                return None
            if not remaining:
                return offset
            target_name = '%'.join(remaining)
            for k, (dotted_name, _leaf) in enumerate(flatten_type_leaf_names(types, t)):
                if dotted_name == target_name:
                    return offset + k
            return None
        t = resolve_expr_type(item, local_vars, used_modules, module_vars, types)
        if t is None:
            return None
        leaves = flatten_type(types, t) if isinstance(t, tuple) else [t]
        offset += len(leaves)
    return None


def scan_name_lookup_registry(files, types, module_vars):
    """Find every name-lookup-then-'not found'-warning idiom in the codebase (see module
       docstring above) and resolve each one to (source file_expr, leaf column position,
       target array name, warning label). The 'not found' write is matched to whichever
       lookup do-loop most recently preceded it in the same subroutine -- reliable here
       because every occurrence of this idiom keeps the lookup loop and its warning
       textually adjacent, even when (as in hru_read.f90's soil_plant_init check) the
       warning ends up nested a level deeper than its sibling checks."""
    out = []
    for path in files:
        lines = read_logical_lines(path)
        n = len(lines)
        i = 0
        while i < n:
            lineno, text = lines[i]
            msub = SUBROUTINE_START.match(text.strip())
            if not msub:
                i += 1
                continue
            i += 1
            used_modules = []
            local_vars = {}
            read_texts = []  # (file_expr, varlist_text)
            current_file_expr = None
            pending = None  # (lhs_expr_text, array_name)
            while i < n and not SUBROUTINE_END.match(lines[i][1].strip()):
                l2, t2 = lines[i]
                s2 = t2.strip()
                mu = USE_RE.match(s2)
                if mu:
                    used_modules.append(mu.group(1).lower())
                    i += 1
                    continue
                decl = parse_declaration_line(t2)
                if decl and not READ_HEAD_RE.match(s2):
                    base, names = decl
                    for nm in names:
                        local_vars[nm] = base
                    i += 1
                    continue
                mo = OPEN_RE.match(s2)
                if mo:
                    current_file_expr = mo.group(2).strip().lower()
                    i += 1
                    continue
                mr = parse_read_statement(s2)
                if mr:
                    unit, is_ld, varlist = mr
                    if is_ld:
                        read_texts.append((current_file_expr, varlist))
                    i += 1
                    continue
                ml = LOOKUP_IF_RE.match(s2)
                if ml:
                    pending = (ml.group(1).strip(), ml.group(2).strip().lower())
                    i += 1
                    continue
                mw = NOT_FOUND_WRITE_RE.search(s2)
                if mw and pending is not None:
                    lhs_expr, array_name = pending
                    pending = None
                    lhs_base, lhs_fields = parse_dotted_expr(lhs_expr)
                    if lhs_base and lhs_fields:
                        for file_expr, varlist in read_texts:
                            if not file_expr:
                                continue
                            pos = find_dotted_field_leaf_position(
                                varlist, lhs_base, lhs_fields, local_vars, used_modules,
                                module_vars, types)
                            if pos is not None:
                                out.append(dict(
                                    srcfile=os.path.basename(path), source_file_expr=file_expr,
                                    position=pos, array=array_name, label=mw.group(1).strip(),
                                    lhs_expr=lhs_expr))
                                break
                    i += 1
                    continue
                i += 1
    return out


def find_array_source_files(files, type_defaults, var_to_type):
    """{array_name: default_filename} for every 'open a file, read one row per array
       element' subroutine in the codebase (landuse_read.f90 -> lum <- landuse.lum,
       soil_db_read.f90 -> soildb <- soils.sol, ...) -- found generically by looking for
       any list-directed read whose varlist indexes some ARRAY(idx) or ARRAY(idx)%field,
       paired with the nearest preceding open() in the same subroutine."""
    out = {}
    for path in files:
        lines = read_logical_lines(path)
        n = len(lines)
        i = 0
        while i < n:
            lineno, text = lines[i]
            msub = SUBROUTINE_START.match(text.strip())
            if not msub:
                i += 1
                continue
            i += 1
            current_file_expr = None
            while i < n and not SUBROUTINE_END.match(lines[i][1].strip()):
                l2, t2 = lines[i]
                s2 = t2.strip()
                mo = OPEN_RE.match(s2)
                if mo:
                    current_file_expr = mo.group(2).strip().lower()
                    i += 1
                    continue
                mr = parse_read_statement(s2)
                if mr and current_file_expr:
                    unit, is_ld, varlist = mr
                    if is_ld:
                        for item in split_top_level(varlist, ','):
                            am = ARRAY_ELEM_ITEM_RE.match(item.strip())
                            if am:
                                arr = am.group(1).lower()
                                if arr not in out:
                                    fn = resolve_default_filename(current_file_expr, type_defaults, var_to_type)
                                    if fn:
                                        out[arr] = fn
                                break
                    i += 1
                    continue
                i += 1
    return out


def check_name_references(dataset_dir, src_files, types, module_vars, type_defaults, var_to_type):
    """Cross-check every name-lookup found by scan_name_lookup_registry against real
       dataset files: does the source file's referenced name actually exist as a `name`
       (assumed column 0 -- true of every lookup-database type this tool has found:
       soils.sol, landuse.lum, topography.hyd, hydrology.hyd, snow.sno, field.fld,
       soil_plant.ini, ...) in the target file? Returns a list of (label, source_filename,
       target_filename, [missing name values])."""
    lookups = scan_name_lookup_registry(src_files, types, module_vars)
    array_source_files = find_array_source_files(src_files, type_defaults, var_to_type)
    findings = []
    seen = set()  # (source_filename, position, target_filename) -- de-dup repeated hits
    for lk in lookups:
        source_filename = resolve_default_filename(lk['source_file_expr'], type_defaults, var_to_type)
        target_filename = array_source_files.get(lk['array'])
        if not source_filename or not target_filename:
            continue
        key = (source_filename, lk['position'], target_filename)
        if key in seen:
            continue
        seen.add(key)
        source_path = os.path.join(dataset_dir, source_filename)
        target_path = os.path.join(dataset_dir, target_filename)
        source_values = read_string_column_values(source_path, lk['position'])
        target_names = read_string_column_values(target_path, 0)
        if source_values is None or target_names is None:
            continue
        target_name_set = set(target_names)
        missing = sorted({v for v in source_values
                           if v and v.lower() != 'null' and v not in target_name_set})
        if missing:
            findings.append((lk['label'], source_filename, target_filename, missing))
    return findings


def main():
    ap = argparse.ArgumentParser(
        description="Cross-check SWAT+ input-file data against what the Fortran source's "
                    "list-directed READ statements actually expect, field by field.",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog=__doc__,
    )
    ap.add_argument("dataset_dir", nargs="?", default=None,
                     help="dataset directory to check (e.g. workdata/IA-Clayton_Test_Case). "
                          "Omit to just build and summarize the source-derived schema index.")
    ap.add_argument("--repo", default=".", help="path to the swatplus repo root (default: .)")
    ap.add_argument("--file", default=None, help="only check this one input filename")
    ap.add_argument("-v", "--verbose", action="store_true",
                     help="also list subroutines/files the tool could not resolve or skipped")
    ap.add_argument("--int_to_float", action="store_true",
                     help="report REAL-typed columns whose data has no decimal point/exponent "
                          "(e.g. '0' instead of '0.0') -- valid Fortran, not a mismatch, off by "
                          "default; prints one summary line per affected column per file, not "
                          "one line per occurrence")
    args = ap.parse_args()

    src_files = sorted(glob.glob(os.path.join(args.repo, SRC_GLOB)))
    if not src_files:
        print(f"no source files found under {os.path.join(args.repo, SRC_GLOB)}", file=sys.stderr)
        sys.exit(2)

    # Everything below is re-derived from src/ on every run -- there is no cached/pinned
    # schema. Stamp which version of the source that was, since the whole point is that
    # the expected column types and counts are whatever THIS checkout's READ statements
    # say, not a fixed schema from when this tool was written.
    try:
        import subprocess
        rev = subprocess.run(["git", "-C", args.repo, "describe", "--always", "--dirty"],
                              capture_output=True, text=True, timeout=5).stdout.strip()
    except Exception:
        rev = None
    if rev:
        print(f"[src] deriving schema from {os.path.abspath(args.repo)} @ {rev}")

    types, module_vars = parse_all_types_and_module_vars(src_files)
    type_defaults, var_to_type = parse_default_filenames(
        os.path.join(args.repo, "src", "input_file_module.f90"))
    count_registry = scan_count_registry(src_files, types, module_vars)
    schemas, unclosed_reuse = scan_subroutines(
        src_files, types, module_vars, type_defaults, var_to_type, count_registry)
    schemas = schemas + materialize_hyd_connect_schemas(
        schemas, src_files, type_defaults, var_to_type)

    resolved = [s for s in schemas if s.default_filename and not s.unresolved_reason
                and (s.prefix_rows or s.repeat_row or s.counted_block)]
    resolved_ids = {id(s) for s in resolved}
    unresolved = [s for s in schemas if id(s) not in resolved_ids]

    print(f"[index] {len(src_files)} source files, {len(types)} derived types, "
          f"{len(schemas)} file-read routines found, {len(resolved)} fully resolved.")
    if unclosed_reuse:
        print(f"\n[source] {len(unclosed_reuse)} unit number reused for a second open() "
              f"with no close() in between:")
        print(f"    Executing an OPEN on a unit that's still connected to a file is "
              f"well-defined, standard-conforming Fortran (has been since FORTRAN 77): "
              f"the effect is as if a CLOSE with no STATUS= (i.e. default disposition, "
              f"KEEP for a named file -- nothing is deleted) ran first, then the new file "
              f"is connected. This is NOT undefined or compiler-dependent behavior, and "
              f"every conforming compiler does the same thing.")
        print(f"    Flagged anyway because it's a readability/maintainability smell, not "
              f"a correctness bug: relying on the implicit close hides \"this reopens a "
              f"DIFFERENT file\" as a side effect of a second open() line instead of "
              f"stating it with an explicit close(), which is easy to misread and easy to "
              f"get wrong if a future edit ever needs to reopen the SAME file mid-routine.")
        for (srcfile, subname, unit, l1, l2b, f1, f2) in unclosed_reuse:
            print(f"  {srcfile} ({subname}): unit {unit} opened for {f1} at line {l1}, "
                  f"reopened for {f2} at line {l2b} with no close() between them")
    if args.verbose:
        for s in unresolved:
            print(f"  [skip] {s.subname} ({s.srcfile}) -> "
                  f"{s.default_filename or s.file_expr}: {s.unresolved_reason}")

    if args.dataset_dir is None:
        if unclosed_reuse:
            sys.exit(1)
        return

    by_filename = {}
    for s in resolved:
        by_filename.setdefault(s.default_filename, s)  # first resolved schema wins

    if args.file:
        by_filename = {k: v for k, v in by_filename.items() if k == args.file}
        if not by_filename:
            print(f"no resolved read-schema for '{args.file}'", file=sys.stderr)
            sys.exit(2)

    total_issues = len(unclosed_reuse)
    total_notices = 0
    # {filename: {field_name: occurrence_count}} -- only populated/printed when
    # --int_to_float is given; a summary of which COLUMNS ever show the pattern, not one
    # line per row that shows it (that reading was tried first and was much too noisy on
    # a real dataset -- e.g. plants.plt alone produced 2279 individual line-hits).
    int_to_float_summary = {}
    checked = 0
    for filename, schema in sorted(by_filename.items()):
        issues, notices, skip_reason = check_dataset_file(schema, args.dataset_dir, verbose=args.verbose)
        if skip_reason:
            if args.verbose:
                print(f"  [skip] {filename}: {skip_reason}")
            continue
        checked += 1
        if issues:
            print(f"\n{filename}  ({schema.subname}, {schema.srcfile}) -- {len(issues)} issue(s):")
            for iss in issues:
                print(f"  line {iss['line']:>6}  field #{iss['field_index']:<3} "
                      f"expected {iss['expected']:<9} got {iss['token']!r:<20} -- {iss['reason']}")
            total_issues += len(issues)
        elif args.verbose:
            mode = (", + repeating rows" if schema.repeat_row else
                    ", + counted header/body blocks" if schema.counted_block else "")
            print(f"  [ok] {filename}: {len(schema.prefix_rows)} prefix row(s){mode}")
        # notices are NOT issues -- not an error, not a warning, don't count toward
        # total_issues or the exit code. A REAL field whose token has no decimal
        # point/exponent is completely valid Fortran (promoted to floating point on
        # read); this is purely informational, and only reported at all under
        # --int_to_float.
        if notices:
            total_notices += len(notices)
            if args.int_to_float:
                per_field = int_to_float_summary.setdefault(filename, {})
                for note in notices:
                    per_field[note['field_name']] = per_field.get(note['field_name'], 0) + 1

    if args.int_to_float and int_to_float_summary:
        print(f"\n[int_to_float] REAL-typed column(s) whose data has no decimal "
              f"point/exponent anywhere it was found -- valid Fortran, NOT a mismatch, "
              f"informational only:")
        for filename in sorted(int_to_float_summary):
            per_field = int_to_float_summary[filename]
            cols = ", ".join(f"{name} ({count}x)" for name, count in
                              sorted(per_field.items(), key=lambda kv: -kv[1]))
            print(f"  {filename}: {cols}")

    # ------------------------------------------------------------------
    # Missing / unexpected input files.
    #
    # "Needed" is determined per-dataset, not from a fixed list: file.cio is the
    # dataset's own manifest (this codebase supports many optional subsystems --
    # gwflow, salts, constituents, water rights, ... -- each switched off with a
    # 'null' slot for most datasets, so a blanket required-file list would false-
    # positive constantly). A separate, smaller set of files is opened via a
    # hardcoded literal filename rather than routed through file.cio (e.g.
    # carb_coefs.cbn) -- those are cross-checked against whether the source
    # guards them with `inquire(file=X, exist=...)` before reading, since that's
    # this codebase's own signal for "this file is conditionally read."
    if not args.file:
        cio_path = os.path.join(args.dataset_dir, "file.cio")
        referenced = parse_file_cio(cio_path)
        referenced |= resolve_cli_indirection(referenced, args.dataset_dir)
        try:
            on_disk = {f.lower() for f in os.listdir(args.dataset_dir)
                       if os.path.isfile(os.path.join(args.dataset_dir, f))}
        except OSError:
            on_disk = set()

        literal_schemas = {}  # default_filename -> schema, for hardcoded (non-file.cio) opens
        for s in schemas:
            if s.default_filename and re.match(r'''^(['"])''', s.file_expr):
                literal_schemas.setdefault(s.default_filename.lower(), s)

        # every filename the source ever assigns a default for, file.cio-routed or not --
        # used only to avoid flagging a currently-disabled-feature file (file.cio nulls its
        # slot, but the placeholder file is still sitting in the dataset dir) as unexpected.
        all_type_default_filenames = {
            fn.lower() for fields in type_defaults.values() for fn in fields.values()
        }
        all_opened_anywhere = scan_all_opens(src_files, type_defaults, var_to_type)

        missing_required = sorted(referenced - on_disk)
        missing_conditional = sorted(
            fn for fn, s in literal_schemas.items()
            if fn not in referenced and fn not in on_disk and not s.guarded
        )
        missing_optional_absent = sorted(
            fn for fn, s in literal_schemas.items()
            if fn not in referenced and fn not in on_disk and s.guarded
        )

        known = (referenced | set(literal_schemas) | all_type_default_filenames
                 | {"file.cio"})
        unexpected = sorted(
            f for f in on_disk
            if f not in known and f not in all_opened_anywhere and not is_output_like(f)
        )

        if os.path.isfile(cio_path):
            if missing_required:
                print(f"\n[missing] file.cio references these files but they are not in "
                      f"{args.dataset_dir}:")
                for fn in missing_required:
                    print(f"  {fn}")
            if missing_conditional:
                # Not counted as a hard failure: these are read via a hardcoded filename
                # (not routed through file.cio) with no `inquire(exist=...)` guard this tool
                # recognizes -- but the source may still gate the whole read behind a feature
                # flag (e.g. bsn_cc%cs_db) this tool doesn't trace. Worth a look, not a verdict.
                print(f"\n[missing?] read via a hardcoded filename (not file.cio-routed) with "
                      f"no recognized existence guard -- verify whether this dataset needs it "
                      f"(not counted below):")
                for fn in missing_conditional:
                    print(f"  {fn}")
            if args.verbose and missing_optional_absent:
                print(f"\n[not present, but guarded/optional -- probably fine]:")
                for fn in missing_optional_absent:
                    print(f"  {fn}")
            if unexpected:
                print(f"\n[unexpected] present in {args.dataset_dir} but not referenced by "
                      f"file.cio (incl. .cli indirection), not opened anywhere in the source, "
                      f"and doesn't look like model output (heuristic -- verify before "
                      f"deleting anything):")
                for fn in unexpected:
                    print(f"  {fn}")
        elif args.verbose:
            print(f"\n[skip] no file.cio in {args.dataset_dir} -- "
                  f"can't determine which optional files this dataset needs")

        total_issues += len(missing_required)

        # HRU / channel (and any other object type this checkout wires up) count
        # cross-check: does the connect file's object list agree with the properties
        # file it points into?
        object_count_findings = check_object_counts(
            args.dataset_dir, src_files, type_defaults, var_to_type)
        if object_count_findings:
            print(f"\n[objcount] connect-file/properties-file object count mismatch:")
            for (objtype, con_fn, prop_fn, n_con, n_prop, missing, orphaned) in object_count_findings:
                print(f"  {objtype}: {con_fn} references {n_con} object(s), "
                      f"{prop_fn} has {n_prop} record(s)")
                if missing:
                    shown = ["(unreadable)" if m is None else m for m in missing[:10]]
                    more = f" (+{len(missing) - 10} more)" if len(missing) > 10 else ""
                    print(f"    {con_fn} references {prop_fn} id(s) not present there: "
                          f"{shown}{more}")
                if orphaned:
                    shown = orphaned[:10]
                    more = f" (+{len(orphaned) - 10} more)" if len(orphaned) > 10 else ""
                    print(f"    {prop_fn} has id(s) no {con_fn} row references: "
                          f"{shown}{more}")
            total_issues += len(object_count_findings)

        # Name-referenced cross-file check: soils.sol, landuse.lum, snow.sno, field.fld,
        # topography.hyd, hydrology.hyd, soil_plant.ini, channel init/hyd/sed/nut, ...
        name_ref_findings = check_name_references(
            args.dataset_dir, src_files, types, module_vars, type_defaults, var_to_type)
        if name_ref_findings:
            print(f"\n[nameref] referenced name(s) not present in the target database file:")
            for (label, source_fn, target_fn, missing) in name_ref_findings:
                shown = missing[:10]
                more = f" (+{len(missing) - 10} more)" if len(missing) > 10 else ""
                print(f"  {source_fn} references {len(missing)} name(s) not found in "
                      f"{target_fn} ({label}): {shown}{more}")
            total_issues += len(name_ref_findings)

    notices_hint = ""
    if total_notices:
        notices_hint = (f", {total_notices} REAL-looks-like-integer notice(s) (not "
                         f"counted as issues" +
                         ("" if args.int_to_float else ", re-run with --int_to_float to list them") +
                         ")")
    print(f"\n[summary] checked {checked} file(s) against {args.dataset_dir}: "
          f"{total_issues} type mismatch(es)/missing-column(s)/missing-file issue(s) "
          f"found{notices_hint}.")
    if total_issues:
        sys.exit(1)


if __name__ == "__main__":
    main()
