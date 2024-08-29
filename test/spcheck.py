#!/usr/bin/env python3

# Generic SWAT+ checker, O. David, CSU, 2024
# - runs through ctest with known scenarios
# - Allows to execute various executables against a scenario
# - Allows to compare scenario outputs for output deltas

import os
import pathlib
import shutil
import sys
import argparse
import glob
from subprocess import Popen
from datetime import datetime




# replace all non-numeric characters (not: 1-9, E, +, -, .) with spaces
def sanitize(a: str) -> str:
    s = list(a)
    for i in range(0, len(s)):
        if (s[i].isdigit()
                or (s[i] == '+' and s[i + 1].isdigit())
                or (s[i] == '-' and s[i + 1].isdigit())
                or s[i] == '.'
                or (s[i].lower() == 'e' and s[i - 1].isdigit())
                or s[i] == '\n'):
            continue
        s[i] = ' '
    return "".join(s).strip()


# check if s is a float
def is_float(s: str) -> bool:
    if s is None:
        return False
    try:
        int(s)  # skip comparisons if ints
        return False
    except ValueError:
        try:
            float(s)
            return True  # only compare floats
        except ValueError:
            return False


def pos_line(line1: str, linetok: [str]) -> str:
    pos: int = 0
    a: [int] = []
    for t in linetok:
        pos = line1.find(t, pos)
        a.append(pos)
        pos += len(t)
    s: [str] = list(' ' * len(line1))
    for i, v in enumerate(a):
        for j, d in enumerate(str(i)):
            s[v + j] = d
    return "".join(s)


# compare all corresponding floats in two lines, use an absolute and relative error
def comp_line(lineno: int, line1: str, line2: str, aerr: float, rerr: float, status: str) -> tuple:
    global print_status_line

    l1 = sanitize(line1)
    l2 = sanitize(line2)

    if len(l1) == 0 and len(l2) == 0:
        return 0, 0.0, 0.0

    l1arr = l1.split()
    l2arr = l2.split()

    # only keep floats on the list
    l1arr = [i for i in l1arr if is_float(i)]
    l2arr = [i for i in l2arr if is_float(i)]

    if len(l1arr) != len(l2arr):
        return -1, 0.0, 0.0   # danger

    err: int = 0
    first: bool = True
    max_re: float = 0.0
    max_ae: float = 0.0
    for i, (t1, t2) in enumerate(zip(l1arr, l2arr)):
        # print(t1, t2)
        if is_float(t1) and is_float(t2):
            f1 = float(t1)
            f2 = float(t2)
            if abs(f1 - f2) >= aerr + rerr * abs(f2):
                if first:
                    if print_status_line:
                        print(status)
                        print_status_line = False
                    print(f"\n    {' ' * len(str(lineno))}Field # {pos_line(line1, l1arr)}")
                    print(f"Line {lineno}: (1) '{line1.rstrip()}'")
                    print(f"     {' ' * len(str(lineno))}  (2) '{line2.rstrip()}'")
                    first = False
                if f2 != 0:
                    re = round((abs(f1 - f2) / f2), 5)
                    max_re = max(max_re, re)
                else:
                    re = '<cannot compute>'
                ae = round(abs(f1 - f2), 5)
                max_ae = max(max_ae, ae)
                print(f"  Field #{i}: {f1} (1) <-> {f2} (2) abserr: {ae}, relerr: {re}")
                err += 1
    return err, max_ae, max_re


# fdiff: compare two files line by line, field by field, only process fields that are floats, ignore the rest
# assumptions:
#    the files are ascii
#    the files have the same overall structure
#    the files have the same number of lines
#    the corresponding lines have the same number of fields
#    the corresponding lines might have numerical differences in their fields
#
#    corresponding float fields of two lines are compared and produce an error if:
#           abs(field1 - field2) >= aerr + rerr * abs(field2)
#               aerr : absolute error, default  1e-8
#               rerr : relative error, default  1e-5  (.001 percent)
#
#    args: the files to compare, absolute and relative error
#    return: tuple (# of errors, max abs error, max rel error)
def cmp_file(file1: any, file2: any, aerr: float = 1e-8, rerr: float = 1e-5, status: str = None, nlines: int = -1, nerrorlines: int = -1) -> tuple:
    errors: int = 0
    error_lines: int = 0
    max_aerr: float = 0.0
    max_rerr: float = 0.0
    if nlines == -1:
        nlines = sys.maxsize
    if nerrorlines == -1:
        nerrorlines = sys.maxsize
    with open(file1, 'r') as f1, open(file2, 'r') as f2:
        for lineno, (l1, l2) in enumerate(zip(f1, f2)):
            if ("MODULAR" in l1) and ("MODULAR" in l2):
                continue
            err, max_a, max_r = comp_line(lineno, l1, l2, aerr, rerr, status)
            errors += err
            max_aerr = max(max_aerr, max_a)
            max_rerr = max(max_rerr, max_r)

            if err > 0:
                error_lines += 1
            if lineno >= nlines:
                break
            if error_lines >= nerrorlines:
                break

    return errors, max_aerr, max_rerr


def copy_data(from_dir: str, to_dir: str) -> None:
    shutil.copytree(from_dir, to_dir, dirs_exist_ok=True)


def run_swat(swat_model: str, wdir: str) -> int:
    if not os.path.exists(swat_model):
        raise Exception(f'Model not found: {swat_model}')
    p = Popen(executable=swat_model, args=[], cwd=wdir, stdout=sys.stdout, stderr=sys.stdout)
    p.wait()
    if p.returncode != 0:
        print(f"\nSWAT+ exited with code: {p.returncode}")
    else:
        print(f"\nCreated new output in {wdir}")
    return p.returncode


def comp_scenario(dir1: str, dir2: str, aerr: float, rerr: float, files: list[str], nlines: int, nerrorlines: int) -> int:
    total_err: int = 0
    global print_status_line
    for file in files:
        status = f"Processing '{file}' in \n  (1) {dir1} and \n  (2) {dir2}"
        print_status_line = True
        err, max_ae, max_re = cmp_file(os.path.join(dir1, file), os.path.join(dir2, file),
                                       aerr=aerr, rerr=rerr, status=status, nlines=nlines, nerrorlines=nerrorlines)
        total_err += err
        if err>0:
            print(f"\nResults for '{file}': #err = {err}, max aerr: {max_ae}, max rerr: {max_re}\n\n####")
    return total_err


def ctest_all(swat_model: str, test_data_dir: str, tmp_dir: str, aerr: float, rerr: float, nlines:int, nerrorlines:int) -> int:
    scenario_dir: str = os.path.join(tmp_dir, pathlib.PurePath(test_data_dir).name)
    pathlib.Path(tmp_dir).mkdir(parents=True, exist_ok=True)

    # 1. copy data from data dir to scenario dir
    copy_data(test_data_dir, scenario_dir)

    # 2. run swat+ in the scenario dir
    run_swat(swat_model, scenario_dir)

    # 3. compare the selected output files
    # files = ['hru_ls_aa.txt','mgt_out.txt', 'hru_totc.txt', 'basin_totc.txt', 'basin_wb_aa.txt']
    test_files: str = os.path.join(scenario_dir, '.testfiles.txt')
    errors: int = 0
    if os.path.isfile(test_files):
        with open(test_files) as f:
            files = [line.strip() for line in f if line.strip() != '' and not line.strip().startswith('#')]
            errors = comp_scenario(test_data_dir, scenario_dir, aerr, rerr, files, nlines=nlines, nerrorlines=nerrorlines)
    else:
        raise Exception(f'Not found: \'{test_files}\', cannot compare output files.')
    return errors


def get_next_run_number(dir: str) -> str:
    files = os.listdir(dir)
    if not files:
        return '01'
    else:
        files = sorted(files)
        last = files[-1]
        n = last.split('#')
        no = int(n[0]) + 1
        return f'{no:02d}'


def find_run(dir: str, no: int) -> str | None:
    pref =  f'{no:02d}'
    f = glob.glob(f'{dir}/{pref}#*')
    if f:
        if f[0].endswith("-FAILED"):
            raise Exception(f'Cannot compare a failed run: \'{f[0]}\'')
        return f[0]
    return None



def run(args):
    cwd = os.getcwd()
    sce_src_path = os.path.join(cwd, data_dir, args.scenario)
    if not os.path.exists(sce_src_path):
        raise Exception(f'path not found: {sce_src_path}')

    sce_run_path = os.path.join(cwd, runs_dir, args.scenario)
    exe_path = os.path.join(cwd, build_dir, args.executable)

    time = datetime.now().strftime('%Y-%m-%dT%H:%M:%S')
    os.makedirs(sce_run_path, exist_ok=True)
    next_no = get_next_run_number(sce_run_path)

    new_run = f'{next_no}#{time}#{args.executable}'

    run_path = os.path.join(sce_run_path, new_run)
    print(f"new run: '{run_path}'")

    # create folder
    os.makedirs(run_path, exist_ok=True)

    # copy scenario
    copy_data(sce_src_path, run_path)

    # run swat
    ret = run_swat(exe_path, run_path)

    if ret != 0:
        os.rename(run_path, run_path + '-FAILED')


def cmp(args):
    cwd = os.getcwd()
    sce_src_path = os.path.join(cwd, data_dir, args.scenario)
    sce_run_path = os.path.join(cwd, runs_dir, args.scenario)

    if not os.path.exists(sce_src_path):
        raise Exception(f'path not found: {sce_src_path}')
    if not os.path.exists(sce_run_path):
        raise Exception(f'path not found: {sce_run_path}')

    a = find_run(sce_run_path, args.a)
    b = find_run(sce_run_path, args.b)

    if a is None:
        raise Exception(f'Invalid run number {args.a} for scenario {args.scenario}')
    if b is None:
        raise Exception(f'Invalid run number {args.b} for scenario {args.scenario}')

    test_files: str = os.path.join(sce_src_path, '.testfiles.txt')
    if os.path.isfile(test_files):
        with open(test_files) as f:
            files = [line.strip() for line in f if line.strip() != '' and not line.strip().startswith('#')]
            err = comp_scenario(a, b, args.abserr, args.relerr, files, args.nlines, args.nerrorlines)
            if err > 0:
                print(f'\n\nTotal: {err} differences with relerr of >= {args.relerr} and abserr >= {args.abserr}')


def ctest(args):
    if not os.path.exists(args.model_path):
        raise Exception(f'model not found: {args.model_path}')
    if not os.path.isdir(args.data_dir):
        raise Exception(f'data dir not found: {args.data_dir}')

    err: int = ctest_all(args.model_path, args.data_dir, args.tmp_dir, args.abserr, args.relerr, args.nlines, args.nerrorlines)
    if err > 0:
        print(f'\nTotal: {err} differences with relerr of >= {args.relerr} and abserr >= {args.abserr}')
        sys.exit(1)


# 'run' with (scenario, exe)  name 1_executablename_date
# 'compare' (scenario, no1, no2,  aerr, rerr)

build_dir = "build"
data_dir = "data"
runs_dir = "runs"

# Not the best implementation, not thread safe. flag to print the comparison header
# when the first diff is recognized.
print_status_line = True

if __name__ == "__main__":
    parser = argparse.ArgumentParser("spcheck", description = 'Utilities for running SWAT+ scenarios')
    subparsers = parser.add_subparsers()

    run_parser = subparsers.add_parser('run', help='run a scenario')
    run_parser.add_argument("executable", type=str, help="name of the executable")
    run_parser.add_argument("scenario", type=str, help="scenario name")
    run_parser.set_defaults(func=run)

    cmp_parser = subparsers.add_parser('compare', help='compares two runs of the same scenarios')
    cmp_parser.add_argument("scenario", type=str, help="scenario name")
    cmp_parser.add_argument("a", type=int, help="first scenario run")
    cmp_parser.add_argument("b", type=int, help="second scenario run")
    cmp_parser.add_argument("--abserr", type=float, default=1e-8, help="absolute error, default: %(default)s")
    cmp_parser.add_argument("--relerr", type=float, default=0.05, help="relative error, default: %(default)s")
    cmp_parser.add_argument("--nlines", type=int, default=-1, help="max # of lines checked per file, default: %(default)s (=unlimited)")
    cmp_parser.add_argument("--nerrorlines", type=int, default=-1, help="max # of error lines reported per file, default: %(default)s (=unlimited)")
    cmp_parser.set_defaults(func=cmp)

    ctest_parser = subparsers.add_parser('ctest', help='compare new scenario against golden one')
    ctest_parser.add_argument("model_path", type=str, help="scenario name")
    ctest_parser.add_argument("data_dir", type=str, help="golden scenario")
    ctest_parser.add_argument("tmp_dir", type=str, help="new scenario")
    ctest_parser.add_argument("--abserr", type=float, default=1e-8, help="absolute error, default: %(default)s")
    ctest_parser.add_argument("--relerr", type=float, default=0.05, help="relative error, default: %(default)s")
    ctest_parser.add_argument("--nlines", type=int, default=-1, help="max # of lines checked per file, default: %(default)s (=unlimited)")
    ctest_parser.add_argument("--nerrorlines", type=int, default=-1, help="max # of error lines reported per file, default: %(default)s (=unlimited)")
    ctest_parser.set_defaults(func=ctest)

    if len(sys.argv) < 2:
        parser.print_help()
        sys.exit(0)

    args = parser.parse_args()
    args.func(args)

