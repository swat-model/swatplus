# Author, fgeter@colostate.edu
# This is generic swat_plus test script.
# arg 1: absolute path to the swat executable.
# arg 2: absolute path to the reference scenario (golden)
# arg 3: absolute path to the test scenario base for swat+ execution
#
# Example:
#   python check.py <dir>/sp1/build/swatplus.exe <dir>/sp1/data/Ames_sub1 <dir>/sp1/build/data
#
# swat outputs will be found in the test data folder + scenario, e.g. <dir>/sp1/build/data/Ames_sub1.
import os
import pathlib
import shutil
import sys
from subprocess import Popen, PIPE


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
def compare_line(lineno: int, line1: str, line2: str, aerr: float, rerr: float) -> tuple:
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
                print(f"  Field #{i}: {f1} (1) <-> {f2} (2) aerr: {ae}, rerr: {re}")
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
def fdiff(file1: any, file2: any, aerr: float = 1e-8, rerr: float = 1e-5) -> tuple:
    errors: int = 0
    max_aerr: float = 0.0
    max_rerr: float = 0.0
    with open(file1, 'r') as f1, open(file2, 'r') as f2:
        for lineno, (l1, l2) in enumerate(zip(f1, f2)):
            if ("MODULAR" in l1) and ("MODULAR" in l2):
                continue
            err, max_a, max_r = compare_line(lineno, l1, l2, aerr, rerr)
            errors += err
            max_aerr = max(max_aerr, max_a)
            max_rerr = max(max_rerr, max_r)
    return errors, max_aerr, max_rerr


# ---------

def copy_data(from_dir: str, to_dir: str) -> None:
    shutil.copytree(from_dir, to_dir, dirs_exist_ok=True)


def run_swat(swat_model: str, wdir: str) -> int:
    p = Popen([swat_model], cwd=wdir, stdin=PIPE, stdout=PIPE, stderr=PIPE)
    stdout, stderr = p.communicate()
    print(stdout.decode())
    print(stderr.decode())
    if p.returncode != 0:
        print(f"SWAT+ exited with code: {p.returncode}")
        exit(1)
    return p.returncode


def check(dir1: str, dir2: str, aerr: float,  rerr: float, *files: list[str]) -> int:
    total_err: int = 0
    for file in files:
        print(f"Processing '{file}' in {dir1} (1) and {dir2} (2):")
        err, max_ae, max_re = fdiff(os.path.join(dir1, file), os.path.join(dir2, file), aerr=aerr, rerr=rerr)
        total_err += err
        print(f"\nResults for '{file}': {dir1} <-> {dir2}, #err = {err}, max aerr: {max_ae}, max rerr: {max_re}")
    return total_err


def test(swat_model: str, test_data_dir: str, tmp_dir: str, aerr: float, rerr: float) -> int:
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
            errors = check(test_data_dir, scenario_dir, aerr, rerr, *files)

    return errors


if __name__ == "__main__":
    if len(sys.argv) != 6:
        print(f"Usage: {sys.argv[0]} <swat_model_path> <data_dir> <tmp_dir> <aerr> <rerr>")
        exit(1)

    aerr: float = float(sys.argv[4])   # aerr: float = 1e-8  # absolute error.
    rerr: float = float(sys.argv[5])  # rerr: float = 0.05  # 5 % relative error threshold.

    err: int = test(sys.argv[1], sys.argv[2], sys.argv[3], aerr, rerr)
    if err > 0:
        print(f'\nTotal: {err} differences with rerr of >= {rerr} and aerr >= {aerr}')
        exit(1)
