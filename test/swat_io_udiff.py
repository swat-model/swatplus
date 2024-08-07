# Author fgeter@colostate.edu
# This script reads swat input and output files and outputs
# two diff files for each file where the difference is between 
# a swat output reference file and a current corresponding swat output file.
# If there is no difference between files, no diff outputs will 
# be generated for that file.
# 
# If there is a difference between the files, then only lines 
# where the difference occured will be output in the diff files.
# The diff files will be output to an abosulte path given as the first 
# argument on the command line. If this path does not exist, then ths
# script will attempt to create it. In addition, a subdirectory called 
# "diff_files" will be created under this absolute path to store the diff files.
# There are two types of diff files that are output.  The first is
# is a the raw output from the python function difflib.unified_diff. 
# The second is generated by processing the difflib.unified_diff output
# to show on each line the line number in the original files where there is a 
# difference along with the actual line itself followed by a line showing 
# where in the line changes have occurred.  The change is indicated be the
# the character "^".  If this line begins with "dc", then there appears to be 
# only changes in data in the swat output columns.  
# If it begins with "fc", it appears there is a swat 
# output format change between the current and reference file. The "fc" is 
# output if the number of swat columns for the line in question appears to have
# changed.   
#
# This script is called as follows:
# python swat_io_udiff.py [full_path_to_tmp_folder_for_diff_output] [full_path_to_reference_folder_location] [full_path_to_current_run_folder_location]
# If the reference path and current path are a file and not a folder, 
# then only that file will be processed.
#
# IMPORTANT: If a file to be compared is specified on the command line, the corresponding diff
#            file will be deleted before a new one is created of the same name. 
#            If a folder to be compared is specified on the command line, 
#            all the diff files in the corresponding diff folder will be
#            deleted before new ones are created.


import difflib
import filecmp
import sys
import os
import glob
import subprocess
import shutil

from datetime import datetime


def create_test_io_folders():
    test_build_folder = os.path.normpath(os.path.abspath(sys.argv[1]))
    swat_exe = sys.argv[2]
    reference_data_folder = os.path.normpath(os.path.abspath(sys.argv[3]))
    # test_build_folder = os.path.normpath("/home/fgeter/csu-scripts/sp1/build/") 
    # swat_exe = "swatplus_60_5_7.exe"
    # reference_data_folder = os.path.normpath("/home/fgeter/csu-scripts/sp1/data/Ithaca_sub6/") 
    if not os.path.exists(test_build_folder):
        print(f"The full path to build folder {test_build_folder} either")
        print("does not exist or the path has incorrect permissions.")
        exit(1)

    if not os.path.exists(reference_data_folder):
        print(f"The full path to reference data folder {reference_data_folder} either") 
        print("does not exist or the path has incorrect permissions.")
        exit(1)

    if os.path.isdir(reference_data_folder) == False:
        print(f"The full path to reference data folder {reference_data_folder} is ") 
        print("is a file and not folder. It must be folder.")
        exit(1)

    # Create the test data folder if it does not exist
    test_data_folder = os.path.join(test_build_folder, "data")
    try:
        os.makedirs(test_data_folder, mode=0o766, exist_ok=True)
    except Exception as e:
        print(f"Could not create test data folder {test_data_folder}.")
        print(f"Python error is:\n{e}")
        exit(1)

    # Open log file in test data folder
    log_filename =  os.path.basename(reference_data_folder) + "_diff.log"
    log_path_name = os.path.join(test_data_folder, log_filename) 
    try:
        log = open(log_path_name, "w")
    except Exception as e:
        print(f"Could not create log file. Python error is\n{e}")
        exit(1)
    now = datetime.now()
    log.write(f"Start of test processing at {now}\n\n")
    print(f"See log file at {log_filename} for test processing info.")
    return test_build_folder, swat_exe, reference_data_folder, test_data_folder, log 

def copy_data_folder(reference_data_folder, test_data_folder, log):
    log.write("Copying reference data to test data folder.\n")
    foldername = os.path.basename(reference_data_folder)
    swat_data_folder = os.path.join(test_data_folder, foldername)
    if os.path.exists(swat_data_folder):
        log.write(f"Removing previous test run folder {swat_data_folder}.\n")
        try:
            shutil.rmtree(swat_data_folder)
        except Exception as e:
            message = f"Could not remove old swat test data folder {test_data_folder}"\
                    + f"Python errer is :\n{e}"
            print(message)
            log.write(f"{message}\n")
    try:
        shutil.copytree(reference_data_folder, swat_data_folder)
    except Exception as e:
        message = f"Could not copy test data folder {reference_data_folder} to {test_data_folder}"\
                + f"Python errer is :\n{e}"
        print(message)
        log.write(f"{message}\n")
        exit(1)
    return swat_data_folder
    

def swat_run_check(test_build_folder, swat_exe, swat_data_folder, log):
    log.write("Executing swatplus.\n")
    swatplus_executable = os.path.join(test_build_folder, swat_exe)
    os.chdir(swat_data_folder)
    result = subprocess.check_call([swatplus_executable])
    if result == 0:
        message = f"swatplus execution succeded."
        log.write(f"{message}\n")
    else:
        message = f"swatplus execution failed."
        log.write(f"{message}\n")
        exit(1)
    return


def create_diff_folder(swat_data_folder, log):
    diff_folder_name = os.path.basename(swat_data_folder) + "_diff_files"
    diff_folder_path = os.path.dirname(swat_data_folder)
    diff_folder_path = os.path.join(diff_folder_path, diff_folder_name)
    if os.path.exists(diff_folder_path):
        log.write(f"Attempting to removing previous diff folder {diff_folder_path}.\n")
        try:
            shutil.rmtree(diff_folder_path)
        except Exception as e:
            message = f"Could not remove previous diff folder {diff_folder_path}." +\
                    + f"Python errer is :\n{e}"
            print(message)
            log.write(f"{message}\n")
            exit(1)
        log.write(f"Removed previous diff folder {diff_folder_path}.\n")
    log.write("Creating diff folder.\n")
    try:
        os.makedirs(diff_folder_path, mode=0o766, exist_ok=True)
    except Exception as e:
        message = f"Could not create test data folder {diff_folder_path}." + \
                  f"Python error is:\n{e}"
        log.write(f"{message}\n")
        exit(1)
    log.write(f"Created diff folder {diff_folder_path}.\n")
    return diff_folder_path



def file_comparison_list(reference_data_folder, swat_data_folder, diff_folder_path, log):
    # Make a list tuples where each tuple has four items:
    # 1. The full path to the reference swat output file to be compared to.
    # 2. The full path to the current swat output file to be compared with the reference file.
    # 3. The full path to the location to store the output of of the unified_diff.
    # 4. The full path to the location to store the post process output of unified_diff output.

    log.write("Start of creating a list of files to compare and the names of their diff output files\n")
    cf_isfile = False
    cf_isdir = False
    compare_files = []

    # 1st case, if a file (not a folder) is specified on the command line to compared.
    if os.path.isfile(swat_data_folder):
        cf_file = os.path.basename(swat_data_folder)
        rf_file = os.path.basename(reference_data_folder)
        diff_file1 = cf_file + "_diff1"
        diff_file2 = rf_file + "_diff2"
        diff_path1 = os.path.join(diff_folder_path, diff_file1)
        diff_path2 = os.path.join(diff_folder_path, diff_file2)
        cf_path =swat_data_folder
        rf_path =reference_data_folder
        item = (rf_path, cf_path, diff_path1, diff_path2)
        log.write(f"Adding {os.path.basename(item[1])}.\n")
        compare_files = compare_files + [item]
    # 2nd case, if a folder is specified on the command line to compared.
    else:
        for dirName, subdirList, fileList in os.walk(swat_data_folder):
            for fname in fileList:
                # if the folder is not subdirectory of the folder
                # specified on the commandline.
                if dirName == swat_data_folder:
                    cf_path = os.path.join(dirName, fname)
                    rf_path = os.path.join(dirName.replace(dirName, reference_data_folder), fname)
                    diff_path1 = os.path.join(diff_folder_path, fname+"_diff1")
                    diff_path2 = os.path.join(diff_folder_path, fname+"_diff2")
                    item = (rf_path, cf_path, diff_path1, diff_path2)
                    log.write(f"Adding {os.path.basename(item[1])}.\n")
                    compare_files = compare_files + [item]
                # if the folder is a subdirectory of the folder
                # specified on the commandline.
                else:
                    subdir = os.path.basename(dirName.replace(swat_data_folder, ""))
                    cf_path = os.path.join(swat_data_folder, subdir, fname)
                    rf_path = os.path.join(reference_data_folder, subdir, fname)
                    diff_path1 = os.path.join(diff_folder_path, subdir, fname+"_diff1")
                    diff_path2 = os.path.join(diff_folder_path, subdir, fname+"_diff2")
                    item = (rf_path, cf_path, diff_path1, diff_path2)
                    log.write(f"Adding {os.path.basename(item[1])}.\n")
                    compare_files = compare_files + [item]
    log.write("End of creating a list of files to compare and there diff output files\n\n")
    return(compare_files)


def is_binary(file_name):
    # This is a function to check and see if the file is a binary file.
    # Returns False if it is a binary file and True if it is not a binary file.
    try:
        with open(file_name, 'tr') as check_file:  # try open file in text mode
            check_file.read()
            return False
    except:  # if fail then file is non-text (binary)
        return True


def create_diff_files(reference_data_folder, swat_data_folder, compare_files, log):
    log.write("Start of creating diff files\n")
    # Create the diff files
    files_with_changed_format = [] 
    for item in compare_files:
        rf_file_path = item[0]
        cf_file_path = item[1]
        if is_binary(cf_file_path) or is_binary(rf_file_path):
            log.write(f"\nEither or both of the files:\n")
            log.write(f"\t{rf_file_path}\n")
            log.write(f"\t{cf_file_path}\n")
            log.write("appear to be binary files. Skipping this file comparison.\n\n")
            continue
        if filecmp.cmp(rf_file_path, cf_file_path) == True:
            log.write(f"No difference between files {rf_file_path} and {cf_file_path}\n")
        else:
            rf_file_object = open(rf_file_path, "r")
            rf_contents = rf_file_object.readlines()
            cf_file_object = open(cf_file_path, "r")
            cf_contents = cf_file_object.readlines()
            log.write(f"Doing a unified diff between: {rf_file_path} and {cf_file_path}\n")
            udiff = difflib.unified_diff(rf_contents, cf_contents, n=0, fromfile=reference_data_folder, tofile=swat_data_folder)
            diff1_filename = item[2]
            diff2_filename = item[3]
            if os.path.exists(os.path.dirname(diff1_filename)) == False:
                os.makedirs(os.path.dirname(diff1_filename))
            diff1_output_file_object = open(diff1_filename, "w")

            for line in udiff:
                diff1_output_file_object.write(line)
            diff1_output_file_object.close()

            diff1_output_file_object = open(diff1_filename, "r")
            diff1_contents = diff1_output_file_object.readlines()
            diff2_output_file_object = open(diff2_filename, "w")
            log.write(f"Processing unified diff output for : {diff1_filename} and {diff2_filename}\n")
            line_dict = {}
            file_format_change = False
            for line in diff1_contents:
                if line[0:3] in ["---", "+++"]:
                    diff2_output_file_object.write(line)
                    continue
                if line[0:2] == "@@":
                    line_list= line.split()
                    delete_info = line_list[1].split(",")
                    delete_line = -int(delete_info[0])
                    if len(delete_info) == 1:
                        delete_num = 1
                    else:
                        delete_num = int(delete_info[1])
                    add_info = line_list[2].split(",")
                    add_line = int(add_info[0])
                    if len(add_info) == 1:
                        add_num = 1
                    else:
                        add_num = int(add_info[1])
                elif line[0] == "-":
                    line_out = str(delete_line) + " " + line
                    line_out_key = str(delete_line).zfill(12) + "_0"
                    line_dict[line_out_key] = line_out
                    delete_line = delete_line + 1
                    delete_num = delete_num -1
                    if delete_num == 0:
                        continue
                elif line[0] == "+":
                    line_out = str(add_line) + " " + line
                    line_out_key = str(add_line).zfill(12) + "_1"
                    line_dict[line_out_key] = line_out
                    add_line = add_line + 1
                    add_num = add_num - 1
                    if add_num == 0:
                        continue
            output_keys = list(line_dict.keys())
            same_line_list = []
            for x in sorted(output_keys):
                cur_line = line_dict[x]
                diff2_output_file_object.write(cur_line)
                if len(same_line_list) < 2:
                    same_line_list.extend([line_dict[x]])
                if len(same_line_list) == 2:
                    l1 = same_line_list[0]
                    l2 = same_line_list[1]
                    format_change = False
                    if len(l1.split()) != len(l2.split()):
                        format_change = True
                        file_format_change = True
                    len1 = len(l1)
                    len2 = len(l2)
                    maxl = max(len1, len2)
                    change_line = ""
                    for c in range(0,maxl):
                        if c < len1 and c < len2:
                            if l1[c] == l2[c] or (l1[c] == "-" and l2[c] == "+"):
                                change_line = change_line + " "
                            else:
                                change_line = change_line + "^"
                        else:
                            change_line = change_line + "^"
                    if format_change:
                        change_line = change_line.replace("  ", "fc", 1)
                    else:
                        change_line = change_line.replace("  ", "dc", 1)
                    diff2_output_file_object.write(change_line + "\n")
                    same_line_list = []
            if file_format_change:
                files_with_changed_format.extend([cf_file_path])
    if len(files_with_changed_format) > 0:
        log.write("\nWarning: There are files that appear to have changed output format:\n")
        for f in files_with_changed_format:
            log.write(f"\t{f}\n")
    log.write("End of creating diff files\n\n")
    return(files_with_changed_format)


def run():
    test_build_folder, swat_exe, reference_data_folder, test_data_folder, log = create_test_io_folders()
    swat_data_folder = copy_data_folder(reference_data_folder, test_data_folder, log)
    swat_run_check(test_build_folder, swat_exe, swat_data_folder, log)
    diff_folder_path =  create_diff_folder(swat_data_folder, log)
    # reference_data_folder, swat_data_folder, diff_folder, log = file_folder_prep()
    comp_files = file_comparison_list(reference_data_folder, swat_data_folder, diff_folder_path, log)
    # delete_old_files(reference_data_folder, comp_files, log)
    files_with_changed_format = create_diff_files(reference_data_folder, swat_data_folder, comp_files, log)
    now = datetime.now()
    log.write(f"End of diff processing at {now}\n\n")
    log.close()
    return files_with_changed_format


if __name__ == "__main__":
    files_with_changed_format = run()
