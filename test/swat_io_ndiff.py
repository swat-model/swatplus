# Author fgeter@colostate.edu
# reference swat input/output folder and a current 
# swat run input/output folder.  
# This script outputs a diff file for each file where
# there is difference between the reference file and 
# the current run file.  If there is no difference,
# no diff output will be generated for that file.
# The diff files will be output to a swat root folder in
# a tmp directory.
# This script called as follows:
# python swat_io_diff.py full_path_to_tmp_folder_for_diff_output full_path_to_reference_folder_location full_path_to_current_run_folder_location 
# 
# IMPORTANT: The tmp folder contents will be removed automatically to remove any previous diff files.

import difflib
import filecmp
import sys
import os
import glob


executable = os.path.basename(os.path.normpath(sys.argv[0]))
# if len(sys.argv) != 3:
#     print(f"{executable} requires three arguments on the command line:")
#     print("\tfull_path_to_tmp_folder_for_diff_output")
#     print("\tfull_path_to_reference_folder_location")
#     print("\tfull_path_to_current_run_folder_location")
#     exit(1)

# tmp_dir_location = os.path.normpath(sys.argv[1])
# rf_location = os.path.normpath(sys.argv[2])
# cf_location = os.path.normpath(sys.argv[3])

tmp_dir_location = os.path.normpath("/home/fgeter/csu-scripts/sp_cmake/sp/sp/tmp/")
rf_location = os.path.normpath("/home/fgeter/csu-scripts/sp_cmake/sp/sp/ref_data/Ithaca_sub6/")
cf_location = os.path.normpath("/home/fgeter/csu-scripts/sp_cmake/sp/sp/test_data/Ithaca_sub6/")

diff_folder = os.path.join(tmp_dir_location, "diff_files")
sig_diff_folder = os.path.join(tmp_dir_location, "sig_diff_files")

try:
    os.makedirs(tmp_dir_location, mode=0o766, exist_ok=True)
except Exception as e:
    print(f"Could create or access temporary file location {tmp_dir_location}.  Python error is:")
    print(e)
    exit(1)

if not os.path.exists(rf_location):
    print(f"The full path to the reference folder {rf_location} either does not exist or the path has incorrect permissions.")
    exit(1)

if not os.path.exists(cf_location):
    print(f"The full path to the current run folder {cf_location} either does not exist or has incorrect permissions.")
    exit(1)

try:
    os.makedirs(diff_folder, mode=0o766, exist_ok=True)
except Exception as e:
    print(f"Could create or access diff file location {diff_folder}.  Python error is:")
    print(e)
    exit(1)

try:
    os.makedirs(sig_diff_folder, mode=0o766, exist_ok=True)
except Exception as e:
    print(f"Could create or access significant diff file location {sig_diff_folder}.  Python error is:")
    print(e)
    exit(1)

df_folder_files = glob.glob(os.path.join(diff_folder, "*"))
for file in df_folder_files:
    try:
        os.remove(file)
    except Exception as e:
        print(f"Warning: Could not remove old diff file {file} in {diff_folder}.  Python error is:")
        print(e)
        continue

sig_diff_folder_files = glob.glob(os.path.join(sig_diff_folder, "*"))
for file in sig_diff_folder_files:
    try:
        os.remove(file)
    except Exception as e:
        print(f"Warning: Could not remove old signifcant diff file {file} in {sig_diff_folder}.  Python error is:")
        print(e)
        continue

filecmp.clear_cache()
dir_list = os.listdir(cf_location)
cmp_result = filecmp.cmpfiles(rf_location, cf_location, dir_list, shallow=True)[1]
for file in cmp_result:

    full_path1 = os.path.join(rf_location, file)
    if os.path.isdir(full_path1):
        continue
    f1_object = open(full_path1, "r")
    file1_contents = f1_object.readlines()
    
    full_path2 = os.path.join(cf_location, file)
    f2_object = open(full_path2, "r")
    file2_contents = f2_object.readlines()
    print(full_path1, full_path2)

    # diff = difflib.ndiff(file1_contents, file2_contents)
    diff = difflib.unified_diff(file1_contents, file2_contents, n=0)
    output_filename =  file + "_diff"
    output_full_path = os.path.join(diff_folder, output_filename)
    print(output_full_path)
    output_file_object = open(output_full_path, "w")
    for line in diff:
        output_file_object.write(line)
    output_file_object.close()
    
    


    # print(l)
    # print()

# fname1 = "reference_file.txt"
# fname2 = "new_file.txt"
# if not filecmp.cmp(fname1, fname2):
#     f1 = open(fname1, "r")
#     file1 = f1.readlines()
#     f2 = open(fname2, "r")
#     file2 = f2.readlines()
#     diff = difflib.ndiff(file1, file2)
#     output_filename =  fname2.split(".")[0] + "_diff.txt"
#     output_file_object = open(output_filename, "w")
#     for line in diff:
#         # line = line.replace("\n", "")
#         # print(line)
#         output_file_object.write(line)
#     f1.close()
#     f2.close()
#     output_file_object.close()
#     print(f"Output file {fname2} has changed. See {output_filename} for the changes.")


