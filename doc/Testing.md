## Scenario Testing



Running tests may reveal potential regressions of code changes against existing and assumed correct output. Here, we test the newly built SWAT+ model executable against known scenario outputs and compare the results. Results should not differ significantly after code changes. Each SWAT+ unit test consists of one reference scenario run, located in `data`. The repository's `test` directory contains the Python test scripts.

Testing is performed through `cmake` and `ctest` implementing the following process:

1. The `data` folder contains SWAT+ reference scenarios. Those scenarios have input and valid output data. The output data is considered `golden`, it means they represent an accepted correct output of the model.

2. The test related entries in `CMakelists.txt` are shown below. Python is required. Various variables are set to point to resources, such as the script that performs the check (`check.py`), the path to the executable, and paths to the reference data set and test data directory. Relative and Absolute tolerances for values deviations are specified via `rel_err` and `abs_err`. Per default, the relative error is 1% and the absolute error 1E-8. The individual tests are followed with the `add_test` commands.

3. Building the project with `cmake -B build`. Two tests are being generated, `Ames_sub1` and `Ithaca_sub6`. Additional tests will need extra `add_test` lines, one for each scenario.

4. Next, the swat executable should be created. See sections below.

5. The tests can now be performed using `ctest`.

6. When executing a single test, e.g. `Ames_sub1`, `check.py`is called and it performs the following steps:
   
   1. The scenario folder `Ames_sub1` is copied from the data directory to the build folder as `build/Ames_sub1)`
   
   2. The swat model is executed in the `build/Ames_sub1` folder and will overwrite previous outputs
   
   3. `check.py` will read the file `build/Ames_sub1/.testfiles.txt`, containing a list of output file names. It may contain the file name `wb.txt` and `soc.txt` on separate lines.
   
   4. Each output file name in this list will be processed and the files are compared such as
      
      `data/Ames_sub1/wb.txt` <-> `build/Ames_sub1/wb.txt`, and
      
      `data/Ames_sub1/soc.txt` <-> `build/Ames_sub1/soc.txt`
   
   5. For each pair of files, only floating point values are compared that occur on the same line and column. If the difference of two floats (float1 and float2) exceeds abs_error and relative error, it is recognized as failure and the error is captured in the test output. float1 and float2 are considered equal if:
      
      $$
      abs(float1 - float2) <= abs\_err + rel\_err * abs(float2)
      $$

7. The test results are summarized after each scenario and the number of values that differ and the maximum relative and absolute error are printed as test result. A scenario test fails if one pair of floats are not equal according to the equation above.

```cmake
#############################################################################
# Testing
find_package(Python REQUIRED)

set(check_py "${PROJECT_SOURCE_DIR}/test/check.py")
set(exe_path "${PROJECT_BINARY_DIR}/${SWATPLUS_EXE}")
set(test_dir "${PROJECT_BINARY_DIR}/data")
set(ref_dir "${PROJECT_SOURCE_DIR}/data")

# error tolerances
set(rel_err "0.01")
set(abs_err "1e-8")

add_test(Ithaca_sub6 python3 ${check_py} ${exe_path} ${ref_dir}/Ithaca_sub6 ${test_dir} ${abs_err} ${rel_err})
add_test(Ames_sub1   python3 ${check_py} ${exe_path} ${ref_dir}/Ames_sub1   ${test_dir} ${abs_err} ${rel_err})
```

Tests are run using the `ctest` command, which is part if the `cmake` installation. Alternatively, you can use the command `make test` within the `build` directory.

```bash
$ cmake -B build                # -> Generate the build files
$ cmake --build build           # -> Build SWAT+ executable

$ cd build                      # -> change into the build folder
$ ctest                         # -> Test all scenarios using ctest
```

`ctest` provides a test summary as output. Somewhere in `build/Testing`, there is also the detailed standard output of `ctest` for each test. Also, you can run all tests, a selected subset of test, or just an individual one. `ctest` is quite powerful and flexible.
