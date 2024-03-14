# SWAT+ on GitHub

This file summarized the concepts, approaches, and methods for managing SWAT+ on GitHub.  Snippets from the project's `CMakeLists.txt` are explained. 

The GitHub repository is setup to build, test, and deploy SWAT+ using CMake. CMake is a cross-platform build tool, it can be used at the command line but it is also supported through various IDEs, etc. More information can be found at [http://cmake.org](http://cmake.org). In addition to CMake, the following tools are also needed:

- `git` tool for version control
- `make` tool (for building)
- `gfortran` or `ifort/ifx` compiler and linker (for compiling/linking)
- `python3` (for testing, optional)

Use the operating system's preferred way of adding those tools to your installation. There is certainly more than one way of getting and installing them.

## Get the Sources

First, get the SWAT+ sources by either cloning the repository using `git` or downloading the zipped sources.

Get the repository from GitHub by cloning it.

```bash
$ git clone https://github.com/swat-model/swatplus
```

Or, download the sources directly from the artifacts, unzip.

```bash
$ wget https://github.com/swat-model/swatplus/archive/refs/tags/v61.zip
```

## Directory Structure

The directory structure is shown below. The `build` directory gets created and populated during the generation of the `cmake` files and the build.

```
swatplus
├── build
│   ├── ...
│   ├── *.mod
│   ├── Testing
│   └── CMakeFiles
│       ├── Makefile.cmake
│       ├── ...
│       └── swatplus.exe.dir
│           ├── *.mod.tstamp
│           ├── src
│           └── ...
├── data                      ---> contains all data sets for testing
│   ├── Ames_sub1
│   ├── Ithaca_sub6
│   └── ...
├── src                       ---> contains all swatplus Fortran source files
│   └── *.f90
├── test                      ---> contains all unit tests sources
│   ├── ...
│   └── check.py
├── CMakeLists.txt
├── Readme.md
└── ...
```

## Tagging

A new model version is directly derived from a tag in the `main` branch. There is no need to manually edit source files and name the executable.  That git tag name is directly projected into 1) `main.f90` and 1) the name of the executable. The file `CMakeLists.txt`  contains the git command to obtain the current tag for the current branch and derive the version from it.  The git command is ` git describe --tags`, which is returning either the current tag, or the most recent tag reachable from the commit as a the relative difference of number of commits and  the abbreviated object name. 

Use the `git log` or the `git tag -l` command to list and find the tags available. The output below shows two tags available for the repository

```bash
$ git log
commit 92b44138a3fdb0ada27ed86e2a3a378188f70aef (HEAD -> main, origin/main, origin/HEAD)
Author: od <odavid@colostate.edu>
Date:   Tue Mar 5 21:43:50 2024 -0700

    fixed 'typo'

commit c5ff4b1083c5b320c72d02ec8b728811bed4441a (tag: 61.1)
Author: od <odavid@colostate.edu>
Date:   Tue Mar 5 20:43:24 2024 -0700

    stop here not recommended (gcc)

commit 6e85302d89115d42df471e02280c4bfe2720f830
Author: od <odavid@colostate.edu>
Date:   Tue Mar 5 20:32:31 2024 -0700

    added Version tag

commit df07e3f572adc21834f05c6711df3566e0231c2f (tag: 61.0)
Author: od <odavid@colostate.edu>
Date:   Tue Mar 5 19:58:38 2024 -0700

    init all
$ git tag 
61.0
61.1
```

To build the SWAT+ executable at version `60.0`, one has to checkout the source at that tag first:

```bash
$ git checkout 60.0
```

Then follow the build instructions below. It will eventually create the executable `swatplus-60.0.exe`. You can also switch back to HEAD of the main branch and again, follow the build steps. This will create the executable  `swatplus-61.1-1-g92b4413.exe` which is the version one commit (`-1-`) after version `61.1`, starting with the object name: `92b4413......` (the `g` in the string indicates Git as the system that created the name).

This tag-based building approach has the advantage to:

- always align the version of the swat executable with the tagged version of the sources in the Git repository.

- allow the fully automated version propagation into source files  and executable in tandem with `cmake` .

- allow the creation of source archives and corresponding executables for a given tag at GitHub

```cmake
execute_process(
  COMMAND git describe --tags
  WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}
  OUTPUT_VARIABLE TAG
  OUTPUT_STRIP_TRAILING_WHITESPACE
)

# SWAT Version number
SET(SWAT_VERSION ${TAG})
set(SWATPLUS_EXE "swatplus-${SWAT_VERSION}.exe")
```

## Versioning

Version information are projected into sources at build time. Below, the template `main.f90.in` is shown.

```fortran
  prog = " SWAT+ @TODAY@        MODULAR Rev @YEAR@.@SWAT_VERSION@"
      write (*,1000)
      open (9003,file='simulation.out')
      write (9003,1000)
 1000 format(1x,"                  SWAT+               ",/,             &
     &          "             Revision @SWAT_VERSION@  ",/,             &
     &          "      Soil & Water Assessment Tool    ",/,             &
     &          "@CMAKE_Fortran_COMPILER_ID@ (@CMAKE_Fortran_COMPILER_VERSION@), @ISO@, @CMAKE_HOST_SYSTEM_NAME@",/,             &
     &          "    Program reading . . . executing",/)
  ...
```

The `main.f90` substitution in `CMakeLists.txt` shown below. The input file `main.f90.in` file gets processed by `cmake` when the project is build (`cmake -B build`) and the file `main.f90` is created with all `cmake` variables resolved to their values.

```cmake
if (EXISTS "${PROJECT_SOURCE_DIR}/src/main.f90.in")
    string (TIMESTAMP ISO   "%Y-%m-%d %H:%M:%S")
    string (TIMESTAMP TODAY "%b %d %Y")  # e.g. produces Dec 7 2023
    string (TIMESTAMP YEAR  "%Y")

    configure_file(
        "${PROJECT_SOURCE_DIR}/src/main.f90.in"
        "${PROJECT_SOURCE_DIR}/src/main.f90"
    )
endif()
```

Running `cmake -B build` will create the file `main.f90` with resolved and and injected variables for `TODAY`, `ISO`, `YEAR`, and others: 

```fortran
 prog = " SWAT+ Mar 04 2024        MODULAR Rev 2024.61.0.0"

      write (*,1000)
      open (9003,file='simulation.out')
      write (9003,1000)
 1000 format(1x,"                  SWAT+               ",/,             &
     &          "             Revision 61.0.0  ",/,             &
     &          "      Soil & Water Assessment Tool    ",/,             &
     &          "GNU (11.4.0), 2024-03-04 14:31:18, Linux",/,             &
     &          "    Program reading . . . executing",/)
  ...
```

> Note: The file `main.f90` should not be edited since it will be overwritten with each fresh build, modify `main.f90.in` instead. 

## Generate the Build files

SWAT+ will be built *out-of-source*, which means that all intermediate and final files are **not** entangled with the sources. This allows keeping the source tree clean, all generated resources by the build can be quickly removed. In addition, multiple SWAT+ build flavors can be build using the single source tree.

```cmake
file(GLOB sources src/*.f90)
add_executable(${SWATPLUS_EXE} ${sources})
```

Make the `swatplus` directory the current working directory. All command examples below assume this.

```bash
$ cd <dir>/swatplus
```

Now, create all build files. Change into the `swatplus` folder and issue one of the following commands to configure and generate the build files. This does not build SWAT+ yet. CMake will process the content of the file `CMakelist.txt` and will generate the build system.

The command below reads the `CMakelist.txt` source file from the current directory `.`, creates a directory `build` and stores all resources for the build in it.

```bash
$ cmake -S . -B build
  <or short>
$ cmake -B build
```

You can force `cmake` to use the GCC gfortran compiler.

```bash
$ cmake -B build -D CMAKE_Fortran_COMPILER=gfortran
```

If you want to use Intel Fortran to build SWAT+, configure as below. It defaults to `gfortran` otherwise.

The Intel `ifort` Fortran compiler will be deprecated in 2024 and is being replaced with the LLVM-based version, `ifx`. Therefore, the use of `ifx` is preferred.

```bash
$ cmake -B build -D CMAKE_Fortran_COMPILER=ifort
  <or better>
$ cmake -B build -D CMAKE_Fortran_COMPILER=ifx
```

If you want to create a `Debug` version of SWAT+ that includes symbols for debugging, configure as below. Use the `Release` flag for building a release version instead.

```bash
$ cmake -B build -D CMAKE_BUILD_TYPE=Debug
```

If you want to delete all files created by `cmake`, simply remove the build folder and start over, e.g. on Linux

```bash
$ rm -rf build
```

or using the Windows terminal:

```powershell
> rd /s /q build
```

## Build the SWAT+ Executable

To build the SWAT+ executable, use the `cmake --build` command in the `swatplus` directory. It will process the previously generated files in `build` and place all intermediate resources (`*.o`, `*.mod` files) into the build directory.

```bash
 $ cmake --build build
```

Use the `-j` option to compile the source files in parallel. The example below is using `8` CPU cores in parallel. This greatly speeds up the compile process.

```bash
 $ cmake --build build -j 8
```

You can find the SWAT+ executable in the build folder as `swatplus.exe`.

Call the build target `clean` to remove all generated object files (`.o`) and modules files (`.mod`). Alternatively, you can do (`cd build; make clean`).

```bash
 $ cmake --build build --target clean
```

Building the swat model is supported for various platforms and compilers as described in the `Readme.md` file.  contains the specific instructions. 

## Testing

Running tests may reveal potential regressions of code changes against existing and assumed correct output. Here, we test the newly built SWAT+ model executable against known scenario outputs and compare the results. Results should not differ significantly after code changes. Each SWAT+ unit test consists of one reference scenario run, located in `data`. The repository's `test` directory contains the Python test scripts.

Testing is performed through `cmake` and `ctest` implementing the following process:

1. The `data` folder contains SWAT+ reference scenarios. Those scenarios have input and valid output data. The output data is considered `golden`, it means they represent an accepted correct output of the model.   

2. The test related entries in `CMakelists.txt` are shown below. Python is required. Various variables are set to point to resources, such as the script that performs the check (`check.py`), the path to the executable, and paths to the reference data set and test data directory. Relative and Absolute tolerances for values deviations are specified via `rel_err` and `abs_err`.  Per default, the relative error is 1% and the absolute error 1E-8. The individual tests are followed with the `add_test` commands. 

3. Building the project with `cmake -B build`. Two tests are being generated,  `Ames_sub1` and `Ithaca_sub6`. Additional tests will need extra `add_test` lines, one for each scenario.

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

7. The test results are summarized after each scenario and the number of values that differ and the maximum relative and absolute  error are printed as test result. A scenario test fails if one pair of floats are not equal according to the equation above.

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

## Installing SWAT+

Finally, you can install `swatplus.exe` by using the `--install` cmake option. Specify the installation directory that will receive a copy of the SWAT+ executable, generated in `build`. The `--prefix` option takes as an argument the base of the binary directory. Not using `--prefix` will copy `swatplus-???.exe` into the system default binary directory. You don't have to use this step if you want to mange moving the swat executable yourself.

```bash
$ cmake --install build --prefix ~/bin
```
