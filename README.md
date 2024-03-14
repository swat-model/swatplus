# SWAT+

The Soil and Water Assessment Tool Plus (SWAT+) is a public domain model jointly developed by the USDA Agricultural Research Service (USDA-ARS) and Texas A&M AgriLife Research, part of The Texas A&M University System. SWAT+ is a small watershed to river basin-scale model to simulate the quality and quantity of surface and ground water and predict the environmental impact of land use, land management practices, and climate change. SWAT is widely used in assessing soil erosion prevention and control, non-point source pollution control and regional management in watersheds.
...

(Add more...)

## Building SWAT+

This repository is setup to build, test, and deploy SWAT+ using `CMake`. CMake is a cross-platform build tool, it can be used at the command line but it is also supported through various IDEs, etc. More information can be found at http://cmake.org. In addition to CMake, make sure the following tools are also installed:

* `make` tool (for building)
* `gfortran` or `ifort/ifx` compiler and linker (for compiling/linking)
* `python3` (for testing, optional)

Use your operating system's preferred way of adding those tools to your installation. There is certainly more than one way of getting and installing them.

## Get the Sources

Fist, get the SWAT+ sources by either cloning the repository using `git` or downloading the zipped sources.

Get the repo from GitHub by cloning it. Use a tagged version (preferred).

```bash
$ git clone https://github.com/swat-model/swatplus
```

Or, download the sources directly from the artifacts, unzip.

```bash
$ wget https://github.com/swat-model/swatplus/archive/refs/tags/v60.7.4.zip
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

## Configure/Generate the Build files

SWAT+ will be built *out-of-source*, which means that all intermediate and final files are __not__ entangled with the sources. This allows keeping the source tree clean, all generated resources by the build can be quickly removed. In addition, multiple SWAT build flavors can be build using the single source tree.

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

You can force cmake to use the GCC gfortran compiler.

```bash
$ cmake -B build -D CMAKE_Fortran_COMPILER=gfortran
```

If you want to use Intel Fortran to build SWAT+, configure as below. It defaults to `gfortran` otherwise.

The Intel `ifort` Fortran Compiler will be deprecated in 2024 and is being replaced with the LLVM-based version, `ifx`. Therefore, the use of `ifx` is preferred.

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

```
> rd /s /q build
```

## Build the SWAT+ Executable

To build the SWAT+ executable, use the `cmake --build` command in the `swatplus` directory. It will process the previously generated files in `build` and place all intermediate resources (*.o, *.mod files) into the build directory.

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

## Scenario Testing

Running tests may reveal potential regressions of code changes against existing and assumed correct output. Here, we test the entire swatplus model against known scenario outputs and compare the results. Results should not differ significantly. Each SWAT+ unit test consists of one scenario run. The repository's `test` directory contains the Python test scripts and the `data` folder the reference scenarios.

Tests are run using the `ctest` command, which is part if the `cmake` installation. Alternatively, you can use the command `make test` within the `build` directory.

```bash
$ cd build
$ ctest
```

`ctest` provides a test summary as output. Somewhere in `build/Testing`, there is also the detailed standard output of `ctest` for each test. Also, you can run all tests, a selected subset of test, or just an individual one. `ctest` is quite powerful and flexible.

## Installing SWAT+

Finally, you can install `swatplus.exe` by using the `--install` cmake option. Specify the installation directory that will receive a copy of the  swatplus executable, generated in `build`. The `--prefix` option takes as an argument the base of the binary dir. Not using `--prefix` will copy swatplus.exe into the system default binary directory. You don't have to use this step if you want to mange moving the swat executable yourself.

```bash
$ cmake --install build --prefix ~/bin
```
