## Building SWAT+

SWAT+ will be built *out-of-source*, which means that all intermediate and final files are **not** entangled with the sources. This allows keeping the source tree clean, all generated resources by the build can be quickly removed. In addition, multiple SWAT+ build flavors can be build using the single source tree.

```cmake
file(GLOB sources src/*.f90)
add_executable(${SWATPLUS_EXE} ${sources})
```

### Generate the Build files

Make the `swatplus` directory the current working directory. All command examples below assume this.

```bash
$ cd <dir>/swatplus
```

Now, create all build files. Issue one of the following commands to configure and generate the build files. This does not build SWAT+ yet. CMake will process the content of the file `CMakeLists.txt` and will generate the build system.

The command below reads the `CMakeLists.txt` source file from the current directory `.`, creates a directory `build` and stores all resources for the build in it.

```bash
$ cmake -S . -B build
  <or short>
$ cmake -B build
```

You can force `cmake` to use the GCC `gfortran` compiler.

```bash
$ cmake -B build -D CMAKE_Fortran_COMPILER=gfortran
```

If you want to use Intel Fortran to build SWAT+, configure as below. It defaults to `gfortran` otherwise.

The Intel `ifort` Fortran compiler will be deprecated in 2024 and is being replaced with the LLVM-based version, `ifx`. Therefore, the use of `ifx` is preferred. To generate the build files on Linux:

```bash
$ cmake -B build -D CMAKE_Fortran_COMPILER=ifort
  <or better>
$ cmake -B build -D CMAKE_Fortran_COMPILER=ifx
```

On Windows you can use VS Code to build SWAT+ or you can issue the commands below to use the different compilers:

```bash
$ cmake -B build -D CMAKE_GENERATOR_TOOLSET=fortran=ifort
$ cmake -B build -D CMAKE_GENERATOR_TOOLSET=fortran=ifx
$ cmake -B build -G "Ninja" -D CMAKE_Fortran_COMPILER=gfortran
```

>  *__Note__: `ifort` and `ifx` use a different generator toolset than `gfortran``  on Windows, also use the windows default CLI, not Powershell to build on the command line.*  



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

### Compiling/Linking the Executable

To build the SWAT+ executable, use the `cmake --build` command in the `swatplus` directory. It will process the previously generated files in `build` and place all intermediate resources (`*.o`, `*.mod` files) into the build directory.

```bash
 $ cmake --build build
```

Use the `-j` option to compile the source files in parallel. The example below is using `8` CPU cores in parallel. This greatly speeds up the compile process.

```bash
 $ cmake --build build -j 8
```

You can find the SWAT+ executable in the build folder as `swatplus-<version>-<arch>.exe` (for Windows). The Linux and Mac executable will not have an `.exe` extension.

Call the build target `clean` to remove all generated object files (`.o`) and modules files (`.mod`). Alternatively, you can do (`cd build; make clean`).

```bash
 $ cmake --build build --target clean
```

Building the swat model is supported for various platforms and compilers as described in the `Readme.md` file. It contains the specific instructions for Windows and Mac builds.

### Installing

Finally, you can install the `swatplus` executable by using the `--install` CMake option. Specify the installation directory that will receive a copy of the SWAT+ executable, generated in `build`. The `--prefix` option takes as an argument the base of the binary directory. Not using `--prefix` will copy `swatplus` into the system default binary directory. You don't have to use this step if you want to manage moving the swat executable yourself.

```bash
$ cmake --install build --prefix ~/bin
```
