## Tagging

A new model version is directly derived from a tag in the `main` branch. There is no need to manually edit source files and name the executable. That git tag name is directly projected into 1) `main.f90` and 1) the name of the executable. The file `CMakeLists.txt` contains the git command to obtain the current tag for the current branch and derive the version from it. The git command is `git describe --tags`, which is returning either the current tag, or the most recent tag reachable from the commit as a the relative difference of number of commits and the abbreviated object name.

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

Then follow the build instructions below. It will eventually create the executable `swatplus-60.0.exe`. You can also switch back to HEAD of the main branch and again, follow the build steps. This will create the executable `swatplus-61.1-1-g92b4413.exe` which is the version one commit (`-1-`) after version `61.1`, starting with the object name: `92b4413......` (the `g` in the string indicates Git as the system that created the name).

This tag-based building approach has the advantage to:

- always align the version of the swat executable with the tagged version of the sources in the Git repository.

- allow the fully automated version propagation into source files and executable in tandem with `cmake` .

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
