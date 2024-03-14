###############################################################
# SWAT+ cmake, req min v3.20 (ifx, etc. support)
###############################################################
if("${CMAKE_SOURCE_DIR}" STREQUAL "${CMAKE_BINARY_DIR}" OR
   "${CMAKE_SOURCE_DIR}/src" STREQUAL "${CMAKE_BINARY_DIR}")
   message(FATAL_ERROR "'in-source' builds not permitted, e.g. use 'cmake -B build' instead.")
endif()

cmake_minimum_required(VERSION 3.20)

project(swatplus VERSION 0.2 LANGUAGES Fortran)

include(CTest)

execute_process(
  COMMAND git describe --tags
  WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}
  OUTPUT_VARIABLE TAG
  OUTPUT_STRIP_TRAILING_WHITESPACE
)

if(TAG STREQUAL "")
   message(FATAL_ERROR "No tag info found.") #should not happen
endif()


# SWAT Version number
SET(SWAT_VERSION ${TAG})
set(SWATPLUS_EXE "swatplus-${SWAT_VERSION}.exe")

# Enable this to 'TRUE' to see the fortran command on compile
# set(CMAKE_VERBOSE_MAKEFILE FALSE)

if (UNIX)
    if(CMAKE_Fortran_COMPILER_ID STREQUAL Intel)
        set(fdialect        "-free -diag-disable=10448")
        set(fdebug          "-traceback -warn all")
        set(frelease        "-O3")
        link_libraries("-static")
    elseif(CMAKE_Fortran_COMPILER_ID STREQUAL IntelLLVM)
        set(fdialect        "-free")
        set(fdebug          "-traceback -warn all")
        set(frelease        "-O3")
        link_libraries("-static")
    elseif(CMAKE_Fortran_COMPILER_ID MATCHES GNU)
        set(fdialect "-fimplicit-none -ffree-line-length-none -fbacktrace -finit-local-zero -fno-unsafe-math-optimizations -frounding-math -fsignaling-nans")
        set(fdebug   "")
        set(frelease "-O")
        if(NOT APPLE)
            link_libraries("-static")
        endif()
    endif()
elseif(WIN32)
    if(CMAKE_Fortran_COMPILER_ID STREQUAL Intel)
        set(fdialect        "/free /Qdiag-disable:all /Qdiag-disable:remarks")
        set(fdebug          "/traceback /warn:all")
        set(frelease        "/O3")
    endif()
endif()


set(CMAKE_Fortran_FLAGS         "${CMAKE_Fortran_FLAGS} ${fdialect}")
set(CMAKE_Fortran_FLAGS_DEBUG   "${CMAKE_Fortran_FLAGS_DEBUG} ${fdebug}")
# set(CMAKE_Fortran_FLAGS_RELEASE "${CMAKE_Fortran_FLAGS_RELEASE} ${frelease}")
set(CMAKE_Fortran_FLAGS_RELEASE "${frelease}")


#############################################################################
# Build

# get_cmake_property(_variableNames VARIABLES)
# list (SORT _variableNames)
# foreach (_variableName ${_variableNames})
#     message(STATUS "${_variableName}=${${_variableName}}")
# endforeach()

if(EXISTS "${PROJECT_SOURCE_DIR}/src/main.f90.in")
    string(TIMESTAMP ISO   "%Y-%m-%d %H:%M:%S")
    string(TIMESTAMP TODAY "%b %d %Y")  # e.g. produces Dec 7 2023
    string(TIMESTAMP YEAR  "%Y")

    configure_file(
        "${PROJECT_SOURCE_DIR}/src/main.f90.in"
        "${PROJECT_SOURCE_DIR}/src/main.f90"
    )
endif()

file(GLOB sources src/*.f90)
add_executable(${SWATPLUS_EXE} ${sources})


#############################################################################
# Install
install(TARGETS ${SWATPLUS_EXE} DESTINATION RUNTIME DESTINATION .)


#############################################################################
# Testing
find_package(Python REQUIRED)

set(check_py "${PROJECT_SOURCE_DIR}/test/check.py")
set(exe_path "${PROJECT_BINARY_DIR}/${SWATPLUS_EXE}")
set(test_dir "${PROJECT_BINARY_DIR}/data")
set(ref_dir "${PROJECT_SOURCE_DIR}/data")

# error tolerances
set(rerr "0.05")
set(aerr "1e-8")

add_test(Ithaca_sub6 python3 ${check_py} ${exe_path} ${ref_dir}/Ithaca_sub6 ${test_dir} ${aerr} ${rerr})
add_test(Ames_sub1   python3 ${check_py} ${exe_path} ${ref_dir}/Ames_sub1   ${test_dir} ${aerr} ${rerr})


#############################################################################
# Summary
message( "-- Verbose Makefile   ${CMAKE_VERBOSE_MAKEFILE}")
message( "-- Compiler           ${CMAKE_Fortran_COMPILER} (${CMAKE_Fortran_COMPILER_ID} ${CMAKE_Fortran_COMPILER_VERSION})")
message( "-- FFLAGS            ${CMAKE_Fortran_FLAGS}")
message( "--  RELEASE           ${CMAKE_Fortran_FLAGS_RELEASE}")
message( "--  DEBUG             ${CMAKE_Fortran_FLAGS_DEBUG}")
message( "-- PROJECT_DIR        ${PROJECT_SOURCE_DIR}")
message( "-- BINARY_DIR         ${PROJECT_BINARY_DIR}")
message( "-- Install Prefix     ${CMAKE_INSTALL_PREFIX}")
message( "-- Executable         ${SWATPLUS_EXE}, rev/tag: ${SWAT_VERSION}")
message( "-- Test Data Dir      ${ref_dir}")
message( "--  Abs Error         ${aerr}")
message( "--  Rel Error         ${rerr}")
