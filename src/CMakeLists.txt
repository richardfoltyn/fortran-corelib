# src/CMakeLists.txt
# Root cmake configuration file for Fortran corelib

cmake_minimum_required(VERSION 3.1)

project(fcorelib Fortran)
# name used for library files, include directory, etc.
set(LIBRARY_NAME fcorelib)

# make sure that the default is a RELEASE
if (NOT CMAKE_BUILD_TYPE)
  set (CMAKE_BUILD_TYPE Release CACHE STRING
      "Choose the type of build, options are: None Debug Release."
      FORCE)
endif (NOT CMAKE_BUILD_TYPE)

###############################################################################
# User-definable options
if (WIN32)
    option(BUILD_SHARED_LIBS "Build shared libraries" OFF)
else()
    option(BUILD_SHARED_LIBS "Build shared libraries" ON)
endif()

option(BUILD_TESTS "Build unit tests" ON)
option(BUILD_EXAMPLES "Build example code" ON)

###############################################################################
# Version
set(CORELIB_VERSION_COUNT 3)
set(CORELIB_VERSION_MAJOR 0)
set(CORELIB_VERSION_MINOR 1)
set(CORELIB_VERSION_PATCH 0)
set(CORELIB_VERSION
	"${CORELIB_VERSION_MAJOR}.${CORELIB_VERSION_MINOR}.${CORELIB_VERSION_PATCH}"
)

###############################################################################
# Path suffixes beneath CMAKE_INSTALL_PREFIX

# if CMAKE_INSTALL_LIBDIR or CMAKE_INSTALL_INCLUDEDIR were specified on the
# command line, use those as prefixes beneath CMAKE_INSTALL_PREFIX where
# library and include files should be placed.

# store values in tempoprary variables, will be overwritten by GNUInstallDirs
set(_INSTALL_LIBDIR "${CMAKE_INSTALL_LIBDIR}")
set(_INSTALL_INCLUDEDDIR "${CMAKE_INSTALL_INCLUDEDIR}")

include(GNUInstallDirs)

# fill in any missing values
if (_INSTALL_LIBDIR)
    set(CMAKE_INSTALL_LIBDIR "${_INSTALL_LIBDIR}")
elseif (NOT CMAKE_INSTALL_LIBDIR)
    set(CMAKE_INSTALL_LIBDIR lib)
endif()

if (_INSTALL_INCLUDEDDIR)
    set(CMAKE_INSTALL_INCLUDEDIR "${_INSTALL_INCLUDEDDIR}")
elseif (NOT CMAKE_INSTALL_INCLUDEDIR)
    set(CMAKE_INSTALL_INCLUDEDIR include)
endif()

###############################################################################
# Testing

if (BUILD_TESTS)
    enable_testing()
endif()

###############################################################################
# Set additional Fortran compiler FLAGS

unset(_FFLAGS)
unset(_FFLAGS_DEBUG)
if (CMAKE_Fortran_COMPILER_ID STREQUAL "GNU")
    # explicitly add -cpp to run C preprocessor if required
    set(GNU_WARN "-Wall -Wextra -Wimplicit-interface -Wimplicit-procedure \
      -Warray-temporaries -Wrealloc-lhs -pedantic")
    set(_FFLAGS "${GNU_WARN} -std=f2008")
    set(_FFLAGS_DEBUG "-fcheck=all")
elseif (CMAKE_Fortran_COMPILER_ID STREQUAL "Intel")
    if (WIN32)
        set(_FFLAGS "/warn:all /standard-semantics /stand:f08")
        set(_FFLAGS_DEBUG "/check:all")
    else ()
        set(_FFLAGS "-warn all -standard-semantics -std08")
        set(_FFLAGS_DEBUG "-check all")
    endif (WIN32)
endif ()

# append additional FLAGS to default cmake FLAGS
set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} ${_FFLAGS}")
# set (CMAKE_Fortran_FLAGS_RELEASE "${CMAKE_Fortran_FLAGS_RELEASE} ${_FFLAGS}")
set(CMAKE_Fortran_FLAGS_DEBUG "${CMAKE_Fortran_FLAGS_DEBUG} ${_FFLAGS_DEBUG}")

###############################################################################
# Report options and settings
if (BUILD_SHARED_LIBS)
    message(STATUS "Building SHARED libraries")
else(BUILD_SHARED_LIBS)
    message(STATUS "Building STATIC libraries")
endif(BUILD_SHARED_LIBS)

message(STATUS "Build type: ${CMAKE_BUILD_TYPE}")
message(STATUS "Fortran compiler flags: ${CMAKE_Fortran_FLAGS}")
# Make sure build type is upper case, otherwise the wrong flags will be shown
string(TOUPPER "${CMAKE_BUILD_TYPE}" _BUILD_TYPE)
message(STATUS "Additional Fortran compiler flags for ${CMAKE_BUILD_TYPE}: ${CMAKE_Fortran_FLAGS_${_BUILD_TYPE}}")

message(STATUS "Library install directory: ${CMAKE_INSTALL_PREFIX}/${CMAKE_INSTALL_LIBDIR}")
message(STATUS "MOD file install directory: ${CMAKE_INSTALL_PREFIX}/${CMAKE_INSTALL_INCLUDEDIR}")

###############################################################################
# Output directories

set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY "${CMAKE_BINARY_DIR}/lib")
set(CMAKE_LIBRARY_OUTPUT_DIRECTORY "${CMAKE_BINARY_DIR}/lib")
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY "${CMAKE_BINARY_DIR}/bin")
set(${PROJECT_NAME}_TESTS_DIRECTORY "${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/tests")
set(${PROJECT_NAME}_EXAMPLES_DIRECTORY "${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/examples")

set(CMAKE_Fortran_MODULE_DIRECTORY ${CMAKE_BINARY_DIR}/include)
set(${PROJECT_NAME}_MODDIR ${CMAKE_Fortran_MODULE_DIRECTORY})

add_subdirectory(corelib)

if (BUILD_TESTS)
    add_subdirectory(tests)
endif()

if (BUILD_EXAMPLES)
    add_subdirectory(examples)
endif()
