# src/CMakeLists.txt
# Root cmake configuration file for Fortran corelib

cmake_minimum_required(VERSION 3.0)

project(fcorelib Fortran)
# name used for library files, include directory, etc.
set(LIBRARY_NAME fcore)

# make sure that the default is a RELEASE
if (NOT CMAKE_BUILD_TYPE)
  set (CMAKE_BUILD_TYPE RELEASE CACHE STRING
      "Choose the type of build, options are: None Debug Release."
      FORCE)
endif (NOT CMAKE_BUILD_TYPE)

###############################################################################
# User-definable options
option(BUILD_SHARED_LIBS "Build shared libraries" ON)
option(BUILD_TESTS "Build unit tests" ON)

###############################################################################
# Path suffixes beneath CMAKE_INSTALL_PREFIX

# if INSTALL_LIBRARY_PREFIX or INSTALL_INCLUDE_PREFIX were specified on the
# command line, use those as prefixes beneath CMAKE_INSTALL_PREFIX where
# library and include files should be placed.

if (NOT INSTALL_LIBRARY_PREFIX)
    set(INSTALL_LIBRARY_PREFIX lib)
endif()

if (NOT INSTALL_INCLUDE_PREFIX)
    set(INSTALL_INCLUDE_PREFIX include)
endif()

###############################################################################
# Testing

if (BUILD_TESTS)
    enable_testing()
endif()

###############################################################################
# Set additional Fortran compiler FLAGS

if (CMAKE_Fortran_COMPILER_ID STREQUAL "GNU")
    # explicitly add -cpp to run C preprocessor
    set(GNU_WARN "-Wall -Wextra -Wimplicit-interface -Wimplicit-procedure -Warray-temporaries -Wrealloc-lhs -pedantic")
    set(${PROJECT_NAME}_Fortran_FLAGS "${GNU_WARN} -std=f2008")
elseif (CMAKE_Fortran_COMPILER_ID STREQUAL "Intel")
    if (WIN32)
        set (${PROJECT_NAME}_Fortran_FLAGS "/warn:all /standard-semantics /stand:f08")
    elseif (WIN32)
        set (${PROJECT_NAME}_Fortran_FLAGS "-warn all -standard-semantics -std08")
    endif (WIN32)
endif ()

# append additional FLAGS to default cmake FLAGS
set (CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} ${${PROJECT_NAME}_Fortran_FLAGS}")
set (CMAKE_Fortran_FLAGS_RELEASE "${CMAKE_Fortran_FLAGS_RELEASE} ${${PROJECT_NAME}_Fortran_FLAGS_RELEASE}")
set (CMAKE_Fortran_FLAGS_DEBUG "${CMAKE_Fortran_FLAGS_DEBUG} ${${PROJECT_NAME}_Fortran_FLAGS_DEBUG}")

###############################################################################
# Report options and settings
if (BUILD_SHARED_LIBS)
    message(STATUS "Building SHARED libraries")
else(BUILD_SHARED_LIBS)
    message(STATUS "Building STATIC libraries")
endif(BUILD_SHARED_LIBS)

message(STATUS "Build type: ${CMAKE_BUILD_TYPE}")
message(STATUS "Fortran compiler flags: ${CMAKE_Fortran_FLAGS}")
message(STATUS "Additional Fortran compiler flags for ${CMAKE_BUILD_TYPE}: ${CMAKE_Fortran_FLAGS_${CMAKE_BUILD_TYPE}}")

message(STATUS "Library install directory: ${CMAKE_INSTALL_PREFIX}/${INSTALL_LIBRARY_PREFIX}")
message(STATUS "MOD file install directory: ${CMAKE_INSTALL_PREFIX}/${INSTALL_INCLUDE_PREFIX}")

###############################################################################
# Output directories

set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY "${CMAKE_BINARY_DIR}/lib")
set(CMAKE_LIBRARY_OUTPUT_DIRECTORY "${CMAKE_BINARY_DIR}/lib")
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY "${CMAKE_BINARY_DIR}/bin")
set(${PROJECT_NAME}_TESTS_DIRECTORY "${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/tests")

set(CMAKE_Fortran_MODULE_DIRECTORY ${CMAKE_BINARY_DIR}/include)
set(${LIBRARY_NAME}_MODDIR ${CMAKE_Fortran_MODULE_DIRECTORY})

add_subdirectory(corelib)

if (BUILD_TESTS)
    add_subdirectory(tests)
endif()
