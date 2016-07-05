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

set(CMAKE_VERBOSE_MAKEFILE ON)

###############################################################################
# User-definable options
option(BUILD_SHARED_LIBS "Build shared libraries" ON)
option(BUILD_TESTS "Build unit tests" ON)

###############################################################################
# Detect target architecture
list(APPEND CMAKE_MODULE_PATH "${CMAKE_CURRENT_SOURCE_DIR}/cmake/Modules")

include(FindTargetArch)
find_target_arch()

if (CMAKE_Fortran_COMPILER_ID STREQUAL "GNU")
    set(INCLUDE_SUBDIR "gfortran")
elseif (CMAKE_Fortran_COMPILER_ID STREQUAL "Intel")
    if (TARGET_ARCH_BITS STREQUAL "64")
        set(INCLUDE_SUBDIR "intel64")
    else()
        set(INCLUDE_SUBDIR "intel")
    endif()
endif ()

###############################################################################
# Testing

if (BUILD_TESTS)
    enable_testing()
endif()

###############################################################################
# Set additional Fortran compiler FLAGS

if (CMAKE_Fortran_COMPILER_ID STREQUAL "GNU")
    set(${PROJECT_NAME}_Fortran_FLAGS "-Wall -Wextra -Wimplicit-interface -Wimplicit-procedure -std=f2008")
elseif (CMAKE_Fortran_COMPILER_ID STREQUAL "Intel")
    if (WIN32)
        set (${PROJECT_NAME}_Fortran_FLAGS "/warn:all /standard-semantics /stand:f08")
    elseif (WIN32)
        set (${PROJECT_NAME}_Fortran_FLAGS "-warn all -standard-semantics -std08")
    endif (WIN32)
else ()
    message(FATAL_ERROR "Unsupported Fortran compiler: ${Fortran_COMPILER_NAME}")
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
