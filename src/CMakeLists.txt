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
# Testing

if (BUILD_TESTS)
    enable_testing()
endif()

###############################################################################
# Set Fortran compiler FLAGS
get_filename_component (Fortran_COMPILER_NAME ${CMAKE_Fortran_COMPILER} NAME)

if (Fortran_COMPILER_NAME MATCHES "gfortran.*")
 	set (${PROJECT_NAME}_Fortran_FLAGS "-Wall -Wextra -Wimplicit-interface -Wimplicit-procedure -std=f2008")
# 	set (${PROJECT_NAME}_Fortran_FLAGS_RELEASE "-O2")
# 	set (${PROJECT_NAME}_Fortran_FLAGS_DEBUG "-O0 -g")
elseif (Fortran_COMPILER_NAME MATCHES "ifort.*")
 	if (WIN32)
        set (${PROJECT_NAME}_Fortran_FLAGS "/nologo /warn:all /standard-semantics /stand:f08")
# 		set (${PROJECT_NAME}_Fortran_FLAGS_RELEASE "/O2")
# 		set (${PROJECT_NAME}_Fortran_FLAGS_DEBUG " /debug:full /Od")
 	elseif (WIN32)
 		set (${PROJECT_NAME}_Fortran_FLAGS "-nologo -warn all -standard-semantics -std08")
# 		set (${PROJECT_NAME}_Fortran_FLAGS_RELEASE "-O2")
# 		set (${PROJECT_NAME}_Fortran_FLAGS_DEBUG "-g -O0")
 	endif (WIN32)
endif (Fortran_COMPILER_NAME MATCHES "gfortran.*")

# append additional FLAGS to default cmake FLAGS
set (CMAKE_Fortran_FLAGS ${CMAKE_Fortran_FLAGS} ${${PROJECT_NAME}_Fortran_FLAGS})
set (CMAKE_Fortran_FLAGS_RELEASE ${CMAKE_Fortran_FLAGS_RELEASE} ${${PROJECT_NAME}_Fortran_FLAGS_RELEASE})
set (CMAKE_Fortran_FLAGS_DEBUG ${CMAKE_Fortran_FLAGS_DEBUG} ${${PROJECT_NAME}_Fortran_FLAGS_DEBUG})

# On Windows FFLAGS don't seem to be automatically added to compiler flags
if (Win32)
	set(CMAKE_Fortran_FLAGS ${ENV_FFLAGS} ${CMAKE_Fortran_FLAGS})
	set(CMAKE_Fortran_FLAGS_RELEASE ${ENV_FLAGS} ${CMAKE_Fortran_FLAGS_RELEASE})
	set(CMAKE_Fortran_FLAGS_DEBUG ${ENV_FLAGS} ${CMAKE_Fortran_FLAGS_DEBUG})
endif (Win32)

###############################################################################
# Report options and settings
if (BUILD_SHARED_LIBS)
	message("*** Building SHARED libraries")
else(BUILD_SHARED_LIBS)
	message("*** Building STATIC libraries")
endif(BUILD_SHARED_LIBS)

message("*** Build type: ${CMAKE_BUILD_TYPE}")
if (CMAKE_BUILD_TYPE MATCHES DEBUG)
	message("*** Fortran compiler flags: ${CMAKE_Fortran_FLAGS_DEBUG}")
elseif (CMAKE_BUILD_TYPE MATCHES RELEASE)
	message("*** Fortran compiler flags: ${CMAKE_Fortran_FLAGS_RELEASE}")
else()
	message("*** Fortran compiler flags: ${CMAKE_Fortran_FLAGS}")
endif()

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
