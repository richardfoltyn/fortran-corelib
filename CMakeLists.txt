
cmake_minimum_required(VERSION 3.12)

project(fcore VERSION 0.1.0 LANGUAGES C Fortran)
# name used for library files, include directory, etc.
set(LIBRARY_NAME fcore)

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
    option(BUILD_SHARED_LIBS "Build shared libraries" OFF)
endif()

option(BUILD_TESTS "Build unit tests" OFF)
option(BUILD_EXAMPLES "Build example code" OFF)
option(DIAGNOSTICS "Enable diagnostic messages" OFF)

###############################################################################
# Testing

if (BUILD_TESTS)
    enable_testing()
endif()

###############################################################################
# Report options and settings
if (BUILD_SHARED_LIBS)
    message(STATUS "Building SHARED libraries")
else(BUILD_SHARED_LIBS)
    message(STATUS "Building STATIC libraries")
endif(BUILD_SHARED_LIBS)

message(STATUS "Build type: ${CMAKE_BUILD_TYPE}")
if (DIAGNOSTICS)
    message(STATUS "Diagnostics ENABLED" )
endif()


###############################################################################
# Path suffixes beneath CMAKE_INSTALL_PREFIX

# if CMAKE_INSTALL_LIBDIR or CMAKE_INSTALL_INCLUDEDIR were specified on the
# command line, use those as prefixes beneath CMAKE_INSTALL_PREFIX where
# library and include files should be placed.

# store values in temporary variables, will be overwritten by GNUInstallDirs
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
# Output directories

set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY "${CMAKE_BINARY_DIR}/${CMAKE_INSTALL_LIBDIR}")
set(CMAKE_LIBRARY_OUTPUT_DIRECTORY "${CMAKE_BINARY_DIR}/${CMAKE_INSTALL_LIBDIR}")
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY "${CMAKE_BINARY_DIR}/bin")
set(TESTS_OUTPUT_DIRECTORY "${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/tests")
set(EXAMPLES_OUTPUT_DIRECTORY "${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/examples")

set(CMAKE_Fortran_MODULE_DIRECTORY "${CMAKE_BINARY_DIR}/include")

# Output directory for additional static library used for linking tests
# and examples irrespective of whether main build is shared/static.
# This is not meant to be installed anywhere, so keep it separate.
set(FCORE_STATIC_DIRECTORY "${CMAKE_BINARY_DIR}/lib-static")

# Allow for installing ifort and gfortran version of libraries side-by-side by
# appending compiler suffix
string(TOLOWER ${CMAKE_Fortran_COMPILER_ID} Fortran_COMPILER_ID)

# Find major compiler version
string(REGEX REPLACE "(^[0-9]+).*" "\\1" Fortran_COMPILER_VERSION_MAJOR
    "${CMAKE_Fortran_COMPILER_VERSION}")

set(Fortran_COMPILER_SUFFIX "${Fortran_COMPILER_ID}-${Fortran_COMPILER_VERSION_MAJOR}")

# Installation directory within <prefix>/lib, <prefix>/include, etc.
# If not defined by -DINSTALL_DIR_NAME by the user, create 
# version- and compiler-specific directory name.
if (NOT DEFINED INSTALL_DIR_NAME)
    set(_version ${PROJECT_VERSION_MAJOR}.${PROJECT_VERSION_MINOR})
    set(INSTALL_DIR_NAME
        "${LIBRARY_NAME}-${_version}-${Fortran_COMPILER_SUFFIX}"
        CACHE STRING "Install directory name"
        FORCE
    )
endif()

get_filename_component(_LIBDIR
    "${CMAKE_INSTALL_PREFIX}/${CMAKE_INSTALL_LIBDIR}/${INSTALL_DIR_NAME}" 
    REALPATH
)
get_filename_component(_INCLDIR
    "${CMAKE_INSTALL_PREFIX}/${CMAKE_INSTALL_INCLUDEDIR}/${INSTALL_DIR_NAME}" 
    REALPATH
)
message(STATUS "Library install directory: ${_LIBDIR}")
message(STATUS "MOD file install directory: ${_INCLDIR}")

################################################################################
# Universal compiler FLAGS for all targets

unset(_FFLAGS)
unset(_FFLAGS_DEBUG)

if (CMAKE_Fortran_COMPILER_ID STREQUAL "GNU" OR MINGW)
    set(_FFLAGS "-std=f2018 -frecursive -fimplicit-none")
    set(_FFLAGS_DEBUG "-fcheck=all")
elseif (CMAKE_Fortran_COMPILER_ID MATCHES "Intel(LLVM)?")
    if (WIN32)
        set(_FFLAGS "/auto /assume:recursion /stand:f18")
        set(_FFLAGS_DEBUG "/check:all")
        # Disable seriously annoying warning that ifort is deprecated
        if (CMAKE_Fortran_COMPILER_ID STREQUAL "Intel")
            set(_FFLAGS "${_FFLAGS} /Qdiag-disable:10448")
        endif ()
    else ()
        set(_FFLAGS "-auto -assume recursion -std18")
        set(_FFLAGS_DEBUG "-check all")
        # Disable seriously annoying warning that ifort is deprecated
        if (CMAKE_Fortran_COMPILER_ID STREQUAL "Intel")
            set(_FFLAGS "${_FFLAGS} -diag-disable=10448")
        endif ()
    endif (WIN32)
endif ()

set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} ${_FFLAGS}")
set(CMAKE_Fortran_FLAGS_DEBUG "${CMAKE_Fortran_FLAGS_DEBUG} ${_FFLAGS_DEBUG}")


###############################################################################
# Add source directories

add_subdirectory(src)

if (BUILD_TESTS)
    add_subdirectory(tests)
endif()

if (BUILD_EXAMPLES)
    add_subdirectory(examples)
endif()


###############################################################################
# Installation

# Path there cmake config files will be installed
set(FCORE_INSTALL_CONFIG_PATH 
    "${CMAKE_INSTALL_LIBDIR}/${INSTALL_DIR_NAME}/cmake"
)

set(FCORE_INSTALL_NAMESPACE ${LIBRARY_NAME})

set(FCORE_CONFIG_FILE ${LIBRARY_NAME}-config.cmake)
set(FCORE_CONFIG_VERSION_FILE ${LIBRARY_NAME}-config-version.cmake)
set(FCORE_TARGETS_FILE ${LIBRARY_NAME}Targets.cmake)

# Directory that contains installation-specific (config) files
get_filename_component(FCORE_INSTALL_SOURCE_DIR
    "${CMAKE_CURRENT_LIST_DIR}/install"
    ABSOLUTE
)

configure_file("${FCORE_INSTALL_SOURCE_DIR}/config-version.cmake.in"
    "${CMAKE_CURRENT_BINARY_DIR}/${FCORE_CONFIG_VERSION_FILE}"
    @ONLY
)

include(CMakePackageConfigHelpers)

configure_package_config_file("${FCORE_INSTALL_SOURCE_DIR}/config.cmake.in"
    "${CMAKE_CURRENT_BINARY_DIR}/${FCORE_CONFIG_FILE}"
    INSTALL_DESTINATION "${FCORE_INSTALL_CONFIG_PATH}"
    PATH_VARS FCORE_INSTALL_CONFIG_PATH
)

# Note: INCLUDES DESTINATION is equivalent to adding
# INTERFACE_INCLUDE_DIRECTORIES property on exported targets.
install(TARGETS ${LIBRARY_NAME} 
    EXPORT ${LIBRARY_NAME}_targets
    RUNTIME DESTINATION "${CMAKE_INSTALL_LIBDIR}/${INSTALL_DIR_NAME}"
    LIBRARY DESTINATION "${CMAKE_INSTALL_LIBDIR}/${INSTALL_DIR_NAME}"
    ARCHIVE DESTINATION "${CMAKE_INSTALL_LIBDIR}/${INSTALL_DIR_NAME}"
    INCLUDES DESTINATION
        "${CMAKE_INSTALL_INCLUDEDIR}/${INSTALL_DIR_NAME}"
)

# export(EXPORT ${LIBRARY_NAME}_targets
#     FILE "${CMAKE_CURRENT_BINARY_DIR}/${FCORE_TARGETS_FILE}"
#     NAMESPACE ${FCORE_INSTALL_NAMESPACE}::
# )

install(EXPORT ${LIBRARY_NAME}_targets
    FILE "${FCORE_TARGETS_FILE}"
    NAMESPACE ${FCORE_INSTALL_NAMESPACE}::
    DESTINATION "${FCORE_INSTALL_CONFIG_PATH}"
)

# install compiler-specific MOD files
install(DIRECTORY "${CMAKE_Fortran_MODULE_DIRECTORY}/"
    DESTINATION "${CMAKE_INSTALL_INCLUDEDIR}/${INSTALL_DIR_NAME}"
    COMPONENT devel
    FILES_MATCHING PATTERN "fcore*.mod"
)

install(FILES
    "${CMAKE_CURRENT_BINARY_DIR}/${FCORE_CONFIG_FILE}"
    "${CMAKE_CURRENT_BINARY_DIR}/${FCORE_CONFIG_VERSION_FILE}"
    "${FCORE_INSTALL_SOURCE_DIR}/compiler_test.f90"
    DESTINATION "${FCORE_INSTALL_CONFIG_PATH}"
    COMPONENT devel
)