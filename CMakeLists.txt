cmake_minimum_required(VERSION 4.1)

project(fcore VERSION 0.1.0 LANGUAGES C Fortran)

if(PROJECT_IS_TOP_LEVEL AND NOT CMAKE_CONFIGURATION_TYPES
        AND NOT CMAKE_BUILD_TYPE)
    set(CMAKE_BUILD_TYPE Release CACHE STRING "Build configuration" FORCE)
endif()

option(BUILD_SHARED_LIBS "Build shared libraries" OFF)
option(FCORE_BUILD_TESTS "Build fcore unit tests" OFF)
option(FCORE_BUILD_EXAMPLES "Build fcore examples" OFF)
option(FCORE_DIAGNOSTICS "Enable fcore diagnostic messages" OFF)

include(GNUInstallDirs)
include(CMakePackageConfigHelpers)
include(cmake/CompilerOptions.cmake)

# Fortran module files are specific to the compiler and its major version.
string(TOLOWER "${CMAKE_Fortran_COMPILER_ID}" FCORE_COMPILER_ID)
string(REGEX MATCH "^[0-9]+" FCORE_COMPILER_MAJOR "${CMAKE_Fortran_COMPILER_VERSION}")
set(FCORE_INSTALL_DIR_NAME
    "fcore-${PROJECT_VERSION_MAJOR}.${PROJECT_VERSION_MINOR}-${FCORE_COMPILER_ID}-${FCORE_COMPILER_MAJOR}"
    CACHE STRING "Version- and compiler-specific installation subdirectory"
)
set(FCORE_INSTALL_LIBDIR "${CMAKE_INSTALL_LIBDIR}/${FCORE_INSTALL_DIR_NAME}")
set(FCORE_INSTALL_INCLUDEDIR "${CMAKE_INSTALL_INCLUDEDIR}/${FCORE_INSTALL_DIR_NAME}")
set(FCORE_INSTALL_CMAKEDIR "${FCORE_INSTALL_LIBDIR}/cmake")
set(FCORE_MODULE_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}/modules")

add_subdirectory(src)

if(FCORE_BUILD_TESTS)
    enable_testing()
    add_subdirectory(tests)
endif()

if(FCORE_BUILD_EXAMPLES)
    add_subdirectory(examples)
endif()

configure_package_config_file(install/config.cmake.in
    "${CMAKE_CURRENT_BINARY_DIR}/fcore-config.cmake"
    INSTALL_DESTINATION "${FCORE_INSTALL_CMAKEDIR}"
)
write_basic_package_version_file(
    "${CMAKE_CURRENT_BINARY_DIR}/fcore-config-version-base.cmake"
    VERSION "${PROJECT_VERSION}"
    COMPATIBILITY SameMinorVersion
)
configure_file(install/config-version.cmake.in
    "${CMAKE_CURRENT_BINARY_DIR}/fcore-config-version.cmake"
    @ONLY
)

install(TARGETS fcore EXPORT fcore_targets
    RUNTIME DESTINATION "${CMAKE_INSTALL_BINDIR}/${FCORE_INSTALL_DIR_NAME}"
    LIBRARY DESTINATION "${FCORE_INSTALL_LIBDIR}"
    ARCHIVE DESTINATION "${FCORE_INSTALL_LIBDIR}"
)
install(EXPORT fcore_targets
    FILE fcoreTargets.cmake
    NAMESPACE fcore::
    DESTINATION "${FCORE_INSTALL_CMAKEDIR}"
    COMPONENT devel
)

# Intel's Visual Studio integration appends the configuration automatically.
if(CMAKE_GENERATOR MATCHES "^Visual Studio")
    set(FCORE_INSTALL_MODULE_DIRECTORY "${FCORE_MODULE_DIRECTORY}/$<CONFIG>")
else()
    set(FCORE_INSTALL_MODULE_DIRECTORY "${FCORE_MODULE_DIRECTORY}")
endif()
install(DIRECTORY "${FCORE_INSTALL_MODULE_DIRECTORY}/"
    DESTINATION "${FCORE_INSTALL_INCLUDEDIR}"
    COMPONENT devel
    FILES_MATCHING PATTERN "fcore*.mod"
)
install(FILES
    "${CMAKE_CURRENT_BINARY_DIR}/fcore-config.cmake"
    "${CMAKE_CURRENT_BINARY_DIR}/fcore-config-version.cmake"
    "${CMAKE_CURRENT_BINARY_DIR}/fcore-config-version-base.cmake"
    "${CMAKE_CURRENT_SOURCE_DIR}/install/compiler_test.f90"
    DESTINATION "${FCORE_INSTALL_CMAKEDIR}"
    COMPONENT devel
)
