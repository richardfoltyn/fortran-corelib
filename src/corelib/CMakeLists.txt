
################################################################################
# Main library and its components

set(CORELIB_COMPONENTS argparse common collections datetime
    io logging strings testing
)

include_directories("${${PROJECT_NAME}_MODDIR}")

# Update corelib module
configure_file(corelib.f90.in
    ${CMAKE_BINARY_DIR}/corelib.f90
)

set(COMPONENT_OBJECTS)

foreach(_comp IN LISTS CORELIB_COMPONENTS)
    add_subdirectory(${_comp})
    set_target_properties(${_comp} PROPERTIES
        Fortran_MODULE_DIRECTORY "${${PROJECT_NAME}_MODDIR}"
    )

    # we need to make sure that -fPIC etc. is passed to compiler when building
    # object libraries that will be used to create a shared lib
    if (BUILD_SHARED_LIBS)
        set_target_properties(${_comp} PROPERTIES POSITION_INDEPENDENT_CODE ON)
    endif ()

    # append created objects, to be used as input in main library
    list(APPEND COMPONENT_OBJECTS $<TARGET_OBJECTS:${_comp}>)
endforeach()

set(SOURCE_FILES
    ${CMAKE_BINARY_DIR}/corelib.f90
)

add_library(${LIBRARY_NAME} ${SOURCE_FILES} ${COMPONENT_OBJECTS})
set_target_properties(${LIBRARY_NAME} PROPERTIES
    OUTPUT_NAME ${LIBRARY_NAME}
    RUNTIME_OUTPUT_DIRECTORY "${CMAKE_LIBRARY_OUTPUT_DIRECTORY}"
    VERSION ${CORELIB_VERSION}
)

# build static library so tests can link against it
if (BUILD_TESTS OR BUILD_EXAMPLES)
    add_library(${LIBRARY_NAME}_static STATIC ${SOURCE_FILES} ${COMPONENT_OBJECTS})
endif()

##########################################################################################
# INSTALLATION

include(CMakePackageConfigHelpers)

# Allow for installing ifort and gfortran version of libraries side-by-side by 
# appending compiler suffix
if (CMAKE_Fortran_COMPILER_ID STREQUAL "GNU")
    set(Fortran_COMPILER_SUFFIX "gf")
elseif (CMAKE_Fortran_COMPILER_ID STREQUAL "Intel")
    set(Fortran_COMPILER_SUFFIX "ifort")
else ()
    message(FATAL_ERROR "Unsupported Fortran compiler: ${CMAKE_Fortran_COMPILER_ID}")
endif ()

set(_version ${CORELIB_VERSION_MAJOR}.${CORELIB_VERSION_MINOR})
set(CORELIB_INSTALL_DIR
    "${LIBRARY_NAME}-${_version}-${Fortran_COMPILER_SUFFIX}"
)

set(CORELIB_INSTALL_CONFIG_PATH 
    ${CMAKE_INSTALL_LIBDIR}/cmake/${CORELIB_INSTALL_DIR}
)

set(CORELIB_INSTALL_NAMESPACE ${LIBRARY_NAME})

set(CORELIB_CONFIG_FILE ${LIBRARY_NAME}-config.cmake)
set(CORELIB_CONFIG_VERSION_FILE ${LIBRARY_NAME}-config-version.cmake)
set(CORELIB_TARGETS_FILE ${LIBRARY_NAME}Targets.cmake)

configure_file(${CORELIB_INSTALL_SOURCE_DIR}/config-version.cmake.in
    ${CMAKE_CURRENT_BINARY_DIR}/${CORELIB_CONFIG_VERSION_FILE}
    @ONLY
)

configure_package_config_file(${CORELIB_INSTALL_SOURCE_DIR}/config.cmake.in
    ${CMAKE_CURRENT_BINARY_DIR}/${CORELIB_CONFIG_FILE}
    INSTALL_DESTINATION ${CORELIB_INSTALL_CONFIG_PATH}
    PATH_VARS CORELIB_INSTALL_CONFIG_PATH
)

# Note: INCLUDES DESTINATION is equivalent to adding
# INTERFACE_INCLUDE_DIRECTORIES property on exported targets.
install(TARGETS ${LIBRARY_NAME} 
    EXPORT ${LIBRARY_NAME}_targets
    RUNTIME DESTINATION "${CMAKE_INSTALL_LIBDIR}/${CORELIB_INSTALL_DIR}"
    LIBRARY DESTINATION "${CMAKE_INSTALL_LIBDIR}/${CORELIB_INSTALL_DIR}"
    ARCHIVE DESTINATION "${CMAKE_INSTALL_LIBDIR}/${CORELIB_INSTALL_DIR}"
    INCLUDES DESTINATION
        "${CMAKE_INSTALL_INCLUDEDIR}/${CORELIB_INSTALL_DIR}"
)

export(EXPORT ${LIBRARY_NAME}_targets
    FILE "${CMAKE_CURRENT_BINARY_DIR}/${CORELIB_TARGETS_FILE}"
    NAMESPACE ${CORELIB_INSTALL_NAMESPACE}::
)

install(EXPORT ${LIBRARY_NAME}_targets
    FILE ${CORELIB_TARGETS_FILE}
    NAMESPACE ${CORELIB_INSTALL_NAMESPACE}::
    DESTINATION ${CORELIB_INSTALL_CONFIG_PATH}
)

# install compiler-specific MOD files
install(DIRECTORY ${${PROJECT_NAME}_MODDIR}/
    DESTINATION "${CMAKE_INSTALL_INCLUDEDIR}/${CORELIB_INSTALL_DIR}"
    COMPONENT devel
)

install(FILES
    "${CMAKE_CURRENT_BINARY_DIR}/${CORELIB_CONFIG_FILE}"
    "${CMAKE_CURRENT_BINARY_DIR}/${CORELIB_CONFIG_VERSION_FILE}"
    "${CORELIB_INSTALL_SOURCE_DIR}/compiler_test.f90"
    DESTINATION ${CORELIB_INSTALL_CONFIG_PATH}
    COMPONENT devel
)

