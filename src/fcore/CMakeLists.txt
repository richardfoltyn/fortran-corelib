
################################################################################
# Main library and its components

set(FCORE_COMPONENTS argparse common collections datetime
    io logging testing
)

include_directories("${${PROJECT_NAME}_MODDIR}")

# Update fcore module
configure_file(fcore.f90.in
    ${CMAKE_BINARY_DIR}/fcore.f90
)

set(COMPONENT_OBJECTS)

foreach(_comp IN LISTS FCORE_COMPONENTS)
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
    ${CMAKE_BINARY_DIR}/fcore.f90
)

add_library(${LIBRARY_NAME} ${SOURCE_FILES} ${COMPONENT_OBJECTS})
set_target_properties(${LIBRARY_NAME} PROPERTIES
    OUTPUT_NAME ${LIBRARY_NAME}
    RUNTIME_OUTPUT_DIRECTORY "${CMAKE_LIBRARY_OUTPUT_DIRECTORY}"
    VERSION ${FCORE_VERSION}
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

set(_version ${FCORE_VERSION_MAJOR}.${FCORE_VERSION_MINOR})
set(FCORE_INSTALL_DIR
    "${LIBRARY_NAME}-${_version}-${Fortran_COMPILER_SUFFIX}"
)

set(FCORE_INSTALL_CONFIG_PATH 
    ${CMAKE_INSTALL_LIBDIR}/cmake/${FCORE_INSTALL_DIR}
)

set(FCORE_INSTALL_NAMESPACE ${LIBRARY_NAME})

set(FCORE_CONFIG_FILE ${LIBRARY_NAME}-config.cmake)
set(FCORE_CONFIG_VERSION_FILE ${LIBRARY_NAME}-config-version.cmake)
set(FCORE_TARGETS_FILE ${LIBRARY_NAME}Targets.cmake)

configure_file(${FCORE_INSTALL_SOURCE_DIR}/config-version.cmake.in
    ${CMAKE_CURRENT_BINARY_DIR}/${FCORE_CONFIG_VERSION_FILE}
    @ONLY
)

configure_package_config_file(${FCORE_INSTALL_SOURCE_DIR}/config.cmake.in
    ${CMAKE_CURRENT_BINARY_DIR}/${FCORE_CONFIG_FILE}
    INSTALL_DESTINATION ${FCORE_INSTALL_CONFIG_PATH}
    PATH_VARS FCORE_INSTALL_CONFIG_PATH
)

# Note: INCLUDES DESTINATION is equivalent to adding
# INTERFACE_INCLUDE_DIRECTORIES property on exported targets.
install(TARGETS ${LIBRARY_NAME} 
    EXPORT ${LIBRARY_NAME}_targets
    RUNTIME DESTINATION "${CMAKE_INSTALL_LIBDIR}/${FCORE_INSTALL_DIR}"
    LIBRARY DESTINATION "${CMAKE_INSTALL_LIBDIR}/${FCORE_INSTALL_DIR}"
    ARCHIVE DESTINATION "${CMAKE_INSTALL_LIBDIR}/${FCORE_INSTALL_DIR}"
    INCLUDES DESTINATION
        "${CMAKE_INSTALL_INCLUDEDIR}/${FCORE_INSTALL_DIR}"
)

export(EXPORT ${LIBRARY_NAME}_targets
    FILE "${CMAKE_CURRENT_BINARY_DIR}/${FCORE_TARGETS_FILE}"
    NAMESPACE ${FCORE_INSTALL_NAMESPACE}::
)

install(EXPORT ${LIBRARY_NAME}_targets
    FILE ${FCORE_TARGETS_FILE}
    NAMESPACE ${FCORE_INSTALL_NAMESPACE}::
    DESTINATION ${FCORE_INSTALL_CONFIG_PATH}
)

# install compiler-specific MOD files
install(DIRECTORY ${${PROJECT_NAME}_MODDIR}/
    DESTINATION "${CMAKE_INSTALL_INCLUDEDIR}/${FCORE_INSTALL_DIR}"
    COMPONENT devel
)

install(FILES
    "${CMAKE_CURRENT_BINARY_DIR}/${FCORE_CONFIG_FILE}"
    "${CMAKE_CURRENT_BINARY_DIR}/${FCORE_CONFIG_VERSION_FILE}"
    "${FCORE_INSTALL_SOURCE_DIR}/compiler_test.f90"
    DESTINATION ${FCORE_INSTALL_CONFIG_PATH}
    COMPONENT devel
)

