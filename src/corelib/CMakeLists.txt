
set(CORELIB_COMPONENTS argparse common collections datetime
    io logging strings testing
)

include_directories("${${PROJECT_NAME}_MODDIR}")

# Update version module
configure_file(version.f90.in
	${CMAKE_BINARY_DIR}/version.f90
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
    ${CMAKE_BINARY_DIR}/version.f90
    corelib.f90
)

add_library(${LIBRARY_NAME} ${SOURCE_FILES} ${COMPONENT_OBJECTS})
set_target_properties(${LIBRARY_NAME} PROPERTIES
    OUTPUT_NAME ${LIBRARY_NAME}
    RUNTIME_OUTPUT_DIRECTORY "${CMAKE_LIBRARY_OUTPUT_DIRECTORY}"
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
    set(Fortran_COMPILER_SUFFIX "gfortran")
elseif (CMAKE_Fortran_COMPILER_ID STREQUAL "Intel")
    set(Fortran_COMPILER_SUFFIX "ifort")
else ()
    message(FATAL_ERROR "Unsupported Fortran compiler: ${CMAKE_Fortran_COMPILER_ID}")
endif ()

set(_version ${CORELIB_VERSION_MAJOR}.${CORELIB_VERSION_MINOR})
set(CORELIB_INSTALL_DIRNAME
    "${LIBRARY_NAME}-${_version}_${Fortran_COMPILER_SUFFIX}"
)

set(CORELIB_CONFIG_INSTALL_DIR 
    ${CMAKE_INSTALL_LIBDIR}/cmake/${CORELIB_INSTALL_DIRNAME})

configure_file(${CMAKE_CURRENT_SOURCE_DIR}/config-version.cmake.in
    ${CMAKE_CURRENT_BINARY_DIR}/${LIBRARY_NAME}-config-version.cmake
    @ONLY
)

configure_package_config_file(${CMAKE_CURRENT_SOURCE_DIR}/config.cmake.in
    ${CMAKE_CURRENT_BINARY_DIR}/${LIBRARY_NAME}-config.cmake
    INSTALL_DESTINATION ${CORELIB_CONFIG_INSTALL_DIR}
)

# Note: INCLUDES DESTINATION is equivalent to adding
# INTERFACE_INCLUDE_DIRECTORIES property on exported targets.
install(TARGETS ${LIBRARY_NAME} 
    EXPORT ${LIBRARY_NAME}_targets
    RUNTIME DESTINATION "${CMAKE_INSTALL_LIBDIR}/${CORELIB_INSTALL_DIRNAME}"
    LIBRARY DESTINATION "${CMAKE_INSTALL_LIBDIR}/${CORELIB_INSTALL_DIRNAME}"
    ARCHIVE DESTINATION "${CMAKE_INSTALL_LIBDIR}/${CORELIB_INSTALL_DIRNAME}"
    INCLUDES DESTINATION
        "${CMAKE_INSTALL_INCLUDEDIR}/${CORELIB_INSTALL_DIRNAME}"
)

export(EXPORT ${LIBRARY_NAME}_targets
    FILE "${CMAKE_CURRENT_BINARY_DIR}/${LIBRARY_NAME}Targets.cmake"
    NAMESPACE ${LIBRARY_NAME}::
)

install(EXPORT ${LIBRARY_NAME}_targets
    FILE ${LIBRARY_NAME}Targets.cmake
    NAMESPACE ${LIBRARY_NAME}::
    DESTINATION ${CORELIB_CONFIG_INSTALL_DIR}
)

# install compiler-specific MOD files
install(DIRECTORY ${${PROJECT_NAME}_MODDIR}/
    DESTINATION "${CMAKE_INSTALL_INCLUDEDIR}/${CORELIB_INSTALL_DIRNAME}"
    COMPONENT devel
)

install(FILES
    "${CMAKE_CURRENT_BINARY_DIR}/${LIBRARY_NAME}-config-version.cmake"
    "${CMAKE_CURRENT_BINARY_DIR}/${LIBRARY_NAME}-config.cmake"
    DESTINATION ${CORELIB_CONFIG_INSTALL_DIR}
    COMPONENT devel
)

