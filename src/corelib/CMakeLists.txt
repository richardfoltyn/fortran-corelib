
set(CORELIB_COMPONENTS argparse common collections datetime logging strings testing utils)

include_directories("${${PROJECT_NAME}_MODDIR}")

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

add_library(${LIBRARY_NAME} ${COMPONENT_OBJECTS})
set_target_properties(${LIBRARY_NAME} PROPERTIES
    OUTPUT_NAME ${LIBRARY_NAME}
    RUNTIME_OUTPUT_DIRECTORY "${CMAKE_LIBRARY_OUTPUT_DIRECTORY}"
)

# build static library so tests can link against it
if (BUILD_TESTS)
    add_library(${LIBRARY_NAME}_static STATIC ${COMPONENT_OBJECTS})
endif()

install(TARGETS ${LIBRARY_NAME}
    RUNTIME DESTINATION "${CMAKE_INSTALL_LIBDIR}"
    LIBRARY DESTINATION "${CMAKE_INSTALL_LIBDIR}"
    ARCHIVE DESTINATION "${CMAKE_INSTALL_LIBDIR}"
    LIBRARY INCLUDES DESTINATION "${CMAKE_INSTALL_INCLUDEDIR}"
    ARCHIVE INCLUDES DESTINATION "${CMAKE_INSTALL_INCLUDEDIR}"
)

install(DIRECTORY ${${PROJECT_NAME}_MODDIR}/
    DESTINATION "${CMAKE_INSTALL_INCLUDEDIR}/fcore"
)
