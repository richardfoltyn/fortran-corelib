# src/tests/CMakeLists.txt
# Build file for unit tests

# Components which have unit tests
list(APPEND COMPONENTS
    argparse
    common
)

include_directories("${CMAKE_Fortran_MODULE_DIRECTORY}")

foreach (_comp IN LISTS COMPONENTS)

    unset(EXE_SOURCE_FILES)
    unset(SHARED_SOURCE_FILES)
    add_subdirectory(${_comp})

    set(CMAKE_RUNTIME_OUTPUT_DIRECTORY
        "${${PROJECT_NAME}_TESTS_DIRECTORY}/${_comp}")

    # Collect additional source files for this component and prepend
    # component directory
    if (SHARED_SOURCE_FILES)
        foreach (f IN LISTS SHARED_SOURCE_FILES)
            list(APPEND SHARED_SOURCE_FILES_PREFIXED "${_comp}/${f}")
        endforeach()
    endif()

    # Component-specific module directory to avoid any naming conflicts
    set(CMAKE_Fortran_MODULE_DIRECTORY
        "${${PROJECT_NAME}_MODDIR}/tests/${_comp}"
    )
    include_directories(BEFORE ${CMAKE_Fortran_MODULE_DIRECTORY})

    foreach (test_source IN LISTS EXE_SOURCE_FILES)
        get_filename_component(exe_name "${test_source}" NAME_WE)
        set(target_name  ${_comp}_${exe_name})
        add_executable(${target_name}
            "${_comp}/${test_source}"
            ${SHARED_SOURCE_FILES_PREFIXED}
        )
        target_link_libraries(${target_name} ${LIBRARY_NAME}_static)
        set_target_properties(${target_name} PROPERTIES
            OUTPUT_NAME ${exe_name}
        )

        add_test(NAME ${exe_name} COMMAND ${exe_name}
            WORKING_DIRECTORY "${${PROJECT_NAME}_TESTS_DIRECTORY}/${_comp}"
        )
    endforeach()
endforeach()
