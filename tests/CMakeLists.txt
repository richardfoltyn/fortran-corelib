# Build file for unit tests

# Components which have unit tests
list(APPEND COMPONENTS
    argparse
    collections
    common
)

foreach (_comp IN LISTS COMPONENTS)

    unset(EXE_SOURCE_FILES)
    unset(SHARED_SOURCE_FILES)

    add_subdirectory(${_comp})

    # Collect additional source files for this component and prepend
    # component directory
    if (SHARED_SOURCE_FILES)
        foreach (f IN LISTS SHARED_SOURCE_FILES)
            list(APPEND SHARED_SOURCE_FILES_PREFIXED "${_comp}/${f}")
        endforeach()
    endif()

    foreach (test_source IN LISTS EXE_SOURCE_FILES)
        get_filename_component(exe_name "${test_source}" NAME_WE)
        set(target_name "test_${_comp}_${exe_name}")

        # Use object library targets from main project as "source" inputs,
        # no need to then link explicitly. This works irrespective of 
        # whether the main library is build as shared or static.
        add_executable(${target_name}
            "${_comp}/${test_source}"
            ${SHARED_SOURCE_FILES_PREFIXED}
            ${FCORE_COMPONENT_OBJECTS}
        )
        # Include directory where main lib modules are places
        target_include_directories(${target_name} PRIVATE 
            "${CMAKE_Fortran_MODULE_DIRECTORY}"
        )

        # Component-specific module directory to avoid any naming conflicts
        set_target_properties(${target_name} PROPERTIES
            OUTPUT_NAME ${exe_name}
            RUNTIME_OUTPUT_DIRECTORY "${TESTS_OUTPUT_DIRECTORY}/${_comp}"
        )

        add_test(NAME ${exe_name} COMMAND ${exe_name}
            WORKING_DIRECTORY "${TESTS_OUTPUT_DIRECTORY}/${_comp}"
        )
    endforeach()
endforeach()
