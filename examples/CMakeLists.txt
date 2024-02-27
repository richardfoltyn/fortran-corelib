# Build file for examples

set(COMPONENTS argparse io)

foreach (_comp IN LISTS COMPONENTS)

    unset(SOURCE_FILES)
    add_subdirectory(${_comp})

    foreach (_file IN LISTS SOURCE_FILES)
        get_filename_component(exe_name "${_file}" NAME_WE)
        set(TARGET_NAME examples_${_comp}_${exe_name})
    	
        add_executable(${TARGET_NAME} "${_comp}/${_file}"
            ${FCORE_COMPONENT_OBJECTS}
        )

        set_target_properties(${TARGET_NAME} PROPERTIES
            OUTPUT_NAME ${exe_name}
            RUNTIME_OUTPUT_DIRECTORY "${EXAMPLES_OUTPUT_DIRECTORY}/${_comp}"
        )
    endforeach()
endforeach()
