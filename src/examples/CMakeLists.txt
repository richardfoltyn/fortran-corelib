# src/examples/CMakeLists.txt
# Build file for examples

set(COMPONENTS argparse io)

foreach (_comp IN LISTS COMPONENTS)

    set(CMAKE_RUNTIME_OUTPUT_DIRECTORY
        "${${PROJECT_NAME}_EXAMPLES_DIRECTORY}/${_comp}"
    )

    unset(SOURCE_FILES)
    add_subdirectory(${_comp})

    foreach (_file IN LISTS SOURCE_FILES)
        get_filename_component(exe_name "${_file}" NAME_WE)
        set(TARGET_NAME examples_${_comp}_${exe_name})
    	add_executable(${TARGET_NAME} "${_comp}/${_file}")
    	target_link_libraries(${TARGET_NAME} ${LIBRARY_NAME}_static)
        set_target_properties(${TARGET_NAME} PROPERTIES
            OUTPUT_NAME ${exe_name}
        )
    endforeach()
endforeach()
