# src/examples/CMakeLists.txt
# Build file for examples

set(COMPONENTS argparse)

foreach (_comp IN LISTS COMPONENTS)

    set(CMAKE_RUNTIME_OUTPUT_compECTORY
        "${${PROJECT_NAME}_EXAMPLES_compECTORY}/${_comp}"
    )

    unset(SOURCE_FILES)
    add_subdirectory(${_comp})

    foreach (_file IN LISTS SOURCE_FILES)
        get_filename_component(exe_name "${_file}" NAME_WE)
    	add_executable(${exe_name} "${_comp}/${_file}")
    	target_link_libraries(${exe_name} ${LIBRARY_NAME}_static)
    endforeach()
endforeach()
