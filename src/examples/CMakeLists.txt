# src/examples/CMakeLists.txt
# Build file for examples

file(GLOB _dirs RELATIVE ${CMAKE_CURRENT_LIST_DIR} "*")

foreach (_dir IN LISTS _dirs)

    if (_dir STREQUAL "CMakeLists.txt")
        continue()
    endif()

    set(CMAKE_RUNTIME_OUTPUT_DIRECTORY
        "${${PROJECT_NAME}_EXAMPLES_DIRECTORY}/${_dir}"
    )

    unset(SOURCE_FILES)
    add_subdirectory("${_dir}")

    foreach (_file IN LISTS SOURCE_FILES)
        get_filename_component(exe_name "${_file}" NAME_WE)
    	add_executable(${exe_name} "${_dir}/${_file}")
    	target_link_libraries(${exe_name} ${LIBRARY_NAME}_static)
    endforeach()
endforeach()
