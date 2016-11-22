# src/tests/CMakeLists.txt
# Build file for unit tests

set(TEST_DIRS argparse strings)
set(TEST_SOURCE_FILES_strings strings/test_str.f90)
set(TEST_SOURCE_FILES_argparse argparse/test_argparse.f90)

foreach (dir IN LISTS TEST_DIRS)

    set(CMAKE_RUNTIME_OUTPUT_DIRECTORY
        "${${PROJECT_NAME}_TESTS_DIRECTORY}/${dir}")

    set(TEST_SOURCE_FILES ${TEST_SOURCE_FILES_${dir}})

    foreach (test_source IN LISTS TEST_SOURCE_FILES)
        get_filename_component(exe_name "${test_source}" NAME_WE)
    	add_executable(${exe_name} "${test_source}")
    	target_link_libraries(${exe_name} ${LIBRARY_NAME}_static)
    	add_test(NAME ${exe_name} COMMAND ${exe_name}
            WORKING_DIRECTORY "${${PROJECT_NAME}_TESTS_DIRECTORY}/${dir}")
    endforeach()
endforeach()
