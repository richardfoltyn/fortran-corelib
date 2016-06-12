
unset(CORELIB_TESTS_SOURCE_FILES)

add_subdirectory(strings)

# include_directories("${CMAKE_Fortran_MODULE_DIRECTORY}")
add_executable(test_strings ${CORELIB_TESTS_SOURCE_FILES})
target_link_libraries(test_strings corelib_static)
