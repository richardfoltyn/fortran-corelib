
get_filename_component(COMPONENT_NAME "${CMAKE_CURRENT_SOURCE_DIR}" NAME)

set(SOURCE_FILES
    assertion_mod.f90
    corelib_testing.f90
    test_case_mod.f90
    test_suite_mod.f90
)

include_directories(${${PROJECT_NAME}_MODDIR})
add_library(${COMPONENT_NAME} OBJECT ${SOURCE_FILES})
add_dependencies(${COMPONENT_NAME} collections strings utils)
