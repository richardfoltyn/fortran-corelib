

get_filename_component(COMPONENT_NAME "${CMAKE_CURRENT_SOURCE_DIR}" NAME)

set(SOURCE_FILES
    collections.f90
    abc_mod.f90
    linked_list_mod.F90
)

include_directories(${${PROJECT_NAME}_MODDIR})

add_library(${COMPONENT_NAME} OBJECT ${SOURCE_FILES})
add_dependencies(${COMPONENT_NAME} common)

target_include_directories(${COMPONENT_NAME}
    PRIVATE
        "${CMAKE_Fortran_MODULE_DIRECTORY}"
)
