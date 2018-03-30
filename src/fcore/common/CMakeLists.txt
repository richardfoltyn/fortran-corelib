

get_filename_component(COMPONENT_NAME "${CMAKE_CURRENT_SOURCE_DIR}" NAME)

set(SOURCE_FILES
    kinds.f90
    alloc_mod.f90
    common.f90
    constants.f90
    base_types.F90
    input_validation.F90
    status_helpers.F90
    strings.f90
)

include_directories(${${PROJECT_NAME}_MODDIR}
    ${CMAKE_CURRENT_LIST_DIR}/include
)

add_library(${COMPONENT_NAME} OBJECT ${SOURCE_FILES})
