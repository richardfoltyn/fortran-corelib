

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

add_library(${COMPONENT_NAME} OBJECT ${SOURCE_FILES})

target_include_directories(${COMPONENT_NAME} BEFORE
    PRIVATE "${CMAKE_CURRENT_LIST_DIR}/include"
)
