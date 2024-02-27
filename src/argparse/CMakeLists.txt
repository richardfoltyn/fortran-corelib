

get_filename_component(COMPONENT_NAME "${CMAKE_CURRENT_SOURCE_DIR}" NAME)

set(SOURCE_FILES
    argparse.f90
    argument.F90
    argument_data.F90
    actions.f90
    parser.F90
    validators.F90
)


add_library(${COMPONENT_NAME} OBJECT ${SOURCE_FILES})
add_dependencies(${COMPONENT_NAME} common collections)

target_include_directories(${COMPONENT_NAME} BEFORE
    PRIVATE "${CMAKE_CURRENT_LIST_DIR}/include"
)