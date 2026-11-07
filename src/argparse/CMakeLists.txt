target_sources(fcore PRIVATE
    argparse.f90
    argument.F90
    argument_data.F90
    actions.f90
    parser.F90
    validators.F90
)

target_include_directories(fcore PRIVATE "${CMAKE_CURRENT_SOURCE_DIR}/include")
