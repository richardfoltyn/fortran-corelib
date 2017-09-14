
get_filename_component(COMPONENT_NAME "${CMAKE_CURRENT_SOURCE_DIR}" NAME)

# List of files that should each be compiled into separate executable
set(EXE_SOURCE_FILES
    argparse.f90 
    PARENT_SCOPE
)
