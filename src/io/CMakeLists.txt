

get_filename_component(COMPONENT_NAME "${CMAKE_CURRENT_SOURCE_DIR}" NAME)

set(SOURCE_FILES
    io_impl.c
    io.f90
    dir.f90
    file.f90
    path.F90
)

add_library(${COMPONENT_NAME} OBJECT ${SOURCE_FILES})
add_dependencies(${COMPONENT_NAME} common)
