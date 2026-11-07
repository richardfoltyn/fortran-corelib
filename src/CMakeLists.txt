configure_file(fcore.f90.in "${CMAKE_CURRENT_BINARY_DIR}/fcore.f90")

add_library(fcore "${CMAKE_CURRENT_BINARY_DIR}/fcore.f90")
add_library(fcore::fcore ALIAS fcore)
set_target_properties(fcore PROPERTIES
    VERSION "${PROJECT_VERSION}"
    Fortran_MODULE_DIRECTORY "${FCORE_MODULE_DIRECTORY}"
)
target_include_directories(fcore
    PRIVATE "${CMAKE_CURRENT_SOURCE_DIR}/include"
    PUBLIC
        "$<BUILD_INTERFACE:${FCORE_MODULE_DIRECTORY}>"
        "$<INSTALL_INTERFACE:${FCORE_INSTALL_INCLUDEDIR}>"
)
if(CMAKE_GENERATOR MATCHES "^Visual Studio")
    target_include_directories(fcore PUBLIC
        "$<BUILD_INTERFACE:${FCORE_MODULE_DIRECTORY}/$<CONFIG>>"
    )
endif()
fcore_configure_compiler_options(fcore)
if(FCORE_DIAGNOSTICS)
    target_compile_definitions(fcore PRIVATE "$<$<COMPILE_LANGUAGE:Fortran>:__DIAGNOSTICS>")
endif()

foreach(component IN ITEMS common collections argparse datetime io logging testing)
    add_subdirectory(${component})
endforeach()
