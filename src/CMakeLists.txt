###############################################################################
# Set additional Fortran compiler FLAGS

unset(_FFLAGS)
unset(_FFLAGS_Debug)

if (CMAKE_C_COMPILER_ID STREQUAL "GNU")
    set(_CFLAGS_Debug -ggdb -O0)
    set(_CFLAGS -Wall -Wextra)
endif()

if (CMAKE_Fortran_COMPILER_ID STREQUAL "GNU")
    set(GNU_WARN -Wall -Wextra -Wimplicit-interface -Wimplicit-procedure 
      -Warray-temporaries -Wrealloc-lhs)
    set(_FFLAGS ${GNU_WARN} -std=f2018 -pedantic -frecursive)
    set(_FFLAGS_Debug -fcheck=all -ggdb -O0)
elseif (CMAKE_Fortran_COMPILER_ID MATCHES "Intel(LLVM)?")
    if (WIN32)
        set(_FFLAGS /warn:all /stand:f18)
        set(_FFLAGS_Debug /check:all)
        # Disable seriously annoying warning that ifort is deprecated
        if (CMAKE_Fortran_COMPILER_ID STREQUAL "Intel")
            set(_FFLAGS ${_FFLAGS} /Qdiag-disable:10448)
        endif ()
    else ()
        set(_FFLAGS -warn all -std18)
        # Disable seriously annoying warning that ifort is deprecated
        if (CMAKE_Fortran_COMPILER_ID STREQUAL "Intel")
            set(_FFLAGS ${_FFLAGS} -diag-disable=10448)
        endif ()
        set(_FFLAGS_Debug -check all -O0 -traceback)
    endif (WIN32)
endif ()


################################################################################
# Main library and its components

# Update fcore version module
configure_file(fcore.f90.in
    ${CMAKE_BINARY_DIR}/fcore.f90
)

set(COMPONENT_OBJECTS)

set(FCORE_COMPONENTS argparse common collections datetime
    io logging testing
)

foreach(_comp IN LISTS FCORE_COMPONENTS)
    add_subdirectory(${_comp})

    target_include_directories(${_comp} 
        PRIVATE
            "${CMAKE_CURRENT_SOURCE_DIR}/include"
        PUBLIC
            "$<INSTALL_INTERFACE:${CMAKE_INSTALL_INCLUDEDIR}/${FCORE_INSTALL_DIR}>"
            "$<BUILD_INTERFACE:${CMAKE_Fortran_MODULE_DIRECTORY}>"
    )

    # we need to make sure that -fPIC etc. is passed to compiler when building
    # object libraries that will be used to create a shared lib
    if (BUILD_SHARED_LIBS)
        set_target_properties(${_comp} PROPERTIES POSITION_INDEPENDENT_CODE ON)
    endif ()

    if (DIAGNOSTICS)
        target_compile_definitions(${_comp} PRIVATE "__DIAGNOSTICS")
    endif()

    # Source files can be both C and Fortran, need to be careful to assign
    # correct flags
    target_compile_options(${_comp} BEFORE PRIVATE
        $<$<COMPILE_LANGUAGE:C>:${_CFLAGS}>
        $<$<COMPILE_LANGUAGE:C>:$<$<CONFIG:Debug>:${_CFLAGS_Debug}>>
        $<$<COMPILE_LANGUAGE:Fortran>:${_FFLAGS}>
        $<$<COMPILE_LANGUAGE:Fortran>:$<$<CONFIG:Debug>:${_FFLAGS_Debug}>>
    )

    # append created objects, to be used as input in main library
    list(APPEND COMPONENT_OBJECTS $<TARGET_OBJECTS:${_comp}>)
endforeach()

add_library(${LIBRARY_NAME} ${CMAKE_BINARY_DIR}/fcore.f90 ${COMPONENT_OBJECTS})
set_target_properties(${LIBRARY_NAME} PROPERTIES
    # RUNTIME_OUTPUT_DIRECTORY "${CMAKE_LIBRARY_OUTPUT_DIRECTORY}"
    VERSION ${PROJECT_VERSION}
)

# Store list of component object libraries in root folder so these can be 
# added as "sources" to unit tests
set(FCORE_COMPONENT_OBJECTS ${COMPONENT_OBJECTS} PARENT_SCOPE)