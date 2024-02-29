###############################################################################
# Set additional Fortran compiler FLAGS

unset(_FFLAGS)
unset(_FFLAGS_DEBUG)

if (CMAKE_C_COMPILER_ID STREQUAL "GNU" OR MINGW)
    set(_CFLAGS_DEBUG "-ggdb -O0")
    set(_CFLAGS "-Wall -Wextra")
endif()

if (CMAKE_Fortran_COMPILER_ID STREQUAL "GNU" OR MINGW)
    set(GNU_WARN "-Wall -Wextra -pedantic -Wimplicit-interface -Wimplicit-procedure -Warray-temporaries -Wrealloc-lhs")
    set(_FFLAGS "${GNU_WARN}")
    set(_FFLAGS_DEBUG "-O0 -ggdb")
elseif (CMAKE_Fortran_COMPILER_ID MATCHES "Intel(LLVM)?")
    if (WIN32)
        set(_FFLAGS "/warn:all")
    else ()
        set(_FFLAGS "-warn all")
        set(_FFLAGS_DEBUG "-O0 -traceback")
    endif (WIN32)
endif ()

set(CMAKE_Fortran_FLAGS "${_FFLAGS} ${CMAKE_Fortran_FLAGS}")
set(CMAKE_Fortran_FLAGS_DEBUG "${_FFLAGS_DEBUG} ${CMAKE_Fortran_FLAGS_DEBUG}")

set(CMAKE_C_FLAGS "${_CFLAGS} ${CMAKE_C_FLAGS}")
set(CMAKE_C_FLAGS_DEBUG "${_CFLAGS_DEBUG} ${CMAKE_C_FLAGS_DEBUG}")


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
            "$<INSTALL_INTERFACE:${CMAKE_INSTALL_INCLUDEDIR}/${INSTALL_DIR_NAME}>"
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

    # append created objects, to be used as input in main library
    list(APPEND COMPONENT_OBJECTS $<TARGET_OBJECTS:${_comp}>)
endforeach()

set(SOURCE_FILES
    ${CMAKE_BINARY_DIR}/fcore.f90
)

add_library(${LIBRARY_NAME} ${SOURCE_FILES} ${COMPONENT_OBJECTS})
set_target_properties(${LIBRARY_NAME} PROPERTIES
    VERSION ${PROJECT_VERSION}
)

if (BUILD_TESTS OR BUILD_EXAMPLES)
    # build static library so tests & examples can link against it
    add_library(${LIBRARY_NAME}_static STATIC 
        ${SOURCE_FILES}
        ${COMPONENT_OBJECTS}
    )

    set_target_properties(${LIBRARY_NAME}_static PROPERTIES
        RUNTIME_OUTPUT_DIRECTORY "${FCORE_STATIC_DIRECTORY}"
        LIBRARY_OUTPUT_DIRECTORY "${FCORE_STATIC_DIRECTORY}"
        ARCHIVE_OUTPUT_DIRECTORY "${FCORE_STATIC_DIRECTORY}"
        OUTPUT_NAME ${LIBRARY_NAME}
    )

    # Add public include directories, this will propagage to tests and 
    # examples.
    target_include_directories(${LIBRARY_NAME}_static
        PUBLIC
            "$<BUILD_INTERFACE:${CMAKE_Fortran_MODULE_DIRECTORY}>"
            "$<BUILD_INTERFACE:${CMAKE_SOURCE_DIR}/src/include>"
    )

endif ()