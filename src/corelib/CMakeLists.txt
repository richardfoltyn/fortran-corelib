
unset(CORELIB_SOURCE_FILES)

add_subdirectory(collections)
add_subdirectory(strings)
add_subdirectory(testing)

set(CORELIB_MODDIR ${CMAKE_Fortran_MODULE_DIRECTORY})

add_library(corelib_static STATIC ${CORELIB_SOURCE_FILES})
add_library(corelib SHARED ${CORELIB_SOURCE_FILES})
