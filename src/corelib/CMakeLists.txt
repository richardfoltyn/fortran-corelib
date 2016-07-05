
unset(CORELIB_SOURCE_FILES)

add_subdirectory(collections)
add_subdirectory(strings)
add_subdirectory(testing)

# build static library so tests can link against it
if (BUILD_TESTS)
    add_library(${LIBRARY_NAME}_static STATIC ${CORELIB_SOURCE_FILES})
endif()

add_library(${LIBRARY_NAME} ${CORELIB_SOURCE_FILES})
set_target_properties(${LIBRARY_NAME} PROPERTIES
    OUTPUT_NAME ${LIBRARY_NAME}
    RUNTIME_OUTPUT_DIRECTORY "${CMAKE_LIBRARY_OUTPUT_DIRECTORY}"
)

install(TARGETS ${LIBRARY_NAME}
    RUNTIME DESTINATION lib
    LIBRARY DESTINATION lib
    ARCHIVE DESTINATION lib)

install(DIRECTORY ${${LIBRARY_NAME}_MODDIR}/
    DESTINATION include/${INCLUDE_SUBDIR}/${LIBRARY_NAME})
