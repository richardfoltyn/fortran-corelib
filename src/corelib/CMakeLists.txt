
unset(CORELIB_SOURCE_FILES)

add_subdirectory(collections)
add_subdirectory(strings)
add_subdirectory(testing)

add_library(${LIBRARY_NAME}_static STATIC ${CORELIB_SOURCE_FILES})
set_target_properties(${LIBRARY_NAME}_static PROPERTIES OUTPUT_NAME ${LIBRARY_NAME})

add_library(${LIBRARY_NAME} SHARED ${CORELIB_SOURCE_FILES})

install(TARGETS ${LIBRARY_NAME}_static ${LIBRARY_NAME}
    RUNTIME DESTINATION lib
    LIBRARY DESTINATION lib
    ARCHIVE DESTINATION lib)

install(DIRECTORY ${${LIBRARY_NAME}_MODDIR}/ DESTINATION include/${LIBRARY_NAME})
