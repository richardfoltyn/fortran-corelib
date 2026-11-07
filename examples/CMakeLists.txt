function(fcore_add_example name source)
    set(target fcore_example_${name})
    add_executable(${target} "${source}")
    target_link_libraries(${target} PRIVATE fcore::fcore)
    fcore_configure_compiler_options(${target})
endfunction()

add_subdirectory(argparse)
add_subdirectory(io)
