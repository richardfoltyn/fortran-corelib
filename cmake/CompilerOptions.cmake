# Keep project flags private: neither parent projects nor installed consumers
# should inherit fcore's warnings, language restrictions, or runtime checks.
function(fcore_configure_compiler_options target)
    target_compile_options(${target} PRIVATE
        "$<$<COMPILE_LANG_AND_ID:C,GNU>:-Wall;-Wextra>"
        "$<$<COMPILE_LANG_AND_ID:Fortran,GNU>:-std=f2018;-frecursive;-fimplicit-none;-Wall;-Wextra;-pedantic;-Wimplicit-interface;-Wimplicit-procedure;-Warray-temporaries;-Wrealloc-lhs>"
        "$<$<AND:$<COMPILE_LANG_AND_ID:Fortran,GNU>,$<CONFIG:Debug>>:-fcheck=all>"
    )

    if(WIN32)
        target_compile_options(${target} PRIVATE
            "$<$<COMPILE_LANG_AND_ID:Fortran,Intel,IntelLLVM>:/auto;/assume:recursion;/stand:f18;/warn:all>"
            "$<$<AND:$<COMPILE_LANG_AND_ID:Fortran,Intel,IntelLLVM>,$<CONFIG:Debug>>:/check:all;/traceback>"
        )
    else()
        target_compile_options(${target} PRIVATE
            "$<$<COMPILE_LANG_AND_ID:Fortran,Intel,IntelLLVM>:-auto;SHELL:-assume recursion;-std18;SHELL:-warn all>"
            "$<$<AND:$<COMPILE_LANG_AND_ID:Fortran,Intel,IntelLLVM>,$<CONFIG:Debug>>:SHELL:-check all;-traceback>"
        )
    endif()
endfunction()
