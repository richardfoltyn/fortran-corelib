# FindTargetArch.cmake

# FIND_TARGET_ARCH detects the the target architecture
# (currently only whether it's 32 or 64 bit)
# Upon completion, the following cache variables will be set:
# TARGET_ARCH_FOUND : bool
#   TRUE if target architecture could be determined
# TARGET_ARCH : string
#   "32" or "64" if architecture detected
# TARGET_ARCH_BITS : string
#   "32" or "64" if architecture detected
function(find_target_arch)
    set(SOURCE_CODE
    "
    program detect_arch

        use iso_c_binding
        use iso_fortran_env

        integer (c_intptr_t) :: i
        integer (int32) :: i32
        integer (int64) :: i64

        if (huge(i) == huge(i32)) then
            stop 32
        else if (huge(i) == huge(i64)) then
            stop 64
        else
            stop -1
        end if

    end program
    ")

    set(FIND_QUIETLY FALSE)
    set(ARCH_REQUIRED FALSE)
    if ("${ARGV0}" STREQUAL "QUIET")
        set(FIND_QUIETLY TRUE)
        set(MSG_MODE WARNING)
    elseif ("${ARGV0}" STREQUAL "REQUIRED")
        set(ARCH_REQUIRED TRUE)
        set(MSG_MODE FATAL_ERROR)
    endif()

    set(FILEPATH "${CMAKE_BINARY_DIR}/detect_arch.f90")

    if (NOT TARGET_ARCH)

        set(TARGET_ARCH_FOUND FALSE)

        file(WRITE "${FILEPATH}" "${SOURCE_CODE}")

        try_run(ARCH_DETECT_RUN_RESULT ARCH_DETECT_COMPILE_RESULT
            ${CMAKE_BINARY_DIR} "${FILEPATH}")

        file(REMOVE "${FILEPATH}")

        if (NOT ARCH_DETECT_COMPILE_RESULT)
            if (NOT FIND_QUIETLY OR ARCH_REQUIRED)
                message(${MSG_MODE} "Could not compile ARCH detection exectable")
            endif()
        else()
            if (ARCH_DETECT_RUN_RESULT STREQUAL "32")
                set(TARGET_ARCH_FOUND TRUE)
                set(TARGET_ARCH "32")
                set(TARGET_ARCH_BITS "32")
            elseif (ARCH_DETECT_RUN_RESULT STREQUAL "64")
                set(TARGET_ARCH_FOUND TRUE)
                set(TARGET_ARCH "64")
                set(TARGET_ARCH_BITS "64")
            else ()
                if (NOT FIND_QUIETLY OR ARCH_REQUIRED)
                    message(${MSG_MODE} "Unknown target ARCH")
                endif()
            endif ()
        endif ()

        set(TARGET_ARCH "${TARGET_ARCH}"
            CACHE STRING "Target architecture" FORCE)
        set(TARGET_ARCH_FOUND ${TARGET_ARCH_FOUND}
            CACHE BOOL "Whether target ARCH found" FORCE)
        set(TARGET_ARCH_BITS ${TARGET_ARCH_BITS}
            CACHE STRING "Target architecture, memomy bus width" FORCE)

        if (NOT FIND_QUIETLY AND TARGET_ARCH_FOUND)
            message(STATUS "Detected target ARCH: ${TARGET_ARCH} bit")
        endif()
    endif()
endfunction()
