target_sources(fcore PRIVATE
    kinds.f90
    alloc_mod.f90
    common.f90
    constants.f90
    base_types.F90
    input_validation.F90
    status_helpers.F90
    strings.f90
)

target_include_directories(fcore PRIVATE "${CMAKE_CURRENT_SOURCE_DIR}/include")
