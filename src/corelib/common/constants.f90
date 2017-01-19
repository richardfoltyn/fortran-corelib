module corelib_common_constants

    use iso_fortran_env, only: int32

    implicit none
    private

    integer, public, parameter :: ENUM_KIND = int32

    integer (ENUM_KIND), public, parameter :: STATUS_OK = 0
    integer (ENUM_KIND), public, parameter :: STATUS_INVALID_INPUT = ishft(1, 10)
    integer (ENUM_KIND), public, parameter :: STATUS_UNKNOWN = ishft(1, 11)
    integer (ENUM_KIND), public, parameter :: STATUS_UNSUPPORTED_OPERATION = ishft(1, 12)
    integer (ENUM_KIND), public, parameter :: STATUS_INVALID_STATE = ishft(1, 13)


end module
