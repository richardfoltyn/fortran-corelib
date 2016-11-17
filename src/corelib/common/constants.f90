module corelib_common_constants

    use iso_fortran_env

    implicit none

    integer, private, parameter :: ENUM_KIND = int32

    integer (ENUM_KIND), parameter :: STATUS_OK = 0
    integer (ENUM_KIND), parameter :: STATUS_INVALID_INPUT = 2 ** 10
    integer (ENUM_KIND), parameter :: STATUS_UNKNOWN = 2 ** 11
    integer (ENUM_KIND), parameter :: STATUS_UNSUPPORTED_OPERATION = 2 * 12
    integer (ENUM_KIND), parameter :: STATUS_INVALID_STATE = 2 ** 13


end module
