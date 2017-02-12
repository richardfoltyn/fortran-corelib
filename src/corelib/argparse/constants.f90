module corelib_argparse_constants

    use corelib_common, only: ENUM_KIND

    implicit none
    private

    integer (ENUM_KIND), public, parameter :: ARGPARSE_ACTION_STORE = 0
    integer (ENUM_KIND), public, parameter :: ARGPARSE_ACTION_STORE_TRUE = 1
    integer (ENUM_KIND), public, parameter :: ARGPARSE_ACTION_STORE_FALSE = 2
    integer (ENUM_KIND), public, parameter :: ARGPARSE_ACTION_STORE_CONST = 4
    integer (ENUM_KIND), public, parameter :: ARGPARSE_ACTION_APPEND = 8

    integer (ENUM_KIND), public, parameter :: ARGPARSE_STATUS_OK = 0
    ! in intial state, no arguments added
    integer (ENUM_KIND), public, parameter :: ARGPARSE_STATUS_INIT = ishft(1, 0)
    integer (ENUM_KIND), public, parameter :: ARGPARSE_STATUS_EMPTY_CMDLINE = ishft(1, 1)
    integer (ENUM_KIND), public, parameter :: ARGPARSE_STATUS_PARSE_ERROR = ishft(1, 2)
    integer (ENUM_KIND), public, parameter :: ARGPARSE_STATUS_PARSED = ishft(1, 3)
    integer (ENUM_KIND), public, parameter :: ARGPARSE_STATUS_INSUFFICIENT_ARGS = ishft(1, 4)
    integer (ENUM_KIND), public, parameter :: ARGPARSE_STATUS_UNKNOWN_ARG = ishft(1, 5)


end module
