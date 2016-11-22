module corelib_argparse_constants

    implicit none

    integer, parameter :: ARGPARSE_ACTION_STORE = 0
    integer, parameter :: ARGPARSE_ACTION_STORE_TRUE = 1
    integer, parameter :: ARGPARSE_ACTION_STORE_FALSE = 2
    integer, parameter :: ARGPARSE_ACTION_STORE_CONST = 4
    integer, parameter :: ARGPARSE_ACTION_APPEND = 8

    integer, parameter :: ARGPARSE_STATUS_OK = 0
    ! in intial state, no arguments added
    integer, parameter :: ARGPARSE_STATUS_INIT = - 1
    integer, parameter :: ARGPARSE_STATUS_EMPTY_CMDLINE = - 2
    integer, parameter :: ARGPARSE_STATUS_PARSE_ERROR = - 4
    integer, parameter :: ARGPARSE_STATUS_PARSED = - 2 ** 3
    integer, parameter :: ARGPARSE_STATUS_INSUFFICIENT_ARGS = - 2 ** 11

end module
