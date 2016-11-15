module corelib_argparse_constants

    implicit none

    integer, parameter :: ARGPARSE_ACTION_STORE = 0
    integer, parameter :: ARGPARSE_ACTION_STORE_TRUE = 2
    integer, parameter :: ARGPARSE_ACTION_STORE_FALSE = 1

    integer, parameter :: ARGPARSE_STATUS_OK = 0
    ! in intial state, no arguments added
    integer, parameter :: ARGPARSE_STATUS_INIT = 1
    integer, parameter :: ARGPARSE_STATUS_EMPTY_CMDLINE = 2
    integer, parameter :: ARGPARSE_STATUS_PARSE_ERROR = 4
    integer, parameter :: ARGPARSE_STATUS_PARSED = 8
    integer, parameter :: ARGPARSE_STATUS_UNKNOWN_ARGUMENT = 16
    integer, parameter :: ARGPARSE_STATUS_INCORRECT_NARGS = 32
    integer, parameter :: ARGPARSE_STATUS_UNKNOWN = 128

end module
