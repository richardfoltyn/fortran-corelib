module corelib_argparse_constants

    use corelib_common_kinds, only: CL_ENUM_KIND

    implicit none

    integer (CL_ENUM_KIND), public, parameter :: ARGPARSE_ACTION_STORE = 0
    integer (CL_ENUM_KIND), public, parameter :: ARGPARSE_ACTION_STORE_TRUE = 1
    integer (CL_ENUM_KIND), public, parameter :: ARGPARSE_ACTION_STORE_FALSE = 2
    integer (CL_ENUM_KIND), public, parameter :: ARGPARSE_ACTION_STORE_CONST = 4
    integer (CL_ENUM_KIND), public, parameter :: ARGPARSE_ACTION_APPEND = 8

    integer (CL_ENUM_KIND), public, parameter :: ARGPARSE_CL_STATUS_OK = 0
    ! in intial state, no arguments added


end module
