module corelib_argparse_actions

    use corelib_common_kinds, only: CL_ENUM_KIND
    use corelib_common_base

    implicit none

    public :: get_action_label

    integer (CL_ENUM_KIND), public, parameter :: ARGPARSE_ACTION_STORE = 0
    integer (CL_ENUM_KIND), public, parameter :: ARGPARSE_ACTION_STORE_TRUE = 1
    integer (CL_ENUM_KIND), public, parameter :: ARGPARSE_ACTION_STORE_FALSE = 2
    integer (CL_ENUM_KIND), public, parameter :: ARGPARSE_ACTION_STORE_CONST = 4
    integer (CL_ENUM_KIND), public, parameter :: ARGPARSE_ACTION_APPEND = 8

contains

pure function get_action_label (code) result(res)
    integer (CL_ENUM_KIND), intent(in) :: code
    type (str) :: res

    select case (code)
    case (ARGPARSE_ACTION_STORE)
        res = "STORE"
    case (ARGPARSE_ACTION_STORE_TRUE)
        res = "STORE_TRUE"
    case (ARGPARSE_ACTION_STORE_FALSE)
        res = "STORE_FALSE"
    case (ARGPARSE_ACTION_STORE_CONST)
        res = "STORE_CONST"
    case (ARGPARSE_ACTION_APPEND)
        res = "APPEND"
    case default
        res = "UNKNOWN ACTION CODE"
    end select
end function

end module
