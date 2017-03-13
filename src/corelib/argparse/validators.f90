module corelib_argparse_validators

    use, intrinsic :: iso_fortran_env
    use corelib_common
    use corelib_argparse_argument, only: fcn_validator
    implicit none
    private

    public :: validate_int32_pos
    public :: validate_int32_nonneg
    public :: validate_str_nonempty

    interface validate_bounds
        module procedure validate_int32_bounds
    end interface

contains

! ------------------------------------------------------------------------------
! Integer argument validators
subroutine validate_int32_pos (val, status)
    !*  VALIDATE_INT32_POS verifies that argument value can be represented
    !   as a 32bit integer, and that its value is positive.
    type (str), intent(in) :: val
    type (status_t), intent(out) :: status

    call validate_bounds (val, status, lb=1)
end subroutine

subroutine validate_int32_nonneg (val, status)
    !*  VALIDATE_INT32_POS verifies that argument value can be represented
    !   as a 32bit integer, and that its value is non-negative.
    type (str), intent(in) :: val
    type (status_t), intent(out) :: status

    call validate_bounds (val, status, lb=0)
end subroutine

subroutine validate_int32_bounds (val, status, lb, ub)
    !*  Routine to check whether argument can be interpreted as an interger,
    !   and optionally if this integer is within a specified range [lb, ub].

    integer, parameter :: INTSIZE = int32
    type (str), intent(in) :: val
    type (status_t), intent(out) :: status
    integer (INTSIZE), intent(in), optional :: lb
    integer (INTSIZE), intent(in), optional :: ub

    integer (INTSIZE):: ival

    call status%init (CL_STATUS_OK)

    call val%parse (ival, status)
    if (status /= CL_STATUS_OK) then
        status = CL_STATUS_VALUE_ERROR
        status%msg = "Invalid value; could not convert to integer"
        return
    end if

    if (present(lb)) then
        if (ival < lb) then
            status = CL_STATUS_VALUE_ERROR
            status%msg = "Invalid value; integer outside of permitted range"
            return
        end if
    end if
    if (present(ub)) then
        if (ival > ub) then
            status = CL_STATUS_VALUE_ERROR
            status%msg = "Invalid value; integer outside of permitted range"
            return
        end if
    end if
end subroutine

subroutine validate_str_nonempty (val, status)
    type (str), intent(in) :: val
    type (status_t), intent(out) :: status

    status = CL_STATUS_OK
    if(len(val) == 0) then
        status = CL_STATUS_VALUE_ERROR
        status%msg = "Invalid value; non-empty string required"
    end if
end subroutine

end module
