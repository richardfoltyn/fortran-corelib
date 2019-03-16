module fcore_argparse_validators

    use, intrinsic :: iso_fortran_env

    use fcore_common
    use fcore_argparse_argument, only: fcn_validator

    implicit none
    private

    public :: validate_pos_int32
    public :: validate_nonneg_int32
    public :: validate_nonneg_real32
    public :: validate_nonneg_real64
    public :: validate_nonempty_str


    contains

subroutine validate_pos_int32 (val, status)
    !*  VALIDATE_INT32_POS verifies that argument value can be represented
    !   as a 32bit integer, and that its value is positive.
    type (str), intent(in) :: val
    type (status_t), intent(out) :: status

    call validate_bounds_int32 (val, status, lb=0, strict_lb=.true.)
end subroutine

subroutine validate_nonneg_int32 (val, status)
    !*  VALIDATE_INT32_POS verifies that argument value can be represented
    !   as a 32bit integer, and that its value is non-negative.
    type (str), intent(in) :: val
    type (status_t), intent(out) :: status

    call validate_bounds_int32 (val, status, lb=0)
end subroutine

subroutine validate_nonneg_real32 (val, status)
    type (str), intent(in) :: val
    type (status_t), intent(out) :: status

    call validate_bounds_real32 (val, status, lb=0.0)
end subroutine

subroutine validate_nonneg_real64 (val, status)
    type (str), intent(in) :: val
    type (status_t), intent(out) :: status

    call validate_bounds_real64 (val, status, lb=0.0d0)
end subroutine

subroutine validate_bounds_int32 (val, status, lb, ub, strict_lb, strict_ub)
    !*  Routine to check whether argument can be interpreted as 32-bit integer
    !   and its value is within a permitted interval.

    integer, parameter :: INTSIZE = int32
    type (str), intent(in) :: val
    type (status_t), intent(out) :: status
    integer (INTSIZE), intent(in), optional :: lb
    integer (INTSIZE), intent(in), optional :: ub

    integer (INTSIZE):: ival

#include "validate_bounds_impl.f90"
end subroutine

subroutine validate_bounds_int64 (val, status, lb, ub, strict_lb, strict_ub)

    integer, parameter :: INTSIZE = int64

    type (str), intent(in) :: val
    type (status_t), intent(out) :: status
    integer (INTSIZE), intent(in), optional :: lb
    integer (INTSIZE), intent(in), optional :: ub

    integer (INTSIZE):: ival

#include "validate_bounds_impl.f90"
end subroutine

subroutine validate_bounds_real32 (val, status, lb, ub, strict_lb, strict_ub)

    integer, parameter :: PREC = real32

    type (str), intent(in) :: val
    type (status_t), intent(out) :: status
    real (PREC), intent(in), optional :: lb
    real (PREC), intent(in), optional :: ub

    real (PREC) :: ival

#include "validate_bounds_impl.f90"
end subroutine

subroutine validate_bounds_real64 (val, status, lb, ub, strict_lb, strict_ub)

    integer, parameter :: PREC = real64

    type (str), intent(in) :: val
    type (status_t), intent(out) :: status
    real (PREC), intent(in), optional :: lb
    real (PREC), intent(in), optional :: ub

    real (PREC) :: ival

#include "validate_bounds_impl.f90"
end subroutine

subroutine validate_nonempty_str (val, status)
    type (str), intent(in) :: val
    type (status_t), intent(out) :: status

    status = FC_STATUS_OK
    if(len(val) == 0) then
        status = FC_STATUS_VALUE_ERROR
        status%msg = "Invalid value; non-empty string required"
    end if
end subroutine

end module
