module corelib_common_casts

    use iso_fortran_env
    use corelib_common_constants

    implicit none
    private

    interface dynamic_cast
        module procedure cast_any_to_int32
    end interface

    public :: dynamic_cast

contains

subroutine cast_any_to_int32(tgt, ptr, status)
    integer, parameter :: INTSIZE = int32
    class (*), intent(in), target :: tgt
    integer (INTSIZE), intent(out), pointer :: ptr
    integer, intent(out), optional :: status

    integer :: lstatus

    lstatus = STATUS_UNSUPPORTED_OPERATION

    select type (tgt)
    type is (integer(INTSIZE))
        ptr => tgt
        lstatus = STATUS_OK
    end select

    if (present(status)) status = lstatus
end subroutine
end module
