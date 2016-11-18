module corelib_common_casts

    use iso_fortran_env
    use corelib_common_constants

    implicit none
    private

    interface dynamic_cast
        module procedure cast_any_to_int32, cast_any_to_array_int32, &
            cast_any_to_int64, cast_any_to_array_int64, &
            cast_any_to_real32, cast_any_to_array_real32, &
            cast_any_to_real64, cast_any_to_array_real64, &
            cast_any_to_logical, cast_any_to_array_logical, &
            cast_any_to_char, cast_any_to_array_char
    end interface

    public :: dynamic_cast

contains

subroutine cast_any_to_int32 (tgt, ptr, status)
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

subroutine cast_any_to_array_int32 (tgt, ptr, status)
    integer, parameter :: INTSIZE = int32
    class (*), intent(in), dimension(:), target :: tgt
    integer (INTSIZE), intent(out), dimension(:), pointer :: ptr
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

subroutine cast_any_to_int64 (tgt, ptr, status)
    integer, parameter :: INTSIZE = int64
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

subroutine cast_any_to_array_int64 (tgt, ptr, status)
    integer, parameter :: INTSIZE = int64
    class (*), intent(in), dimension(:), target :: tgt
    integer (INTSIZE), intent(out), dimension(:), pointer :: ptr
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

subroutine cast_any_to_real32 (tgt, ptr, status)
    integer, parameter :: PREC = real32
    class (*), intent(in), target :: tgt
    real (PREC), intent(out), pointer :: ptr
    integer, intent(out), optional :: status

    integer :: lstatus

    lstatus = STATUS_UNSUPPORTED_OPERATION

    select type (tgt)
    type is (real(PREC))
        ptr => tgt
        lstatus = STATUS_OK
    end select

    if (present(status)) status = lstatus
end subroutine

subroutine cast_any_to_array_real32 (tgt, ptr, status)
    integer, parameter :: PREC = real32
    class (*), intent(in), dimension(:), target :: tgt
    real (PREC), intent(out), dimension(:), pointer :: ptr
    integer, intent(out), optional :: status

    integer :: lstatus

    lstatus = STATUS_UNSUPPORTED_OPERATION

    select type (tgt)
    type is (real(PREC))
        ptr => tgt
        lstatus = STATUS_OK
    end select

    if (present(status)) status = lstatus
end subroutine

subroutine cast_any_to_real64 (tgt, ptr, status)
    integer, parameter :: PREC = real64
    class (*), intent(in), target :: tgt
    real (PREC), intent(out), pointer :: ptr
    integer, intent(out), optional :: status

    integer :: lstatus

    lstatus = STATUS_UNSUPPORTED_OPERATION

    select type (tgt)
    type is (real(PREC))
        ptr => tgt
        lstatus = STATUS_OK
    end select

    if (present(status)) status = lstatus
end subroutine

subroutine cast_any_to_array_real64 (tgt, ptr, status)
    integer, parameter :: PREC = real64
    class (*), intent(in), dimension(:), target :: tgt
    real (PREC), intent(out), dimension(:), pointer :: ptr
    integer, intent(out), optional :: status

    integer :: lstatus

    lstatus = STATUS_UNSUPPORTED_OPERATION

    select type (tgt)
    type is (real(PREC))
        ptr => tgt
        lstatus = STATUS_OK
    end select

    if (present(status)) status = lstatus
end subroutine


subroutine cast_any_to_logical (tgt, ptr, status)
    class (*), intent(in), target :: tgt
    logical, intent(out), pointer :: ptr
    integer, intent(out), optional :: status

    integer :: lstatus

    lstatus = STATUS_UNSUPPORTED_OPERATION

    select type (tgt)
    type is (logical)
        ptr => tgt
        lstatus = STATUS_OK
    end select

    if (present(status)) status = lstatus
end subroutine

subroutine cast_any_to_array_logical (tgt, ptr, status)
    class (*), intent(in), dimension(:), target :: tgt
    logical, intent(out), dimension(:), pointer :: ptr
    integer, intent(out), optional :: status

    integer :: lstatus

    lstatus = STATUS_UNSUPPORTED_OPERATION

    select type (tgt)
    type is (logical)
        ptr => tgt
        lstatus = STATUS_OK
    end select

    if (present(status)) status = lstatus
end subroutine

subroutine cast_any_to_char (tgt, ptr, status)
    class (*), intent(in), target :: tgt
    character (*), intent(out), pointer :: ptr
    integer, intent(out), optional :: status

    integer :: lstatus

    lstatus = STATUS_UNSUPPORTED_OPERATION

    select type (tgt)
    type is (character (*))
        ptr => tgt
        lstatus = STATUS_OK
    end select

    if (present(status)) status = lstatus
end subroutine

subroutine cast_any_to_array_char (tgt, ptr, status)
    class (*), intent(in), dimension(:), target :: tgt
    character (*), intent(out), dimension(:), pointer :: ptr
    integer, intent(out), optional :: status

    integer :: lstatus

    lstatus = STATUS_UNSUPPORTED_OPERATION

    select type (tgt)
    type is (character (*))
        ptr => tgt
        lstatus = STATUS_OK
    end select

    if (present(status)) status = lstatus
end subroutine


end module
