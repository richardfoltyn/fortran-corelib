module corelib_common_casts

    use iso_fortran_env
    use corelib_common_constants
    use corelib_common_status

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

pure subroutine status_init (status)
    type (status_t), intent(out), optional :: status
    if (present(status)) then
        call status%clear ()
        status = CL_STATUS_OK
    end if
end subroutine

pure subroutine status_error (status, type_name)
    type (status_t), intent(out), optional :: status
    character (*), intent(in) :: type_name

    if (present(status)) then
        status = CL_STATUS_TYPE_ERROR
        status%msg = "Unsupported cast to " // type_name
    end if
end subroutine

subroutine cast_any_to_int32 (tgt, ptr, status)
    integer, parameter :: INTSIZE = int32
    class (*), intent(in), target :: tgt
    integer (INTSIZE), intent(out), pointer :: ptr
    type (status_t), intent(out), optional :: status

    call status_init (status)

    select type (tgt)
    type is (integer(INTSIZE))
        ptr => tgt
    class default
        call status_error (status, "int32")
    end select
end subroutine

subroutine cast_any_to_array_int32 (tgt, ptr, status)
    integer, parameter :: INTSIZE = int32
    class (*), intent(in), dimension(:), target :: tgt
    integer (INTSIZE), intent(out), dimension(:), pointer :: ptr
    type (status_t), intent(out), optional :: status

    call status_init (status)

    select type (tgt)
    type is (integer(INTSIZE))
        ptr => tgt
    class default
        call status_error (status, "int32(:)")
    end select
end subroutine

subroutine cast_any_to_int64 (tgt, ptr, status)
    integer, parameter :: INTSIZE = int64
    class (*), intent(in), target :: tgt
    integer (INTSIZE), intent(out), pointer :: ptr
    type (status_t), intent(out), optional :: status

    call status_init (status)

    select type (tgt)
    type is (integer(INTSIZE))
        ptr => tgt
    class default
        call status_error (status, "int64")
    end select
end subroutine

subroutine cast_any_to_array_int64 (tgt, ptr, status)
    integer, parameter :: INTSIZE = int64
    class (*), intent(in), dimension(:), target :: tgt
    integer (INTSIZE), intent(out), dimension(:), pointer :: ptr
    type (status_t), intent(out), optional :: status

    call status_init (status)

    select type (tgt)
    type is (integer(INTSIZE))
        ptr => tgt
    class default
        call status_error (status, "int64(:)")
    end select
end subroutine

subroutine cast_any_to_real32 (tgt, ptr, status)
    integer, parameter :: PREC = real32
    class (*), intent(in), target :: tgt
    real (PREC), intent(out), pointer :: ptr
    type (status_t), intent(out), optional :: status

    call status_init (status)

    select type (tgt)
    type is (real(PREC))
        ptr => tgt
    class default
        call status_error (status, "real32")
    end select
end subroutine

subroutine cast_any_to_array_real32 (tgt, ptr, status)
    integer, parameter :: PREC = real32
    class (*), intent(in), dimension(:), target :: tgt
    real (PREC), intent(out), dimension(:), pointer :: ptr
    type (status_t), intent(out), optional :: status

    call status_init (status)

    select type (tgt)
    type is (real(PREC))
        ptr => tgt
    class default
        call status_error (status, "real32(:)")
    end select
end subroutine

subroutine cast_any_to_real64 (tgt, ptr, status)
    integer, parameter :: PREC = real64
    class (*), intent(in), target :: tgt
    real (PREC), intent(out), pointer :: ptr
    type (status_t), intent(out), optional :: status

    call status_init (status)

    select type (tgt)
    type is (real(PREC))
        ptr => tgt
    class default
        call status_error (status, "real64")
    end select
end subroutine

subroutine cast_any_to_array_real64 (tgt, ptr, status)
    integer, parameter :: PREC = real64
    class (*), intent(in), dimension(:), target :: tgt
    real (PREC), intent(out), dimension(:), pointer :: ptr
    type (status_t), intent(out), optional :: status

    call status_init (status)

    select type (tgt)
    type is (real(PREC))
        ptr => tgt
    class default
        call status_error (status, "real64(:)")
    end select
end subroutine


subroutine cast_any_to_logical (tgt, ptr, status)
    class (*), intent(in), target :: tgt
    logical, intent(out), pointer :: ptr
    type (status_t), intent(out), optional :: status

    call status_init (status)

    select type (tgt)
    type is (logical)
        ptr => tgt
    class default
        call status_error (status, "logical")
    end select
end subroutine

subroutine cast_any_to_array_logical (tgt, ptr, status)
    class (*), intent(in), dimension(:), target :: tgt
    logical, intent(out), dimension(:), pointer :: ptr
    type (status_t), intent(out), optional :: status

    call status_init (status)

    select type (tgt)
    type is (logical)
        ptr => tgt
    class default
        call status_error (status, "logical(:)")
    end select
end subroutine

subroutine cast_any_to_char (tgt, ptr, status)
    class (*), intent(in), target :: tgt
    character (*), intent(out), pointer :: ptr
    type (status_t), intent(out), optional :: status

    call status_init (status)

    select type (tgt)
    type is (character (*))
        ptr => tgt
    class default
        call status_error (status, "character")
    end select
end subroutine

subroutine cast_any_to_array_char (tgt, ptr, status)
    class (*), intent(in), dimension(:), target :: tgt
    character (*), intent(out), dimension(:), pointer :: ptr
    type (status_t), intent(out), optional :: status

    call status_init (status)

    select type (tgt)
    type is (character (*))
        ptr => tgt
    class default
        call status_error (status, "character(:)")
    end select
end subroutine


end module
