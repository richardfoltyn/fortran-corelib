module corelib_utils_casts

    use iso_fortran_env
    implicit none
    private

    interface dynamic_cast
        module procedure cast_any_to_int32
    end interface

    public :: dynamic_cast

contains

subroutine cast_any_to_int32(from, to)
    class (*), intent(in), target :: from
    integer (int32), intent(out), pointer :: to

    select type (obj => from)
    type is (integer(int32))
        to => obj
    class default
        error stop "Unsupported cast"
    end select
end subroutine
end module
