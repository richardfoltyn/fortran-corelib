module corelib_argparse_argument

    use iso_fortran_env
    use corelib_argparse_constants

    implicit none
    private

    type, public :: argument
        private
        type (str) :: name, abbrev
        class (*), dimension(:), allocatable :: default
        integer :: action = ARGPARSE_ACTION_STORE
        logical :: required = .false.
        logical :: is_present = .false.
        integer :: nargs = 1
        type (str), dimension(:), allocatable :: passed_values
    contains
        procedure, pass :: get_int32 => argument_get_int32
        generic, public :: get => get_int32

        procedure, public, pass :: parse => argument_parse
    end type

contains

subroutine argument_get_int32 (self, val, status)
    class (argument), intent(in), target :: self
    integer (int32), intent(out), dimension(:) :: val
    integer, intent(out), optional :: status

    integer (int32), pointer :: ptr_default
    integer :: lstatus, i

    lstatus = ARGPARSE_STATUS_PARSED

    if (size(val) < self%nargs) then
        lstatus = ARGPARSE_STATUS_INCORRECT_NARGS
        goto 100
    end if

    if (self%is_present) then
        do i = 1, self%nargs
            call self%passed_value(i)%parse (val(i), lstatus)
            if (lstatus /= STR_PARSE_SUCCESS) then
                lstatus = ARGPARSE_STATUS_PARSE_ERROR
                goto 100
            end if
        end do
    else if (allocated (self%default)) then
        call dynamic_cast (self%default, ptr_default)
        val(i) = ptr_default
    end if

100 continue
    if (present(status)) status = lstatus

end subroutine

! subroutine argument_parse_scalar (self, val, status)
!     class (argument), intent(in out) :: self
!     character (*), intent(in), optional :: val
!
!     if (self%action == ARGPARSE_ACTION_STORE) then
!         self
! end subroutine

! ------------------------------------------------------------------------------
! Casts

subroutine dynamic_cast_argument (tgt, ptr)
    class (*), intent(in), pointer :: obj
    class (argument), intent(out), pointer :: ptr

    select type (tgt)
    class is (argument)
        ptr => tgt
    class default
        error stop "Unsupported cast"
    end select
end subroutine

end module
