module corelib_argparse_parser

    use iso_fortran_env
    use corelib_strings
    use corelib_collections
    use corelib_utils

    use corelib_argparse_constants
    use corelib_argparse_argument

    implicit none
    private

    type, public :: argparser
        private

        type (linked_list), allocatable :: args
        type (str) :: description
        integer :: status = ARGPARSE_STATUS_INIT
    contains
        procedure, pass :: add_argument_str => argparser_add_argument_str
        procedure, pass :: add_argument_char => argparser_init_char
        generic, public :: add_argument => add_argument_str, add_argument_char

        procedure, pass :: init_str => argparser_init_str
        procedure, pass :: init_char => argparser_init_char
        generic, public :: init => init_str, init_char
        ! procedure, pass :: parse_args_str
        ! procedure, pass :: parse_args_cmdline
        !
        ! generic, public :: parse_args => parse_args_str, parse_args_cmdline
    end type



contains

! ------------------------------------------------------------------------------
! Initialization

pure subroutine argparser_init_str (self, description)
    class (ArgParser), intent(in out) :: self
    class (str), intent(in), optional :: description

    if (present(description)) self%description = description
end subroutine

pure subroutine argparser_init_char (self, description)
    class (ArgParser), intent(in out) :: self
    character (len=*), intent(in) :: description

    call self%init (str(description))
end subroutine

! ------------------------------------------------------------------------------
! Adding arguments
pure subroutine argparser_add_argument_str (self, name, action, default, required, help)
    class (ArgParser), intent(in out) :: self
    class (str), intent(in) :: name
    integer, intent(in), optional :: action
    class (*), intent(in), optional :: default
    logical, intent(in), optional :: required
    class (str), intent(in), optional :: help



end subroutine

pure subroutine argparser_add_argument_char (self, name, action, default, required, help)
    class (ArgParser), intent(in out) :: self
    character (*), intent(in) :: name, help
    class (*), intent(in), optional :: default
    integer, intent(in) :: action
    logical, intent(in) :: required

    optional :: action, required, help

    type (str) :: lhelp

    if (present(help)) lhelp = str(help)

    call self%add_argument (str(name), action, default, required, lhelp)
end subroutine

subroutine argparser_get_str (self, name, val, status)
    class (ArgParser), intent(in out) :: self
    type (str), intent(in) :: name
    class (*), intent(out) :: val
    integer, intent(out), optional :: status

    type (Argument), pointer :: ptr_arg
    ! deallocated automatically on subroutine exit
    class (iterator), allocatable :: iter
    class (*), pointer :: ptr_item

    ! supported value types
    integer (int32), pointer :: ptr_int32

    ! this is an error that should be trigged by the developer,
    ! no need to exit gracefully
    if (self%status == ARGPARSE_STATUS_INIT) then
        error stop "Need arguments have been specified"
    else if (self%status == ARGPARSE_STATUS_PARSE_ERROR) then
        if (present(status)) status = ARGPARSE_STATUS_PARSE_ERROR
        return
    else if (self%status /= ARGPARSE_STATUS_PARSED) then
        if (present(status)) status = ARGPARSE_STATUS_UNKNOWN
        write (ERROR_UNIT, *) "Unknown error encountered"
        return
    end if

    ! try to locate name in list of arguments
    ! get list iterator
    call self%args%get_iter (iter)

    nullify (ptr_arg)

    do while (iter%has_next())
        ptr_item => iter%item()
        call dynamic_cast (ptr_item, ptr_arg)

        if (ptr_arg%name == name) exit
        nullify (ptr_arg)
    end do

    if (.not. associated(ptr_arg)) then
        if (present(status)) status = ARGPARSE_STATUS_UNKNOWN_ARGUMENT
        write (ERROR_UNIT, *) "Unknown argument: " // name%to_char()
    end if

    ! at this point ptr_arg points to the argument identified by name
    select type (base => val)
    type is (integer(int32))
        call dynamic_cast (base, ptr_int32)
        call ptr_arg%get (ptr_int32)
    end select

end subroutine



end module
