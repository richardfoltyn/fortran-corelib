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
        procedure, pass :: add_argument_scalar_str => argparser_add_argument_scalar_str
        procedure, pass :: add_argument_scalar_char => argparser_add_argument_scalar_char
        procedure, pass :: add_argument_array_str => argparser_add_argument_array_str
        procedure, pass :: add_argument_array_char => argparser_add_argument_array_char
        generic, public :: add_argument => add_argument_scalar_str, &
            add_argument_scalar_char, add_argument_array_str, add_argument_array_char

        procedure, pass :: init_str => argparser_init_str
        procedure, pass :: init_char => argparser_init_char
        generic, public :: init => init_str, init_char

        procedure, pass :: get_scalar_str => argparse_get_scalar_str
        procedure, pass :: get_scalar_char => argparse_get_scalar_char
        procedure, pass :: get_array_str => argparse_get_array_str
        procedure, pass :: get_array_char => argparse_get_array_char
        generic, public :: get => get_scalar_str, get_scalar_char, &
            get_array_str, get_array_char

        procedure, public, pass :: parse => argparse_parse

        procedure, pass :: append => argparser_append
        procedure, pass :: find_arg => argparser_find_arg
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
pure subroutine argparser_add_argument_array_str (self, name, abrev, action, &
        nargs, const, default, required, help, status)

    class (argparser), intent(in out) :: self
    class (str), intent(in) :: name
    class (str), intent(in), optional :: abbrev
    integer, intent(in), optional :: action
    integer, intent(in), optional :: nargs
    class (*), intent(in), dimension(:), optional :: const
    class (*), intent(in), dimension(:), optional :: default
    logical, intent(in), optional :: required
    class (str), intent(in), optional :: help
    integer, intent(out), optional :: status

    type (argument) :: arg
    integer :: lstatus

    call arg%init (name, abbrev, action, nargs, const, default, required, &
        help, lstatus)

    if (lstatus == ARGPARSE_STATUS_OK) then
        call self%append (arg)
    end if

    if (present(status)) status = lstatus

end subroutine

pure subroutine argparser_add_argument_scalar_str (self, name, abbrev, action, &
        nargs, const, default, required, help, status)

    class (argparser), intent(in out) :: self
    class (str), intent(in) :: name
    class (str), intent(in), optional :: abbrev
    integer, intent(in), optional :: action
    integer, intent(in), optional :: nargs
    class (*), intent(in), optional :: const
    class (*), intent(in), optional :: default
    logical, intent(in), optional :: required
    class (str), intent(in), optional :: help
    integer, intent(out), optional :: status

    type (argument) :: arg
    integer :: lstatus

    class (*), allocatable :: work1, work2

    allocate (work1(1), source=const)
    allocate (work2(1), source=default)

    call self%add_argument (name, abbrev, action, nargs, work1, work2, &
        required, help, status)

end subroutine

pure subroutine argparser_add_argument_scalar_char (self, name, abbrev, action, &
        nargs, const, default, required, help, status)

    class (argparser), intent(in out) :: self
    character (*), intent(in) :: name
    character (*), intent(in), optional :: abbrev
    integer, intent(in), optional :: action
    integer, intent(in), optional :: nargs
    class (*), intent(in), optional :: const
    class (*), intent(in), optional :: default
    logical, intent(in), optional :: required
    character (*), intent(in), optional :: help
    integer, intent(out), optional :: status

    type (str) :: lhelp, labbrev

    if (present(help)) lhelp = str(help)
    if (present(abbrev)) labbrev = str(abbrev)

    call self%add_argument (str(name), labbrev, action, nargs, const, default, &
        required, lhelp, status)
end subroutine

pure subroutine argparser_add_argument_array_char (self, name, abbrev, action, &
        nargs, const, default, required, help, status)

    class (argparser), intent(in out) :: self
    character (*), intent(in) :: name
    character (*), intent(in), optional :: abbrev
    integer, intent(in), optional :: action
    integer, intent(in), optional :: nargs
    class (*), intent(in), dimension(:), optional :: const
    class (*), intent(in), dimension(:), optional :: default
    logical, intent(in), optional :: required
    character (*), intent(in), optional :: help
    integer, intent(out), optional :: status

    type (str) :: lhelp, labbrev

    if (present(help)) lhelp = str(help)
    if (present(abbrev)) labbrev = str(abbrev)

    call self%add_argument (str(name), labbrev, action, nargs, const, default, &
        required, lhelp, status)
end subroutine

subroutine argparser_append (self, arg)
    class (argparser), intent(in out) :: self
    class (argument), intent(in) :: arg

    if (.not. allocated(self%args)) allocate (self%args)

    call self%args%append (arg)
end subroutine

! ------------------------------------------------------------------------------
! GET methods

subroutine argparser_get_array_str (self, name, val, status)
    class (argparser), intent(in out) :: self
    type (str), intent(in) :: name
    class (*), intent(out), dimension(:) :: val
    integer, intent(out), optional :: status

    class (argument), pointer :: ptr_arg
    character (100) :: msg

    integer :: lstatus

    ! this is an error that should be trigged by the developer,
    ! no need to exit gracefully
    if (self%status == ARGPARSE_STATUS_INIT) then
        error stop "No arguments have been specified"
    else if (self%status == ARGPARSE_STATUS_PARSE_ERROR) then
        lstatus = ARGPARSE_STATUS_PARSE_ERROR
        goto 100
    else if (self%status /= ARGPARSE_STATUS_PARSED) then
        lstatus = ARGPARSE_STATUS_UNKNOWN
        msg = "Unknown error encountered"
        goto 100
    end if

    call self%find_arg (name, ptr_arg)

    if (.not. associated(ptr_arg)) then
        lstatus = ARGPARSE_STATUS_UNKNOWN_ARGUMENT
        msg = "Unknown argument: " // name%to_char()
        goto 100
    end if

    ! at this point ptr_arg points to the argument identified by name.
    ! Retrieve stored argument value
    call ptr_arg%get (val, lstatus)

100 continue
    if (present(status)) status = lstatus
    if (len_trim(msg) > 0) write (ERROR_UNIT, fmt=*) msg
end subroutine

subroutine argparser_get_scalar_str (self, name, val, status)
    class (argparser), intent(in out) :: self
    type (str), intent(in) :: name
    class (*), intent(out) :: val
    integer, intent(out), optional :: status

    class (argument), pointer :: ptr_arg
    character (100) :: msg

    integer :: lstatus

    ! this is an error that should be trigged by the developer,
    ! no need to exit gracefully
    if (self%status == ARGPARSE_STATUS_INIT) then
        error stop "No arguments have been specified"
    else if (self%status == ARGPARSE_STATUS_PARSE_ERROR) then
        lstatus = ARGPARSE_STATUS_PARSE_ERROR
        goto 100
    else if (self%status /= ARGPARSE_STATUS_PARSED) then
        lstatus = ARGPARSE_STATUS_UNKNOWN
        msg = "Unknown error encountered"
        goto 100
    end if

    call self%find_arg (name, ptr_arg)

    if (.not. associated(ptr_arg)) then
        lstatus = ARGPARSE_STATUS_UNKNOWN_ARGUMENT
        msg = "Unknown argument: " // name%to_char()
        goto 100
    end if

    ! at this point ptr_arg points to the argument identified by name.
    ! Retrieve stored argument value
    call ptr_arg%get (val, lstatus)

100 continue
    if (present(status)) status = lstatus
    if (len_trim(msg) > 0) write (ERROR_UNIT, fmt=*) msg
end subroutine

subroutine argparser_get_array_char (self, name, val, status)
    class (argparser), intent(in out) :: self
    character (*), intent(in) :: name
    class (*), intent(out), dimension(:) :: val
    integer, intent(out), optional :: status

    call self%get (str(name), val, status)
end subroutine

subroutine argparser_get_scalar_char (self, name, val, status)
    class (argparser), intent(in out) :: self
    character (*), intent(in) :: name
    class (*), intent(out) :: val
    integer, intent(out), optional :: status

    call self%get (str(name), val, status)
end subroutine

! ------------------------------------------------------------------------------
! FIND_ARG method
subroutine argparser_find_arg (self, name, ptr_arg)
    class (argparser), intent(in) :: self
    class (str), intent(in) :: name
    class (argument), intent(out), pointer :: ptr_arg

    ! deallocated automatically on subroutine exit
    class (iterator), allocatable :: iter
    class (*), pointer :: ptr_item

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

end subroutine

! ------------------------------------------------------------------------------
! PARSE method

subroutine argparse_parse (self, status)
    class (argparse), intent(in out) :: self
    integer, intent(out), optional :: status

    integer :: lstatus
    character (100) :: msg


100 continue
    if (present(status)) status = lstatus
    if (len(msg) > 0) write (ERROR_UNIT, fmt=*) msg

end subroutine

end module
