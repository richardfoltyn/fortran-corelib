! gfortran up to v5.x incorrectly processes procedure calls if the actual argument
! is a temporary array of user-derived type, and the dummy argument is
! polymorphic; see https://gcc.gnu.org/bugzilla/show_bug.cgi?id=60322
#if __GFORTRAN__ && (__GNUC__  < 6)
#define _POLYMORPHIC_ARRAY(t) type (t)
#else
#define _POLYMORPHIC_ARRAY(t) class (t)
#endif


module corelib_argparse_parser

    use iso_fortran_env
    use corelib_common
    use corelib_strings
    use corelib_collections

    use corelib_argparse_constants
    use corelib_argparse_argument

    implicit none
    private

    ! buffer size used to read in command line arguments
    integer, parameter :: CMD_BUFFER_SIZE = 1024
    ! buffer for printing error messages to stderr
    integer, parameter :: MSG_LENGTH = 100
    ! seperator used to split a list of arguments into individual tokens
    character (1), parameter :: LIST_SEP = ','

    type, public :: argparser
        private

        type (linked_list), allocatable :: args
        type (str) :: description
        integer :: status = ARGPARSE_STATUS_INIT
    contains
        procedure, pass :: argparser_add_argument_array_str
        procedure, pass :: argparser_add_argument_scalar_default_str
        procedure, pass :: argparser_add_argument_scalar_str
        procedure, pass :: argparser_add_argument_array_char
        procedure, pass :: argparser_add_argument_scalar_char
        procedure, pass :: argparser_add_argument_scalar_default_char
        generic, public :: add_argument => argparser_add_argument_array_str, &
            argparser_add_argument_scalar_default_str, &
            argparser_add_argument_scalar_str, &
            argparser_add_argument_array_char, &
            argparser_add_argument_scalar_char, &
            argparser_add_argument_scalar_default_char

        procedure, pass :: init_str => argparser_init_str
        procedure, pass :: init_char => argparser_init_char
        generic, public :: init => init_str, init_char

        procedure, public, pass :: reset => argparser_reset
        procedure, pass :: reset_args => argparser_reset_args

        procedure, pass :: argparser_get_array_str
        procedure, pass :: argparser_get_scalar_str
        procedure, pass :: argparser_get_array_char
        procedure, pass :: argparser_get_scalar_char
        generic, public :: get => argparser_get_array_str, &
            argparser_get_scalar_str, &
            argparser_get_array_char, &
            argparser_get_scalar_char

        procedure, pass :: argparser_parse_array
        procedure, pass :: argparser_parse_cmd
        generic, public :: parse => argparser_parse_array, argparser_parse_cmd

        procedure, pass :: append => argparser_append
        procedure, pass :: find_arg => argparser_find_arg
        procedure, pass :: parse_long => argparser_parse_long
        procedure, pass :: parse_abbrev => argparser_parse_abbrev
        procedure, pass :: collect_values => argparser_collect_values
        procedure, pass :: length => argparser_length

        procedure, pass :: argparser_check_input_str
        procedure, pass :: argparser_check_input_char
        generic :: check_input => argparser_check_input_str, argparser_check_input_char

        procedure, pass :: argparser_is_present_str
        procedure, pass :: argparser_is_present_char
        generic, public :: is_present => argparser_is_present_str, &
            argparser_is_present_char

        procedure, pass :: argparser_is_defined_str
        procedure, pass :: argparser_is_defined_char
        generic, public :: is_defined => argparser_is_defined_str, &
            argparser_is_defined_char

        procedure, pass :: argparser_get_nargs_str
        procedure, pass :: argparser_get_nargs_char
        generic, public :: get_nargs => argparser_get_nargs_str, &
            argparser_get_nargs_char
    end type

    interface len
        module procedure argparser_len
    end interface

    public :: len

contains

! ------------------------------------------------------------------------------
! Initialization

pure subroutine argparser_init_str (self, description)
    class (argparser), intent(in out) :: self
    class (str), intent(in), optional :: description

    call self%reset ()

    if (present(description)) self%description = description
end subroutine

pure subroutine argparser_init_char (self, description)
    class (argparser), intent(in out) :: self
    character (len=*), intent(in) :: description

    call self%init (str(description))
end subroutine

pure subroutine argparser_reset (self)
    class (argparser), intent(in out) :: self

    if (allocated(self%args)) deallocate(self%args)

    self%description = ""
    self%status = ARGPARSE_STATUS_INIT
end subroutine

! ------------------------------------------------------------------------------
! Length method, len() generic

pure function argparser_length (self) result(res)
    class (argparser), intent(in) :: self
    integer :: res
    res = 0
    if (allocated(self%args)) res = len(self%args)
end function

pure function argparser_len (obj) result(res)
    class (argparser), intent(in) :: obj
    integer :: res
    res = obj%length ()
end function

! ------------------------------------------------------------------------------
! Adding arguments
subroutine argparser_add_argument_array_str (self, name, abbrev, action, &
        nargs, required, help, status, default, const)

    class (argparser), intent(in out) :: self
    class (str), intent(in) :: name
    class (str), intent(in), optional :: abbrev
    integer, intent(in), optional :: action
    integer, intent(in), optional :: nargs
    logical, intent(in), optional :: required
    class (str), intent(in), optional :: help
    integer, intent(out), optional :: status
    class (*), intent(in), dimension(:), optional :: default
    class (*), intent(in), dimension(:), optional :: const

    type (argument) :: arg
    integer :: lstatus
    character (MSG_LENGTH) :: msg

    msg = ""

    call self%check_input (name, abbrev, action, nargs, lstatus, msg)
    if (lstatus /= STATUS_OK) goto 100

    call arg%init (name, abbrev, action, required, nargs, &
        help, lstatus, default, const)

    if (lstatus == STATUS_OK) call self%append (arg)

100 continue
    if (present(status)) status = lstatus
    call print_error (msg)
end subroutine

subroutine argparser_add_argument_scalar_default_str (self, name, abbrev, action, &
        nargs, required, help, status, default)

    class (argparser), intent(in out) :: self
    class (str), intent(in) :: name
    class (str), intent(in), optional :: abbrev
    integer, intent(in), optional :: action
    integer, intent(in), optional :: nargs
    logical, intent(in), optional :: required
    class (str), intent(in), optional :: help
    integer, intent(out), optional :: status
    class (*), intent(in) :: default

    type (argument) :: arg
    integer :: lstatus
    character (MSG_LENGTH) :: msg

    msg = ""

    call self%check_input (name, abbrev, action, nargs, lstatus, msg)
    if (lstatus /= STATUS_OK) goto 100

    call arg%init (name, abbrev, action, required, nargs, help, lstatus, default)

    if (lstatus == STATUS_OK) then
        call self%append (arg)
    end if

100 continue
    if (present(status)) status = lstatus
    call print_error (msg)
end subroutine

subroutine argparser_add_argument_scalar_str (self, name, abbrev, action, &
        nargs, required, help, status, default, const)

    class (argparser), intent(in out) :: self
    class (str), intent(in) :: name
    class (str), intent(in), optional :: abbrev
    integer, intent(in), optional :: action
    integer, intent(in), optional :: nargs
    logical, intent(in), optional :: required
    class (str), intent(in), optional :: help
    integer, intent(out), optional :: status
    class (*), intent(in) :: default, const

    type (argument) :: arg
    integer :: lstatus
    character (MSG_LENGTH) :: msg

    msg = ""

    call self%check_input (name, abbrev, action, nargs, lstatus, msg)
    if (lstatus /= STATUS_OK) goto 100

    call arg%init (name, abbrev, action, required, nargs, help, lstatus, default, const)

    if (lstatus == STATUS_OK) call self%append (arg)

100 continue
    if (present(status)) status = lstatus
    call print_error (msg)
end subroutine

subroutine argparser_add_argument_scalar_default_char (self, name, abbrev, action, &
        nargs, required, help, status, default)

    class (argparser), intent(in out) :: self
    character (*), intent(in) :: name
    character (*), intent(in), optional :: abbrev
    integer, intent(in), optional :: action
    integer, intent(in), optional :: nargs
    logical, intent(in), optional :: required
    character (*), intent(in), optional :: help
    integer, intent(out), optional :: status
    class (*), intent(in) :: default

    type (str) :: lhelp, labbrev, lname
    type (argument) :: arg
    integer :: lstatus
    character (MSG_LENGTH) :: msg

    msg = ""

    call self%check_input (name, abbrev, action, nargs, lstatus, msg)
    if (lstatus /= STATUS_OK) goto 100

    lname = name
    if (present(help)) lhelp = str(help)
    if (present(abbrev)) labbrev = str(abbrev)

    call arg%init (lname, labbrev, action, required, nargs, lhelp, lstatus, default)
    if (lstatus == STATUS_OK) call self%append (arg)

100 continue
    if (present(status)) status = lstatus
    call print_error (msg)
end subroutine

subroutine argparser_add_argument_scalar_char (self, name, abbrev, action, &
        nargs, required, help, status, default, const)

    class (argparser), intent(in out) :: self
    character (*), intent(in) :: name
    character (*), intent(in), optional :: abbrev
    integer, intent(in), optional :: action
    integer, intent(in), optional :: nargs
    logical, intent(in), optional :: required
    character (*), intent(in), optional :: help
    integer, intent(out), optional :: status
    class (*), intent(in) :: default, const

    type (str) :: lhelp, labbrev, lname
    type (argument) :: arg
    integer :: lstatus
    character (MSG_LENGTH) :: msg

    msg = ""

    call self%check_input (name, abbrev, action, nargs, lstatus, msg)
    if (lstatus /= STATUS_OK) goto 100

    lname = name
    if (present(help)) lhelp = str(help)
    if (present(abbrev)) labbrev = str(abbrev)

    call arg%init (lname, labbrev, action, required, nargs, lhelp, lstatus, &
        default, const)

    if (lstatus == STATUS_OK) call self%append (arg)

100 continue
    if (present(status)) status = lstatus
    call print_error (msg)
end subroutine

subroutine argparser_add_argument_array_char (self, name, abbrev, action, &
        nargs, required, help, status, const, default)

    class (argparser), intent(in out) :: self
    character (*), intent(in) :: name
    character (*), intent(in), optional :: abbrev
    integer, intent(in), optional :: action
    integer, intent(in), optional :: nargs
    logical, intent(in), optional :: required
    character (*), intent(in), optional :: help
    integer, intent(out), optional :: status
    class (*), intent(in), dimension(:), optional :: const
    class (*), intent(in), dimension(:), optional :: default

    type (str) :: lhelp, labbrev, lname
    type (argument) :: arg
    integer :: lstatus
    character (MSG_LENGTH) :: msg

    msg = ""

    call self%check_input (name, abbrev, action, nargs, lstatus, msg)
    if (lstatus /= STATUS_OK) goto 100

    lname = name
    if (present(help)) lhelp = str(help)
    if (present(abbrev)) labbrev = str(abbrev)

    call arg%init (lname, labbrev, action, required, nargs, lhelp, lstatus, &
        default, const)

    if (lstatus == STATUS_OK) call self%append (arg)

100 continue
    if (present(status)) status = lstatus
    call print_error (msg)
end subroutine

subroutine argparser_check_input_str (self, name, abbrev, action, nargs, status, msg)
    class (argparser), intent(in) :: self
    class (str), intent(in) :: name
    class (str), intent(in), optional :: abbrev
    integer, intent(in), optional :: action, nargs
    integer, intent(out) :: status
    character (*), intent(out) :: msg

    ! by default return invalid input status
    status = STATUS_INVALID_INPUT

    if (len_trim(name) == 0) then
        msg = "Invalid argument: name"
        return
    end if

    ! check whether argument with this name is already defined
    if (self%is_defined (name)) then
        msg = "Argument '" // name // "' already defined"
        return
    end if

    if (present(abbrev)) then
        if (len_trim(abbrev) == 0) then
            msg = "Invalid argument: abbrev"
            return
        end if

        ! check whether abbreviation of this name already exists
        if (self%is_defined (abbrev, is_abbrev=.true.)) then
            msg = "Argument with abbreviation '" // abbrev // "' already defined"
        end if
    end if

    call validate_action (action, status)
    if (status /= STATUS_OK) then
        msg = "Invalid argument: action"
        return
    end if

    if (present(nargs)) then
        if (nargs < 0) then
            msg = "Invalid argument: nargs"
            return
        end if
    end if

    status = STATUS_OK
end subroutine

subroutine argparser_check_input_char (self, name, abbrev, action, nargs, status, msg)
    class (argparser), intent(in) :: self
    character (*), intent(in) :: name
    character (*), intent(in), optional :: abbrev
    integer, intent(in), optional :: action, nargs
    integer, intent(out) :: status
    character (*), intent(out) :: msg

    type (str) :: labbrev, lname
    lname = name
    if (present (abbrev)) then
        labbrev = abbrev
        call self%check_input (lname, labbrev, action, nargs, status, msg)
    else
        call self%check_input (lname, action=action, nargs=nargs, &
            status=status, msg=msg)
    end if
end subroutine

subroutine validate_action (action, status)
    integer, intent(in), optional :: action
    integer, intent(out) :: status

    status = STATUS_OK
    if (.not. present(action)) return
    select case (action)
    case (ARGPARSE_ACTION_STORE)
        return
    case (ARGPARSE_ACTION_STORE_TRUE)
        return
    case (ARGPARSE_ACTION_STORE_FALSE)
        return
    case (ARGPARSE_ACTION_STORE_CONST)
        return
    case (ARGPARSE_ACTION_APPEND)
        return
    case default
        status = STATUS_INVALID_INPUT
        return
    end select
end subroutine

! ------------------------------------------------------------------------------
! APPEND method
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

    msg = ""

    if (len(self) == 0) then
        lstatus = STATUS_INVALID_STATE
        msg = "No arguments have been specified"
        goto 100
    else if (self%status == ARGPARSE_STATUS_PARSE_ERROR) then
        lstatus = ARGPARSE_STATUS_PARSE_ERROR
        goto 100
    else if (self%status /= ARGPARSE_STATUS_PARSED) then
        lstatus = STATUS_UNKNOWN
        msg = "Unknown error encountered"
        goto 100
    end if

    ptr_arg => self%find_arg (name)

    if (.not. associated(ptr_arg)) then
        lstatus = STATUS_INVALID_INPUT
        msg = "Unknown argument: " // name%to_char()
        goto 100
    end if

    ! at this point ptr_arg points to the argument identified by name.
    ! Retrieve stored argument value
    call ptr_arg%parse (val, lstatus, msg)

100 continue
    if (present(status)) status = lstatus
    if (len_trim(msg) > 0) write (ERROR_UNIT, fmt=*) trim(msg)
end subroutine

subroutine argparser_get_scalar_str (self, name, val, status)
    class (argparser), intent(in out) :: self
    type (str), intent(in) :: name
    class (*), intent(out) :: val
    integer, intent(out), optional :: status

    class (argument), pointer :: ptr_arg
    character (100) :: msg
    integer :: lstatus

    msg = ""

    if (len(self) == 0) then
        lstatus = STATUS_INVALID_STATE
        msg = "No arguments have been specified"
        goto 100
    else if (self%status == ARGPARSE_STATUS_PARSE_ERROR) then
        lstatus = ARGPARSE_STATUS_PARSE_ERROR
        goto 100
    else if (self%status /= ARGPARSE_STATUS_PARSED) then
        lstatus = STATUS_UNKNOWN
        msg = "Unknown error encountered"
        goto 100
    end if

    ptr_arg => self%find_arg (name)

    if (.not. associated(ptr_arg)) then
        lstatus = STATUS_INVALID_INPUT
        msg = "Unknown argument: " // name%to_char()
        goto 100
    end if

    ! at this point ptr_arg points to the argument identified by name.
    ! Retrieve stored argument value
    call ptr_arg%parse (val, lstatus, msg)

100 continue
    if (present(status)) status = lstatus
    if (len_trim(msg) > 0) write (ERROR_UNIT, fmt=*) trim(msg)
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
function argparser_find_arg (self, name, is_abbrev) result(ptr_arg)
    class (argparser), intent(in) :: self
    class (str), intent(in) :: name
    logical, intent(in), optional :: is_abbrev
    class (argument), pointer :: ptr_arg

    ! deallocated automatically on subroutine exit
    class (iterator), allocatable :: iter
    class (*), pointer :: ptr_item

    logical :: lis_abbrev

    lis_abbrev = .false.
    if (present(is_abbrev)) lis_abbrev = is_abbrev

    nullify (ptr_arg)

    if (len(self) == 0) return

    ! try to locate name in list of arguments
    ! get list iterator
    call self%args%get_iter (iter)

    do while (iter%has_next())
        ptr_item => iter%item()
        call dynamic_cast (ptr_item, ptr_arg)

        if (lis_abbrev) then
            if (ptr_arg%abbrev == name) exit
        else
            if (ptr_arg%name == name) exit
        end if
        nullify (ptr_arg)
    end do

end function

! ------------------------------------------------------------------------------
! IS_DEFINED method

function argparser_is_defined_str (self, identifier, is_abbrev) result (res)
    class (argparser), intent(in) :: self
    type (str), intent(in) :: identifier
    logical, intent(in), optional :: is_abbrev
    logical :: res

    type (argument), pointer :: ptr_arg

    nullify (ptr_arg)
    ptr_arg => self%find_arg (identifier, is_abbrev)

    res = associated (ptr_arg)
end function

function argparser_is_defined_char (self, identifier, is_abbrev) result(res)
    class (argparser), intent(in) :: self
    character (*), intent(in) :: identifier
    logical, intent(in), optional :: is_abbrev
    logical :: res

    res = self%is_defined (str(identifier), is_abbrev)
end function

! ------------------------------------------------------------------------------
! GET_NARGS method

subroutine argparser_get_nargs_str (self, identifier, nargs, is_abbrev, status)
    class (argparser), intent(in) :: self
    type (str), intent(in) :: identifier
    integer, intent(out) :: nargs
    logical, intent(in), optional :: is_abbrev
    integer, intent(out), optional :: status

    integer :: lstatus
    type (argument), pointer :: ptr_arg

    nargs = 0

    nullify (ptr_arg)
    ptr_arg => self%find_arg (identifier, is_abbrev)

    if (associated(ptr_arg)) then
        lstatus = STATUS_OK
        nargs = ptr_arg%get_cmd_nargs ()
    else
        lstatus = STATUS_INVALID_INPUT
    end if

    if (present(status)) status = lstatus
end subroutine


subroutine argparser_get_nargs_char (self, identifier, nargs, is_abbrev, status)
    class (argparser), intent(in) :: self
    character (*), intent(in) :: identifier
    integer, intent(out) :: nargs
    logical, intent(in), optional :: is_abbrev
    integer, intent(out), optional :: status

    call self%get_nargs (str(identifier), nargs, is_abbrev, status)
end subroutine

! ------------------------------------------------------------------------------
! IS_PRESENT method

subroutine argparser_is_present_str (self, identifier, is_present, is_abbrev, status)
    class (argparser), intent(in) :: self
    type (str), intent(in) :: identifier
    logical, intent(out) :: is_present
    logical, intent(in), optional :: is_abbrev
    integer, intent(out), optional :: status

    type (argument), pointer :: ptr_arg
    integer :: lstatus

    is_present = .false.

    nullify (ptr_arg)
    ptr_arg => self%find_arg (identifier, is_abbrev)

    if (associated(ptr_arg)) then
        lstatus = STATUS_OK
        is_present = ptr_arg%is_present
    else
        lstatus = STATUS_INVALID_INPUT
    end if

    if (present(status)) status = lstatus
end subroutine


subroutine argparser_is_present_char (self, identifier, is_present, is_abbrev, status)
    class (argparser), intent(in) :: self
    character (*), intent(in) :: identifier
    logical, intent(out) :: is_present
    logical, intent(in), optional :: is_abbrev
    integer, intent(out), optional :: status

    call self%is_present (str(identifier), is_present, is_abbrev, status)
end subroutine

! ------------------------------------------------------------------------------
! RESET_ARGS method
subroutine argparser_reset_args (self)
    class (argparser), intent(in out) :: self

    ! deallocated automatically on subroutine exit
    class (iterator), allocatable :: iter
    class (*), pointer :: ptr_item
    class (argument), pointer :: ptr_arg

    nullify (ptr_arg, ptr_item)

    ! get list iterator
    call self%args%get_iter (iter)

    do while (iter%has_next())
        ptr_item => iter%item()
        call dynamic_cast (ptr_item, ptr_arg)

        call ptr_arg%reset ()
    end do

end subroutine

! ------------------------------------------------------------------------------
! PARSE method

subroutine argparser_parse_array (self, cmd_args, status)
    class (argparser), intent(in out) :: self
    _POLYMORPHIC_ARRAY (str), intent(in), dimension(:) :: cmd_args
    integer, intent(out), optional :: status

    integer :: i, cmd_nargs
    integer :: lstatus
    character (100) :: msg
    type (str) :: cmd_arg, str_tmp

    lstatus = STATUS_INVALID_STATE
    msg = ""

    if (len(self) == 0) then
        msg = "Need to add arguments before parsing"
        goto 100
    end if

    ! undo changes to arguments applied by any previous calls to set() if
    ! parse() has been called repeatedly
    call self%reset_args ()

    cmd_nargs = size(cmd_args)

    ! argparser state seems to be valid for parsing
    lstatus = STATUS_OK

    i = 1
    do while (i <= cmd_nargs)
        cmd_arg = cmd_args(i)

        ! find associated argument object, either using the long name or the
        ! abbreviation
        if (cmd_arg%startswith ('--')) then
            ! pass argument name and (possible) =value to "long" parser
            str_tmp = cmd_arg%substring (3, -1)
            call self%parse_long (i, str_tmp, cmd_nargs, lstatus, msg)
            if (lstatus /= STATUS_OK) goto 100

        else if (cmd_arg%startswith ('-')) then

            str_tmp = cmd_arg%substring (2, -1)
            call self%parse_abbrev (i, str_tmp, cmd_nargs, lstatus, msg)
            if (lstatus /= STATUS_OK) goto 100

        end if

        ! NB: i is incremented in parse_long / parse_abbrev routines
        ! as necessary.
    end do

    ! set status to indicate that command line parsing was successful
    self%status = ARGPARSE_STATUS_PARSED

100 continue
    if (present(status)) status = lstatus
    if (lstatus /= STATUS_OK) self%status = ARGPARSE_STATUS_PARSE_ERROR
    if (len_trim(msg) > 0) write (ERROR_UNIT, fmt=*) trim(msg)

end subroutine


subroutine argparser_parse_cmd (self, status)
    class (argparser), intent(in out) :: self
    integer, intent(out), optional :: status

    character (CMD_BUFFER_SIZE) :: buf
    type (str), dimension(:), allocatable :: cmd_args
    integer :: cmd_nargs, i

    ! count does not include the command name
    cmd_nargs = command_argument_count ()

    allocate (cmd_args (cmd_nargs))
    do i = 1, cmd_nargs
        buf = ""
        call get_command_argument (i, buf)
        cmd_args(i) = trim(buf)
    end do

    call self%parse (cmd_args, status)
end subroutine

subroutine argparser_parse_long (self, offset, cmd_arg, cmd_nargs, status, msg)
    class (argparser), intent(in out) :: self
    integer, intent(in out) :: offset
    type (str), intent(in) :: cmd_arg
    integer, intent(in) :: cmd_nargs
    integer, intent(out) :: status
    character (*), intent(out) :: msg

    type (str) :: cmd_name, str_tmp
    type (str), dimension(:), allocatable :: cmd_values, work
    class (argument), pointer :: ptr_arg

    integer :: j

    status = STATUS_OK

    ! check whether there is an = and separate token in that case
    j = index (cmd_arg, "=")
    if (j > 0) then
        cmd_name = cmd_arg%substring (j-1)
        str_tmp = cmd_arg%substring (j+1, -1)
        allocate (cmd_values(1), source=str_tmp)
    else
        cmd_name = cmd_arg
    end if

    ! find corresponding argument object
    ptr_arg => self%find_arg (cmd_name, is_abbrev=.false.)

    ! Check that the argument name given on command line corresponds to
    ! defined argument
    if (.not. associated (ptr_arg)) then
        status = STATUS_INVALID_INPUT
        msg = "Unknown argument name: " // cmd_name
        goto 100
    end if

    ! collect required number of argument values
    if (allocated (cmd_values) .and. ptr_arg%nargs /= 1) then
        status = ARGPARSE_STATUS_PARSE_ERROR
        msg = "Argument '" // ptr_arg%name // &
            "': received one argument value, expected " // str(ptr_arg%nargs)
        goto 100
    end if

    ! store command line arguments in argument object
    if (allocated (cmd_values)) then
        ! at this point we know that nargs = 1 for this argument
        if (ptr_arg%action == ARGPARSE_ACTION_APPEND) then
            ! if in append mode, attempt to split potential list of arguments
            ! into separate values
            call move_alloc (cmd_values, work)
            ! split work into tokens, store in cmd_values
            call work(1)%split (cmd_values, sep=LIST_SEP, drop_empty=.true.)
            deallocate (work)
        end if

        call ptr_arg%set (cmd_values)
    else
        ! collect the number of requested arguments from the following commands
        call self%collect_values (offset+1, cmd_nargs, ptr_arg, cmd_values, &
            status, msg)
        if (status == ARGPARSE_STATUS_INSUFFICIENT_ARGS) goto 100

        ! store command line arguments in argument object
        call ptr_arg%set (cmd_values)

        ! skip the next nargs arguments, those were used as values
        offset = offset + ptr_arg%nargs
    end if

    if (allocated(cmd_values)) deallocate (cmd_values)

    ! move to next command line argument
    offset = offset + 1

100 continue
end subroutine

subroutine argparser_parse_abbrev (self, offset, cmd_arg, cmd_nargs, status, msg)
    class (argparser), intent(in out) :: self
    integer, intent(in out) :: offset
    type (str), intent(in) :: cmd_arg
    integer, intent(in) :: cmd_nargs
    integer, intent(out) :: status
    character (*), intent(out) :: msg

    type (str) :: cmd_name
    type (str), dimension(:), allocatable :: cmd_values, work
    class (argument), pointer :: ptr_arg

    integer :: j

    status = STATUS_OK

    ! loop through all characters; note that argument values can
    ! only be specified for the very last argument, ie
    ! -xyz foo bar will assign arguments foo, bar as values to z
    do j = 1, len(cmd_arg)
        cmd_name = cmd_arg%substring (j, j)
        ptr_arg => self%find_arg (cmd_name, is_abbrev=.true.)

        ! Check that the argument name given on command line corresponds to
        ! defined argument
        if (.not. associated (ptr_arg)) then
            status = STATUS_INVALID_INPUT
            msg = "Unknown argument name: " // cmd_name
            goto 100
        end if

        if (ptr_arg%nargs > 0 .and. j < len(cmd_arg)) then
            ! cannot satisfy any positive number of values for abbrev.
            ! arguments that are not the last character
            status = ARGPARSE_STATUS_INSUFFICIENT_ARGS
            msg = "Argument '" // ptr_arg%name  // "': expected " // &
                str(ptr_arg%nargs) // " arguments, found 0"
            goto 100
        else if (ptr_arg%nargs == 0) then
            call ptr_arg%set ()
        else if (ptr_arg%nargs > 0) then
            ! need to collect argument values
            call self%collect_values (offset+1, cmd_nargs, ptr_arg, &
                cmd_values, status, msg)
            if (status == ARGPARSE_STATUS_INSUFFICIENT_ARGS) goto 100

            ! Tokenize individual values if ACTION_APPEND
            if (ptr_arg%action == ARGPARSE_ACTION_APPEND) then
                ! if in append mode, attempt to split potential list of arguments
                ! into separate values
                call move_alloc (cmd_values, work)
                ! split work into tokens, store in cmd_values
                call work(1)%split (cmd_values, sep=LIST_SEP, drop_empty=.true.)
                deallocate (work)
            end if

            ! store command line arguments in argument object
            call ptr_arg%set (cmd_values)

            ! skip the next nargs arguments, those were used as values
            offset = offset + ptr_arg%nargs

            if (allocated(cmd_values)) deallocate (cmd_values)
        end if
    end do

    ! move to next command line argument
    offset = offset + 1

100 continue
end subroutine

subroutine argparser_collect_values (self, offset, cmd_nargs, ptr_arg, &
        cmd_values, status, msg)

    class (argparser), intent(in out) :: self
    integer, intent(in) :: offset, cmd_nargs
    class (argument), intent(in), pointer :: ptr_arg
    type (str), intent(out), dimension(:), allocatable :: cmd_values
    integer, intent(out) :: status
    character (*), intent(out) :: msg

    character (CMD_BUFFER_SIZE) :: buf
    integer :: j

    status = STATUS_OK
    if (allocated(cmd_values)) deallocate (cmd_values)

    allocate (cmd_values(ptr_arg%nargs))

    j = 0
    do while (j < ptr_arg%nargs)
        ! check that the number of command line arguments is not too low
        if (offset + j > cmd_nargs) then
            status = ARGPARSE_STATUS_INSUFFICIENT_ARGS
            msg = "Argument '" // ptr_arg%name  // "': expected " // &
                str(ptr_arg%nargs) // " arguments, found " // str(j-1)
            return
        end if

        call get_command_argument (offset + j, buf)
        cmd_values(j+1) = trim(buf)
        j = j + 1
    end do

end subroutine

! ------------------------------------------------------------------------------
! Helper routines

subroutine print_error (msg, unit)
    character (*), intent(in) :: msg
    integer, intent(in), optional :: unit

    integer :: lunit
    lunit = ERROR_UNIT
    if (present(unit)) lunit = unit

    if (len_trim(msg) > 0) write (lunit, fmt=*) trim(msg)
end subroutine

end module
