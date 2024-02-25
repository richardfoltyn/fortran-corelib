
#include <fcore.h>

module fcore_argparse_parser

    use iso_fortran_env
    use fcore_common
    use fcore_collections

    use fcore_argparse_argument
    use fcore_argparse_argument_data
    use fcore_argparse_actions

    implicit none
    private

    ! Status codes for internal use
    ! in intial state, no arguments added
    integer (FC_ENUM_KIND), parameter :: ARGPARSE_STATUS_INIT = 0
    integer (FC_ENUM_KIND), parameter :: ARGPARSE_STATUS_EMPTY_CMDLINE = ishft(1, 1)
    integer (FC_ENUM_KIND), parameter :: ARGPARSE_STATUS_PARSE_ERROR = ishft(1, 2)
    integer (FC_ENUM_KIND), parameter :: ARGPARSE_STATUS_PARSED = ishft(1, 3)
    integer (FC_ENUM_KIND), parameter :: ARGPARSE_STATUS_HELP_PRESENT = ishft(1, 4)

    ! only status code that needs to be communicated to client code
    public :: ARGPARSE_STATUS_PARSE_ERROR, ARGPARSE_STATUS_HELP_PRESENT

    ! buffer size used to read in command line arguments
    integer, parameter :: CMD_BUFFER_SIZE = 1024
    ! buffer for printing error messages to stderr
    integer, parameter :: MSG_LENGTH = 100
    ! seperator used to split a list of arguments into individual tokens
    character (1), parameter :: LIST_SEP = ','

    character (*), parameter :: UNMAPPED_ARG_NAME = '::UNMAPPED'

    character (*), parameter :: HELP_ARGNAME = "help"
    character (*), parameter :: HELP_ABBREV = "h"
    integer, parameter :: LINEWIDTH = 80
        !*  Linewidth when printing diagnostic messages, e.g. help text.


    type, public :: argparser
        private

        type (linked_list), allocatable :: args
        type (str) :: description
        type (str) :: progname
        integer :: status = ARGPARSE_STATUS_INIT
    contains
        procedure, pass :: argparser_add_argument_str
        procedure, pass :: argparser_add_argument_scalar_default_str
        procedure, pass :: argparser_add_argument_char
        procedure, pass :: argparser_add_argument_scalar_default_char
        procedure, pass :: argparser_add_argument_array_default_str
        procedure, pass :: argparser_add_argument_array_default_char
        generic, public :: add_argument => argparser_add_argument_str, &
            argparser_add_argument_scalar_default_str, &
            argparser_add_argument_char, &
            argparser_add_argument_scalar_default_char, &
            argparser_add_argument_array_default_str, &
            argparser_add_argument_array_default_char

        procedure, pass :: init_str => argparser_init_str
        procedure, pass :: init_char => argparser_init_char
        generic, public :: init => init_str, init_char

        procedure, public, pass :: reset => argparser_reset
        procedure, pass :: reset_args => argparser_reset_args

        procedure, pass :: argparser_get_scalar_str
        procedure, pass :: argparser_get_scalar_char
        procedure, pass :: argparser_get_array_char
        procedure, pass :: argparser_get_array_str
        generic, public :: get => argparser_get_scalar_str, &
            argparser_get_scalar_char, &
            argparser_get_array_str, &
            argparser_get_array_char

        procedure, pass :: argparser_get_unmapped_scalar
        procedure, pass :: argparser_get_unmapped_array
        generic, public :: get_unmapped => argparser_get_unmapped_scalar

        generic, public :: get_unmapped => argparser_get_unmapped_array

        procedure, pass :: argparser_parse_array
        procedure, pass :: argparser_parse_cmd
        generic, public :: parse => argparser_parse_array, argparser_parse_cmd

        procedure, pass :: create_arg => argparser_create_arg
        procedure, pass :: find_arg => argparser_find_arg
        procedure, pass :: parse_long => argparser_parse_long
        procedure, pass :: parse_abbrev => argparser_parse_abbrev
        procedure, pass :: parse_unmapped => argparser_parse_unmapped
        procedure, pass :: collect_values => argparser_collect_values
        procedure, pass :: get_nargs => argparser_get_nargs

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

        procedure, pass :: argparser_get_nvals_str
        procedure, pass :: argparser_get_nvals_char
        generic, public :: get_nvals => argparser_get_nvals_str, &
            argparser_get_nvals_char
        procedure, pass, public :: get_num_unmapped => argparser_get_num_unmapped

        procedure, pass :: help_present => argparser_help_present

        procedure, public, pass :: print_help => argparser_print_help

        procedure, pass :: argparser_assign
        generic, public :: assignment(=) => argparser_assign
    end type

    interface sanitize_argument_text
        procedure sanitize_argument_text_str, sanitize_argument_text_char
    end interface


contains

! ------------------------------------------------------------------------------
! Initialization

subroutine argparser_init_str (self, description, progname)
    class (argparser), intent(inout) :: self
    class (str), intent(in), optional :: description
    class (str), intent(in), optional :: progname

    type (str) :: help_text
    type (status_t) :: status

    class (argument), pointer :: ptr_arg
    integer :: cmd_len, cmd_status
    character (:), allocatable :: cmd_value
    character (0) :: dummy

    type (str), dimension(1) :: names, abbrevs

    call self%reset ()

    self%progname = ""
    if (present(progname)) then
        self%progname = progname
    else
        ! Get program name as the first token on command line
        call get_command_argument (0, dummy, cmd_len)
        if (cmd_len > 0) then
            allocate (character (cmd_len) :: cmd_value)
            call get_command_argument (0, cmd_value, cmd_len, cmd_status)
            if (cmd_len > 0 .and. cmd_status == 0) then
                self%progname = cmd_value(1:cmd_len)
            end if
            deallocate (cmd_value)
        end if
    end if
    if (present(description)) self%description = description

    ! Add "fake" argument object that will be used to store all unmapped
    ! command line arguments.
    ! Create argument "in place", ie. copy it first into the collection,
    ! set its attributes later. Avoids (potentially flawed) copying
    ! of attributes when adding to collection.
    ptr_arg => self%create_arg ()

    names(1) = str(UNMAPPED_ARG_NAME)
    call ptr_arg%init (names, action=ARGPARSE_ACTION_APPEND, status=status)

    ! Add "built-in" argument --help/-h that displays help text
    ptr_arg => self%create_arg ()
    help_text = "Display this help message"
    names(1) = HELP_ARGNAME
    abbrevs(1) = HELP_ABBREV
    call ptr_arg%init (names, abbrevs, action=ARGPARSE_ACTION_STORE_TRUE, &
        help=help_text, status=status)

end subroutine

subroutine argparser_init_char (self, description, progname)
    class (argparser), intent(inout) :: self
    character (*), intent(in) :: description
    character (*), intent(in), optional :: progname

    if (present(progname)) then
        call self%init (str(description), str(progname))
    else
        call self%init (str(description))
    end if

end subroutine

pure subroutine argparser_reset (self)
    class (argparser), intent(inout) :: self

    if (allocated(self%args)) deallocate(self%args)

    self%description = ""
    self%status = ARGPARSE_STATUS_INIT
end subroutine

! ------------------------------------------------------------------------------
! Length method, len() generic

pure function argparser_get_nargs (self) result(res)
    class (argparser), intent(in) :: self
    integer :: res
    res = 0
    if (allocated(self%args)) res = len(self%args)
end function

! ------------------------------------------------------------------------------
! Adding arguments
!
! API for calling argparser::add needs to handle
!   1.  Array or scalar argument values (default and cost)
!   2.  Optional values (default and cost)
!   3.  Meta-data (name, abbrev, etc.) passed in as character or str.
!
! The corresponding routines thus have the following (simplified) signitures
! that allow calls to argparser::add to be unambiguously mapped to routines:
!   1.  default(:); const(:), optional
!   2.  default; const, optional
!   3.  no default; no const
! This set of routines is replicated for both character and type(str) metadata.
!
! It is a design decision to not allow adding argument with 'const' specified
! but missing 'default', as it is unclear what should be returned if the
! argument is not present on the command line (Python's argparse returns None,
! but that is not an option here.)
!
! Note on character-type data in default / const dummy arguments:
! These need to be copied over into str (array) components, as otherwise
! it is impossible to retrieve them into character types in a robust way.
! Additionally, attempting to copy character data into character arrays
! crashes when code was compiled with gfortran.


subroutine argparser_add_argument_array_default_str (self, name, abbrev, &
        action, nargs, required, help, status, validator, default, const)
    !*  Add argument definition using the following interface:
    !       1. str meta-data
    !       2. array values
    !       3. mandatory 'default', optional 'const'

    class (argparser), intent(inout) :: self
    class (str), intent(in) :: name
    class (str), intent(in), optional :: abbrev
    integer (FC_ENUM_KIND), intent(in), optional :: action
    integer, intent(in), optional :: nargs
    logical, intent(in), optional :: required
    class (str), intent(in), optional :: help
    type (status_t), intent(out), optional :: status
    procedure (fcn_validator), optional :: validator
    class (*), intent(in), dimension(:) :: default
    class (*), intent(in), dimension(:), optional :: const

    type (status_t) :: lstatus
    type (argument_data) :: default_data, const_data

    class (argument), pointer :: ptr_arg
    type (str), dimension(1) :: names
    type (str), allocatable, dimension(:) :: abbrevs

    nullify (ptr_arg)

    call self%check_input (name, abbrev, action, nargs, lstatus)
    if (lstatus /= FC_STATUS_OK) goto 100

    call sanitize_argument_text (name, abbrev, help, names, abbrevs)
    call argument_data_alloc (default_data, default)
    if (present(const)) call argument_data_alloc (const_data, const)

    ptr_arg => self%create_arg ()

    call ptr_arg%init (names, abbrevs, action, required, nargs, help, lstatus, &
        default_data, const_data, validator=validator)

    call process_argument_status (name, lstatus)

100 continue
    if (present(status)) status = lstatus
    ! Clean up argument object in case initialization failed
    if (associated(ptr_arg) .and. lstatus /= FC_STATUS_OK) then
        call self%args%remove (size(self%args))
        nullify (ptr_arg)
    end if
end subroutine


subroutine argparser_add_argument_str (self, name, abbrev, &
        action, nargs, required, help, status, validator)
    !*  Add argument definition using the following interface:
    !       1. str meta-data
    !       2. array values
    !       3. no 'default', no 'const'

    class (argparser), intent(inout) :: self
    class (str), intent(in) :: name
    class (str), intent(in), optional :: abbrev
    integer (FC_ENUM_KIND), intent(in), optional :: action
    integer, intent(in), optional :: nargs
    logical, intent(in), optional :: required
    class (str), intent(in), optional :: help
    type (status_t), intent(out), optional :: status
    procedure (fcn_validator), optional :: validator

    class (argument), pointer :: ptr_arg
    type (status_t) :: lstatus

    type (str), dimension(1) :: names
    type (str), allocatable, dimension(:) :: abbrevs

    nullify (ptr_arg)

    call self%check_input (name, abbrev, action, nargs, lstatus)
    if (lstatus /= FC_STATUS_OK) goto 100

    call sanitize_argument_text (name, abbrev, help, names, abbrevs)

    ptr_arg => self%create_arg ()

    call ptr_arg%init (names, abbrevs, action, required, nargs, help, lstatus, &
        validator=validator)

    call process_argument_status (name, lstatus)

100 continue
    if (present(status)) status = lstatus
    ! Clean up argument object in case initialization failed
    if (associated(ptr_arg) .and. lstatus /= FC_STATUS_OK) then
        call self%args%remove (size(self%args))
        nullify (ptr_arg)
    end if
end subroutine

subroutine argparser_add_argument_scalar_default_str (self, name, abbrev, action, &
        nargs, required, help, status, validator, default, const)
    !*  Add argument definition using the following interface:
    !       1. str meta-data
    !       2. scalar values
    !       3. mandatory 'default', optional 'const'

    class (argparser), intent(inout) :: self
    class (str), intent(in) :: name
    class (str), intent(in), optional :: abbrev
    integer (FC_ENUM_KIND), intent(in), optional :: action
    integer, intent(in), optional :: nargs
    logical, intent(in), optional :: required
    class (str), intent(in), optional :: help
    type (status_t), intent(out), optional :: status
    procedure (fcn_validator), optional :: validator
    class (*), intent(in) :: default
    class (*), intent(in), optional :: const

    class (argument), pointer :: ptr_arg
    type (status_t) :: lstatus
    type (argument_data) :: default_data, const_data

    type (str), dimension(1) :: names
    type (str), allocatable, dimension(:) :: abbrevs

    nullify (ptr_arg)

    call self%check_input (name, abbrev, action, nargs, lstatus)
    if (lstatus /= FC_STATUS_OK) goto 100

    call sanitize_argument_text (name, abbrev, help, names, abbrevs)
    call argument_data_alloc (default_data, default)
    if (present(const)) call argument_data_alloc (const_data, const)

    ptr_arg => self%create_arg ()

    call ptr_arg%init (names, abbrevs, action, required, nargs, help, lstatus, &
        default_data, const_data, validator=validator)

    call process_argument_status (name, lstatus)

100 continue
    if (present(status)) status = lstatus
    ! Clean up argument object in case initialization failed
    if (associated(ptr_arg) .and. lstatus /= FC_STATUS_OK) then
        call self%args%remove (size(self%args))
        nullify (ptr_arg)
    end if

end subroutine

subroutine argparser_add_argument_scalar_default_char (self, name, abbrev, action, &
        nargs, required, help, status, validator, default, const)
    !*  Add argument definition using the following interface:
    !       1. character meta-data
    !       2. scalar values
    !       3. mandatory 'default', optional 'const'

    class (argparser), intent(inout) :: self
    character (*), intent(in) :: name
    character (*), intent(in), optional :: abbrev
    integer (FC_ENUM_KIND), intent(in), optional :: action
    integer, intent(in), optional :: nargs
    logical, intent(in), optional :: required
    character (*), intent(in), optional :: help
    type (status_t), intent(out), optional :: status
    procedure (fcn_validator), optional :: validator
    class (*), intent(in) :: default
    class (*), intent(in), optional :: const

    type (str) :: lhelp
    class (argument), pointer :: ptr_arg
    type (status_t) :: lstatus
    type (argument_data) :: default_data, const_data

    type (str), dimension(1) :: names
    type (str), allocatable, dimension(:) :: abbrevs

    nullify (ptr_arg)

    call self%check_input (name, abbrev, action, nargs, lstatus)
    if (lstatus /= FC_STATUS_OK) goto 100

    call sanitize_argument_text (name, abbrev, help, names, abbrevs, lhelp)
    call argument_data_alloc (default_data, default)
    if (present(const)) call argument_data_alloc (const_data, const)

    ptr_arg => self%create_arg ()

    call ptr_arg%init (names, abbrevs, action, required, nargs, lhelp, lstatus, &
        default_data, const_data, validator=validator)

    call process_argument_status (names(1), lstatus)

100 continue
    if (present(status)) status = lstatus
    ! Clean up argument object in case initialization failed
    if (associated(ptr_arg) .and. lstatus /= FC_STATUS_OK) then
        call self%args%remove (size(self%args))
        nullify (ptr_arg)
    end if
end subroutine


subroutine argparser_add_argument_array_default_char (self, name, abbrev, action, &
        nargs, required, help, status, validator, default, const)
    !*  Add argument definition using the following interface:
    !       1. character meta-data
    !       2. array values
    !       3. mandatory 'default', optional 'const'

    class (argparser), intent(inout) :: self
    character (*), intent(in) :: name
    character (*), intent(in), optional :: abbrev
    integer (FC_ENUM_KIND), intent(in), optional :: action
    integer, intent(in), optional :: nargs
    logical, intent(in), optional :: required
    character (*), intent(in), optional :: help
    type (status_t), intent(out), optional :: status
    procedure (fcn_validator), optional :: validator
    class (*), intent(in), dimension(:) :: default
    class (*), intent(in), dimension(:), optional :: const

    type (str) :: lhelp
    class (argument), pointer :: ptr_arg
    type (status_t) :: lstatus
    type (argument_data) :: default_data, const_data

    type (str), dimension(1) :: names
    type (str), allocatable, dimension(:) :: abbrevs

    nullify (ptr_arg)

    call self%check_input (name, abbrev, action, nargs, lstatus)
    if (lstatus /= FC_STATUS_OK) goto 100

    call sanitize_argument_text (name, abbrev, help, names, abbrevs, lhelp)

    select type (default)
    type is (character (*))
        call argument_data_alloc_char (default_data, default)
    class default
        call argument_data_alloc (default_data, default)
    end select

    if (present(const)) then
        select type (const)
        type is (character (*))
            call argument_data_alloc_char (const_data, const)
        class default
            call argument_data_alloc (const_data, const)
        end select
    end if

    ptr_arg => self%create_arg ()

    call ptr_arg%init (names, abbrevs, action, required, nargs, lhelp, lstatus, &
        default_data, const_data, validator=validator)

    call process_argument_status (names(1), lstatus)

100 continue
    if (present(status)) status = lstatus
   ! Clean up argument object in case initialization failed
    if (associated(ptr_arg) .and. lstatus /= FC_STATUS_OK) then
        call self%args%remove (size(self%args))
        nullify (ptr_arg)
    end if
end subroutine



subroutine argparser_add_argument_char (self, name, abbrev, action, &
        nargs, required, help, status, validator)
    !*  Add argument definition using the following interface:
    !       1. character meta-data
    !       2. array values
    !       3. no 'default', no 'const'

    class (argparser), intent(inout) :: self
    character (*), intent(in) :: name
    character (*), intent(in), optional :: abbrev
    integer (FC_ENUM_KIND), intent(in), optional :: action
    integer, intent(in), optional :: nargs
    logical, intent(in), optional :: required
    character (*), intent(in), optional :: help
    type (status_t), intent(out), optional :: status
    procedure (fcn_validator), optional :: validator

    type (str) :: lhelp
    class (argument), pointer :: ptr_arg
    type (status_t) :: lstatus

    type (str), dimension(1) :: names
    type (str), allocatable, dimension(:) :: abbrevs

    nullify (ptr_arg)

    call self%check_input (name, abbrev, action, nargs, lstatus)
    if (lstatus /= FC_STATUS_OK) goto 100

    call sanitize_argument_text (name, abbrev, help, names, abbrevs, lhelp)

    ptr_arg => self%create_arg ()

    call ptr_arg%init (names, abbrevs, action, required, nargs, lhelp, lstatus, &
        validator=validator)

    call process_argument_status (names(1), lstatus)

100 continue
    if (present(status)) status = lstatus
    ! Clean up argument object in case initialization failed
    if (associated(ptr_arg) .and. lstatus /= FC_STATUS_OK) then
        call self%args%remove (size(self%args))
        nullify (ptr_arg)
    end if
end subroutine


subroutine sanitize_argument_text_str (name, abbrev, help, names, abbrevs)
    !*  SANITIZE_ARGUMENT_TEXT_STR converts user-provided text arguments
    !   such as name, abbrev into data that can be passed to ARGUMENT's
    !   INIT method.
    class (str), intent(in) :: name
    class (str), intent(in), optional :: abbrev
    class (str), intent(in) :: help
        !*  Help string. Unchanged for the 'str' routine, provided for 
        !   API compatibility with 'character' routine.
    type (str), intent(inout), dimension(:) :: names
    type (str), intent(inout), dimension(:), allocatable :: abbrevs

    names(1) = name
    if (allocated(abbrevs)) deallocate(abbrevs)
    if (present(abbrev)) then
        allocate (abbrevs(1))
        abbrevs(1) = abbrev
    else
        allocate (abbrevs(0))
    end if

end subroutine


subroutine sanitize_argument_text_char (name, abbrev, help, names, abbrevs, help_str)
    !*  SANITIZE_ARGUMENT_TEXT_CHAR converts user-provided text arguments
    !   such as name, abbrev into data that can be passed to ARGUMENT's
    !   INIT method.
    character (*), intent(in) :: name
    character (*), intent(in), optional :: abbrev
    type (str), intent(inout), dimension(:) :: names
    type (str), intent(inout), dimension(:), allocatable :: abbrevs
    character (*), intent(in), optional :: help
    type (str), intent(out) :: help_str

    names(1) = trim(name)
    if (allocated(abbrevs)) deallocate(abbrevs)
    if (present(abbrev)) then
        allocate (abbrevs(1))
        abbrevs(1) = trim(abbrev)
    else
        allocate (abbrevs(0))
    end if

    help_str = ""
    if (present(help)) help_str = trim(help)

end subroutine


!-------------------------------------------------------------------------------
! Input validation

subroutine argparser_check_input_str (self, name, abbrev, action, nargs, status)
    !*  ARGPARSER_CHECK_INPUT_STR performs input validation on all user-specified
    !   argument meta-data.

    class (argparser), intent(in) :: self
    class (str), intent(in) :: name
    class (str), intent(in), optional :: abbrev
    integer (FC_ENUM_KIND), intent(in), optional :: action
    integer, intent(in), optional :: nargs
    type (status_t), intent(out) :: status
        !*  Exit status. Status code is FC_STATUS_OK on exit if inputs are valid,
        !   and one of the error codes otherwise.

    ! by default return invalid input status
    call status%init (FC_STATUS_OK)

    if (name%lower() == HELP_ARGNAME) then
        status%msg = "Argument --" // HELP_ARGNAME // " is reserved for internal use"
        goto 100
    end if

    if (present(abbrev)) then
        if (abbrev%lower() == HELP_ABBREV) then
            status%msg = "Argument -" // HELP_ABBREV // " is reserved for internal use"
            goto 100
        end if
    end if

    call validate_identifier (name, status)
    if (status /= FC_STATUS_OK) then
        status%msg = "Invalid name: " // status%msg
        goto 100
    end if

    ! check whether argument with this name is already defined
    if (self%is_defined (name)) then
        status%msg = "Invalid name '" // name // &
            "': argument name already defined"
        goto 100
    end if

    if (present(abbrev)) then
        if (len(abbrev) /= 1) then
            status%msg = "Invalid abbrev '" // abbrev // &
                "': value must be character of length 1"
            goto 100
        end if

        call validate_identifier (abbrev, status)
        if (status /= FC_STATUS_OK) then
            status%msg = "Invalid abbrev '" // abbrev // "': " // status%msg
            goto 100
        end if

        ! check whether abbreviation of this name already exists
        if (self%is_defined (abbrev, is_abbrev=.true.)) then
            status%msg = "Invalid abbrev '" // abbrev // &
                "' : argument abbrev already defined"
            goto 100
        end if
    end if

    call validate_action (action, status)
    if (status /= FC_STATUS_OK) then
        status%msg = "Invalid action for argument '" // name // "'"
        goto 100
    end if

    if (present(nargs)) then
        if (nargs < 0) then
            status%msg = "Invalid nargs for argument '" // name // "'"
            goto 100
        end if
    end if

    ! No errors encountered; set OK status and exit.
    call status%init (FC_STATUS_OK)
    return
    
100 continue
    status = FC_STATUS_VALUE_ERROR
end subroutine

subroutine argparser_check_input_char (self, name, abbrev, action, nargs, status)
    class (argparser), intent(in) :: self
    character (*), intent(in) :: name
    character (*), intent(in), optional :: abbrev
    integer (FC_ENUM_KIND), intent(in), optional :: action
    integer, intent(in), optional :: nargs
    type (status_t), intent(out) :: status

    type (str) :: labbrev, lname
    lname = trim(name)
    if (present (abbrev)) then
        labbrev = trim(abbrev)
        call self%check_input (lname, labbrev, action, nargs, status)
    else
        call self%check_input (lname, action=action, nargs=nargs, &
            status=status)
    end if
end subroutine

pure subroutine validate_identifier (s, status)
    !*  VALIDATE_IDENTIFIER checks whether a given name/abbrev passed is a valid
    !   argument identifier.
    !   Valid identifiers consist only of alpha-numeric characters as well as
    !   '-' and '_', but are not permitted to begin with '-'.
    !   Empty values are not permitted.
    type (str), intent(in) :: s
    type (status_t), intent(out) :: status

    character (:), allocatable :: work
    character (1), parameter :: valid_special(2) = ['-', '_']

    integer :: i, n
    logical :: is_valid

    call status%init (FC_STATUS_OK)

    n = len(s)
    if (n == 0) then
        status = FC_STATUS_VALUE_ERROR
        status%msg = "Empty value not allowed"
        return
    end if
    if (s%startswith('-')) then
        status = FC_STATUS_VALUE_ERROR
        status%msg = "String value must not start with '-'"
    end if

    allocate (character (n) :: work)
    work = s

    do i = 1, n
        is_valid = any(work(i:i) == valid_special) .or. is_alnum (work(i:i))
        if (.not. is_valid) then
            status = FC_STATUS_VALUE_ERROR
            status%msg = "Invalid character encountered: '" // work(i:i) // "'"
        end if
    end do

end subroutine


subroutine validate_action (action, status)
    !*  VALIDATE_ACTION verifies that a user-provided integer value corresponds
    !   to a valid ARGPARSER action.
    integer (FC_ENUM_KIND), intent(in), optional :: action
    type (status_t), intent(out) :: status

    call status%init (FC_STATUS_OK)
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
    case (ARGPARSE_ACTION_TOGGLE)
        return
    case (ARGPARSE_ACTION_APPEND)
        return
    case default
        status = FC_STATUS_VALUE_ERROR
    end select
end subroutine


function argparser_create_arg (self) result(res)
    !*  ARGPARSER_APPEND adds an empty argument object to the collection of
    !   arguments and returns a pointer to it.

    class (argparser), intent(inout) :: self
    class (argument), pointer :: res

    class (*), pointer :: ptr_item
    type (argument) :: arg

    if (.not. allocated(self%args)) allocate (self%args)

    ! Appending creates a copy that is added to the collection
    call self%args%append (arg)
    ! Get pointer to last item added
    ptr_item => self%args%item(size(self%args))
    call dynamic_cast (ptr_item, res)

end function

! ------------------------------------------------------------------------------
! GET methods


subroutine argparser_get_array_str (self, name, val, status)
    class (argparser), intent(inout) :: self
    type (str), intent(in) :: name
    class (*), intent(inout), dimension(:) :: val
        !*  Note: decrease as INOUT, this seems to affect how buggy gfortran
        !   version pass on array extents.
    type (status_t), intent(out), optional :: status

    class (argument), pointer :: ptr_arg
    type (status_t) :: lstatus
    integer :: n, k
    character (:), dimension(:), allocatable :: cwork

    call lstatus%init (FC_STATUS_OK)

    call validate_identifier (name, lstatus)
    if (lstatus /= FC_STATUS_OK) goto 100

    call argparser_get_check_state (self, lstatus)
    if (lstatus /= FC_STATUS_OK) goto 100

    call self%find_arg (name, ptr_arg, lstatus)
    if (lstatus /= FC_STATUS_OK) goto 100

    ! at this point ptr_arg points to the argument identified by name.

    ! We need to treat character arrays separately as gfortran 5 screws this
    ! up otherwise.
    select type (val)
    type is (character (*))
        ! At this point we can still recover the correct length attribute
        ! of the character array VAL, but wouldn't be able to do that in
        ! ARGUMENT::POLY_PARSE anymore if compiled with gfortran. Hence
        ! "convert" to non-polymorphic data type here and avoid this.
        n = size(val)
        k = len(val)
        allocate (character (k) :: cwork(n))
        call ptr_arg%parse (cwork, lstatus)
        val(1:n) = cwork(1:n)
        deallocate (cwork)
    class default
        call ptr_arg%poly_parse (val, lstatus)
    end select

    call process_argument_status (name, lstatus)

100 continue
    if (present(status)) status = lstatus
end subroutine


subroutine argparser_get_array_char (self, name, val, status)
    !*  ARGPARSER_GET_ARRAY_CHAR returns the array-valued data for argument
    !   of a given name.
    !
    !   Note: This is not a wrapper around the STR-version of this routine
    !   to avoid inserting more routines with unlimited polymorphic arrays
    !   into the call stack due to a bug in gfortran 5 and lower.
    class (argparser), intent(inout) :: self
    character (*), intent(in) :: name
    class (*), intent(inout), dimension(:) :: val
        !*  Note: decrease as INOUT, this seems to affect how buggy gfortran
        !   version pass on array extents.
    type (status_t), intent(out), optional :: status

    type (str) :: sname
    class (argument), pointer :: ptr_arg
    type (status_t) :: lstatus
    integer :: n, k
    character (:), dimension(:), allocatable :: cwork

    call lstatus%init (FC_STATUS_OK)

    sname = name

    call validate_identifier (sname, lstatus)
    if (lstatus /= FC_STATUS_OK) goto 100

    call argparser_get_check_state (self, lstatus)
    if (lstatus /= FC_STATUS_OK) goto 100

    call self%find_arg (sname, ptr_arg, lstatus)
    if (lstatus /= FC_STATUS_OK) goto 100

    ! at this point ptr_arg points to the argument identified by name.

    ! We need to treat character arrays separately as gfortran 5 screws this
    ! up otherwise.
    select type (val)
    type is (character (*))
        ! At this point we can still recover the correct length attribute
        ! of the character array VAL, but wouldn't be able to do that in
        ! ARGUMENT::POLY_PARSE anymore if compiled with gfortran. Hence
        ! "convert" to non-polymorphic data type here and avoid this.
        n = size(val)
        k = len(val)
        allocate (character (k) :: cwork(n))
        call ptr_arg%parse (cwork, lstatus)
        val = cwork
        deallocate (cwork)
    class default
        call ptr_arg%poly_parse (val, lstatus)
    end select

    call process_argument_status (sname, lstatus)

100 continue
    if (present(status)) status = lstatus
end subroutine


subroutine argparser_get_scalar_str (self, name, val, status)
    class (argparser), intent(inout) :: self
    type (str), intent(in) :: name
    class (*), intent(inout) :: val
    type (status_t), intent(out), optional :: status

    class (argument), pointer :: ptr_arg
    type (status_t) :: lstatus

    call lstatus%init (FC_STATUS_OK)

    call validate_identifier (name, lstatus)
    if (lstatus /= FC_STATUS_OK) goto 100

    call argparser_get_check_state (self, lstatus)
    if (lstatus /= FC_STATUS_OK) goto 100

    call self%find_arg (name, ptr_arg, lstatus)
    if (lstatus /= FC_STATUS_OK) goto 100

    ! at this point ptr_arg points to the argument identified by name.
    ! Retrieve stored argument value
    call ptr_arg%poly_parse (val, lstatus)

    call process_argument_status (name, lstatus)

100 continue
    if (present(status)) status = lstatus
end subroutine


subroutine argparser_get_scalar_char (self, name, val, status)
    class (argparser), intent(inout) :: self
    character (*), intent(in) :: name
    class (*), intent(inout) :: val
    type (status_t), intent(out), optional :: status

    call self%get (str(name), val, status)
end subroutine


subroutine argparser_get_unmapped_array (self, val, status)
    !*  GET_UNMAPPED_ARRAY returns an array of command line arguments
    !   that could not be mapped to any named argument.
    !
    !   Note: Do not call GET_ARRAY_STR to do the actual work since
    !   there we perform input validation, and the identifier for
    !   the argument object that stores unmapped values was chosen to be
    !   an invalid value by design.
    class (argparser), intent(in) :: self
    class (*), intent(inout), dimension(:) :: val
    type (status_t), intent(out), optional :: status

    class (argument), pointer :: ptr_arg
    type (status_t) :: lstatus
    type (str) :: name
    integer :: nvals

    call lstatus%init (FC_STATUS_OK)

    call argparser_get_check_state (self, lstatus)
    if (lstatus /= FC_STATUS_OK) goto 100

    name = UNMAPPED_ARG_NAME
    call self%find_arg (name, ptr_arg)

    ! Handle dimension check upfront, even though argument::parse also does this.
    nvals = ptr_arg%get_nvals ()
    if (nvals == 0) then
        lstatus = FC_STATUS_INVALID_STATE
        lstatus%msg = "No unmapped command line arguments present."
        goto 100
    else if (size(val) < nvals) then
        lstatus = FC_STATUS_VALUE_ERROR
        lstatus%msg = "Array size insufficient to store all unmapped arguments"
        goto 100
    end if

    call ptr_arg%poly_parse (val, lstatus)

    ! Note: do not post-process error message, as this would prepend
    ! internal argument name.
100 continue
    if (present(status)) status = lstatus
end subroutine


subroutine argparser_get_unmapped_scalar (self, val, status)
    !*  GET_UNMAPPED_SCALAR returns the command line argument that
    !   could not be mapped to any named argument.
    !   Returns an error if there is not exactly one unnamed argument.
    class (argparser), intent(in) :: self
    class (*), intent(inout) :: val
    type (status_t), intent(out), optional :: status

    class (argument), pointer :: ptr_arg
    type (status_t) :: lstatus
    type (str) :: name
    integer :: nvals

    call lstatus%init (FC_STATUS_OK)

    call argparser_get_check_state (self, lstatus)
    if (lstatus /= FC_STATUS_OK) goto 100

    name = UNMAPPED_ARG_NAME
    call self%find_arg (name, ptr_arg)

    ! Handle dimension check upfront, even though argument::parse also does this.
    nvals = ptr_arg%get_nvals ()
    if (nvals == 0) then
        lstatus = FC_STATUS_INVALID_STATE
        lstatus%msg = "No unmapped command line arguments present."
        goto 100
    else if (nvals > 1) then
        lstatus = FC_STATUS_VALUE_ERROR
        lstatus%msg = "Array size insufficient to store all unmapped arguments"
        goto 100
    end if

    call ptr_arg%poly_parse (val, lstatus)

100 continue
    if (present(status)) status = lstatus
end subroutine


subroutine argparser_get_check_state (self, status)
    class (argparser), intent(in) :: self
    type (status_t), intent(out) :: status

    call status%init (FC_STATUS_OK)

    if (self%get_nargs() == 0) then
        status = FC_STATUS_INVALID_STATE
        status%msg = "No arguments defined"
        return
    else if (self%status == ARGPARSE_STATUS_PARSE_ERROR) then
        status = FC_STATUS_INVALID_STATE
        status%msg = "Cannot retrieve argument due to previous parsing error"
        return
    else if (self%status /= ARGPARSE_STATUS_PARSED) then
        status = FC_STATUS_UNKNOWN
        status%msg = "Unknown error encountered"
        return
    end if
end subroutine

! ------------------------------------------------------------------------------
! FIND_ARG method
subroutine argparser_find_arg (self, name, ptr_arg, status, is_abbrev)
    class (argparser), intent(in) :: self
    class (str), intent(in) :: name
    class (argument), intent(out), pointer :: ptr_arg
    type (status_t), intent(out), optional :: status
    logical, intent(in), optional :: is_abbrev

    ! deallocated automatically on subroutine exit
    class (iterator), allocatable :: iter
    class (*), pointer :: ptr_item

    logical :: lis_abbrev, found

    lis_abbrev = .false.
    if (present(is_abbrev)) lis_abbrev = is_abbrev

    nullify (ptr_arg)

    if (self%get_nargs() == 0) then
        if (present(status)) then
            status = FC_STATUS_INVALID_STATE
            status%msg = "No arguments defined"
        end if
        return
    end if

    ! try to locate name in list of arguments
    ! get list iterator
    call self%args%get_iter (iter)

    do while (iter%has_next())
        ptr_item => iter%item()
        ! cannot have invalid cast as linked list can contain only argument
        ! objects
        call dynamic_cast (ptr_item, ptr_arg)

        ! If name or abbrev. matches, exit routine, leaving
        ! ptr_arg pointing to current argument object
        found = ptr_arg%matches (name, lis_abbrev)
        if (found) goto 100

        nullify (ptr_arg)
    end do

100 continue

    ! Set status on exit
    if (present(status)) then
        call status%init (FC_STATUS_OK)
        if (.not. associated (ptr_arg)) then
            status = FC_STATUS_VALUE_ERROR
            status%msg = "Unknown argment: '" // name // "'"
        end if
    end if

end subroutine



! ------------------------------------------------------------------------------
! IS_DEFINED method

function argparser_is_defined_str (self, identifier, is_abbrev) result (res)
    class (argparser), intent(in) :: self
    type (str), intent(in) :: identifier
    logical, intent(in), optional :: is_abbrev
    logical :: res

    class (argument), pointer :: ptr_arg

    nullify (ptr_arg)
    call self%find_arg (identifier, ptr_arg, is_abbrev=is_abbrev)

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
! GET_NVALS method

function argparser_get_nvals_str (self, identifier, is_abbrev) result(res)
    class (argparser), intent(in) :: self
    type (str), intent(in) :: identifier
    logical, intent(in), optional :: is_abbrev
    integer :: res

    class (argument), pointer :: ptr_arg

    ! if argument name not found, return -1
    res = -1

    nullify (ptr_arg)
    call self%find_arg (identifier, ptr_arg, is_abbrev=is_abbrev)

    if (associated(ptr_arg)) res = ptr_arg%get_nvals ()

end function

function argparser_get_nvals_char (self, identifier, is_abbrev) result(res)
    class (argparser), intent(in) :: self
    character (*), intent(in) :: identifier
    logical, intent(in), optional :: is_abbrev
    integer :: res

    res = self%get_nvals (str(identifier), is_abbrev)
end function

function argparser_get_num_unmapped (self) result(res)
    class (argparser), intent(in) :: self
    integer :: res

    class (argument), pointer :: ptr_arg
    type (str) :: id

    id = UNMAPPED_ARG_NAME

    ! this should always return a valid pointer
    call self%find_arg (id, ptr_arg)
    res = ptr_arg%get_nvals ()
end function

! ------------------------------------------------------------------------------
! IS_PRESENT method

function argparser_is_present_str (self, identifier, is_abbrev) result(res)
    class (argparser), intent(in) :: self
    type (str), intent(in) :: identifier
    logical, intent(in), optional :: is_abbrev
    logical :: res

    class (argument), pointer :: ptr_arg

    res = .false.

    nullify (ptr_arg)
    call self%find_arg (identifier, ptr_arg, is_abbrev=is_abbrev)

    if (associated(ptr_arg)) res = ptr_arg%is_present
end function


function argparser_is_present_char (self, identifier, is_abbrev) result(res)
    class (argparser), intent(in) :: self
    character (*), intent(in) :: identifier
    logical, intent(in), optional :: is_abbrev
    logical :: res

    res = self%is_present (str(identifier), is_abbrev)
end function

! ------------------------------------------------------------------------------
! RESET_ARGS method
subroutine argparser_reset_args (self)
    class (argparser), intent(inout) :: self

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
    class (argparser), intent(inout) :: self
    class (str), intent(in), dimension(:) :: cmd_args
    type (status_t), intent(out), optional :: status

    integer :: i, cmd_nargs
    type (status_t) :: lstatus
    type (str) :: cmd_arg
    logical :: help_present

    call lstatus%init (FC_STATUS_OK)
    self%status = ARGPARSE_STATUS_PARSE_ERROR

    ! Check whether --help/-h was passed before anything else, as then we
    ! just display the help message and exit.
    help_present = self%help_present (cmd_args)

    ! Do not bother parsing any other arguments if --help was passed.
    if (help_present) then
        lstatus = ARGPARSE_STATUS_HELP_PRESENT
        self%status = ARGPARSE_STATUS_HELP_PRESENT
        call self%print_help ()
        goto 100
    end if

    if (self%get_nargs() == 0) then
        lstatus = FC_STATUS_INVALID_STATE
        lstatus%msg = "Need to define arguments before parsing"
        goto 100
    end if

    ! undo changes to arguments applied by any previous calls to set() if
    ! parse() has been called repeatedly
    call self%reset_args ()

    cmd_nargs = size(cmd_args)

    i = 1
    do while (i <= cmd_nargs)
        cmd_arg = cmd_args(i)

        ! find associated argument object, either using the long name or the
        ! abbreviation
        if (cmd_arg%startswith ('--')) then
            call self%parse_long (cmd_args, i, lstatus)
            if (lstatus /= FC_STATUS_OK) goto 100
        else if (cmd_arg%startswith ('-')) then
            call self%parse_abbrev (cmd_args, i, lstatus)
            if (lstatus /= FC_STATUS_OK) goto 100
        else
            call self%parse_unmapped (cmd_arg, lstatus)
            if (lstatus /= FC_STATUS_OK) goto 100
            ! Process unmapped arguments only one at a time.
            i = i + 1
        end if

        ! NB: i is incremented in parse_long / parse_abbrev routines
        ! as necessary.
    end do

    ! set status to indicate that command line parsing was successful
    self%status = ARGPARSE_STATUS_PARSED

100 continue
    if (present(status)) status = lstatus

end subroutine


subroutine argparser_parse_cmd (self, status)
    !*  ARGPARSE_PARSE_CMD parses arguments provided at the command line
    class (argparser), intent(inout) :: self
    type (status_t), intent(out), optional :: status

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

    call self%parse (cmd_args, status=status)
end subroutine

subroutine argparser_parse_long (self, cmd_args, offset, status)
    class (argparser), intent(inout) :: self
    class (str), intent(in), dimension(:) :: cmd_args
    integer, intent(inout) :: offset
    type (status_t), intent(out) :: status

    type (str) :: cmd_name, str_value, cmd_arg
    type (str), dimension(:), allocatable :: cmd_values
    class (argument), pointer :: ptr_arg

    integer :: j, cmd_nargs

    call status%init (FC_STATUS_OK)
    cmd_nargs = size(cmd_args)

    ! remove leading -- from long syntax
    cmd_arg = cmd_args(offset)%substring (3, -1)

    ! move to next command line argument; perform this even if
    ! parsing errors are encountered further below
    offset = offset + 1

    ! check whether there is an = and separate token in that case
    j = index (cmd_arg, "=")
    if (j > 0) then
        cmd_name = cmd_arg%substring (j-1)
    else
        cmd_name = cmd_arg
    end if

    ! find corresponding argument object
    call self%find_arg (cmd_name, ptr_arg, status, is_abbrev=.false.)
    if (status /= FC_STATUS_OK) goto 200

    ! if argument value was passed within the same command line argument,
    ! extract it from substring after the '='
    if (j > 0) then
        ! select substring following the '='
        str_value = cmd_arg%substring (j + 1, -1)
        ! if ACTION_APPEND is specified and the --name=value format was used,
        ! try to tokenze 'value' into components using list separator
        if (ptr_arg%action == ARGPARSE_ACTION_APPEND) then
            call str_value%split (cmd_values, LIST_SEP, drop_empty=.false.)
        else
            allocate (cmd_values(1), source=str_value)
        end if
    end if

    ! If argument value was specified as --name=value, we require
    ! that the argument supports exactly one value.
    ! At this point, this single value is stored in cmd_values(1).
    if (allocated (cmd_values) .and. ptr_arg%nargs /= 1) then
        status = ARGPARSE_STATUS_PARSE_ERROR
        status%msg = "Argument '" // cmd_name // &
            "': received one argument value, expected " // str(ptr_arg%nargs)
        goto 200
    end if

    ! store command line arguments in argument object
    if (allocated (cmd_values)) then
        call ptr_arg%set (cmd_name, cmd_values, status)
        if (status /= FC_STATUS_OK) goto 100
    else
        ! collect the number of requested arguments from the following commands
        ! This also supports collecting 0 values (e.g. if action is
        ! STORE_CONST), then cmd_values is allocated to cmd_values(1:0).
        call self%collect_values (cmd_name, cmd_args, offset, ptr_arg, cmd_values, status)
        if (status /= FC_STATUS_OK) goto 100

        ! store command line arguments in argument object
        call ptr_arg%set (cmd_name, cmd_values, status)
        if (status /= FC_STATUS_OK) goto 100

        ! skip the next nargs arguments, those were used as values
        offset = offset + ptr_arg%nargs
    end if

    return

100 continue
    ! Post-processing of status objects received from argument methods if
    ! error occurs
    call process_argument_status (cmd_name, status)
    return

200 continue
    ! Post-processing of status objects populated in current routine
    ! Note: none required at this point.
end subroutine

subroutine argparser_parse_abbrev (self, cmd_args, offset, status)
    class (argparser), intent(inout) :: self
    class (str), intent(in), dimension(:) :: cmd_args
    integer, intent(inout) :: offset
    type (status_t), intent(out) :: status

    type (str) :: cmd_name, cmd_arg
    type (str), dimension(:), allocatable :: cmd_values
    class (argument), pointer :: ptr_arg

    integer :: j, cmd_nargs

    call status%init (FC_STATUS_OK)
    cmd_nargs = size(cmd_args)

    ! remove leading - from argument (list)
    cmd_arg = cmd_args(offset)%substring (2, -1)

    ! move to next command line argument, even if errors are
    ! encountered below
    offset = offset + 1

    ! loop through all characters; note that argument values can
    ! only be specified for the very last argument, ie
    ! -xyz foo bar will assign arguments foo, bar as values to z
    do j = 1, len(cmd_arg)
        cmd_name = cmd_arg%substring (j, j)
        call self%find_arg (cmd_name, ptr_arg, status, is_abbrev=.true.)
        if (status /= FC_STATUS_OK) goto 200

        if (ptr_arg%nargs > 0 .and. j < len(cmd_arg)) then
            ! cannot satisfy any positive number of values for abbrev.
            ! arguments that are not the last character
            status = FC_STATUS_INVALID_STATE
            status%msg = "Argument '" // cmd_name  // "': expected " // &
                str(ptr_arg%nargs) // " arguments, found 0"
            goto 200
        else if (ptr_arg%nargs == 0) then
            ! can safely discard the status argument return value if
            ! no user-provided data is present, as nothing can go wrong.
            call ptr_arg%set (name=cmd_name, status=status)
            if (status /= FC_STATUS_OK) goto 100
        else if (ptr_arg%nargs > 0) then
            ! need to collect argument values
            call self%collect_values (cmd_name, cmd_args, offset, ptr_arg, &
                cmd_values, status)
            if (status /= FC_STATUS_OK) goto 100

            ! store command line arguments in argument object
            call ptr_arg%set (cmd_name, cmd_values, status)
            if (status /= FC_STATUS_OK) goto 100

            ! skip the next nargs arguments, those were used as values
            offset = offset + ptr_arg%nargs

            if (allocated(cmd_values)) deallocate (cmd_values)
        end if
    end do

    return

100 continue
    ! Post-processing of status objects received from argument methods if
    ! error occurs
    call process_argument_status (cmd_name, status)
    return

200 continue
    ! Post-processing of status objects populated in current routine
    ! Note: none required at this point.
end subroutine

subroutine argparser_collect_values (self, cmd_name, cmd_args, offset, ptr_arg, &
        cmd_values, status)

    class (argparser), intent(inout) :: self
    class (str), intent(in) :: cmd_name
        !*  Argument name, as specified by user on CLI
    class (str), intent(in), dimension(:) :: cmd_args
    integer, intent(in) :: offset
    class (argument), intent(in), pointer :: ptr_arg
    type (str), intent(out), dimension(:), allocatable :: cmd_values
    type (status_t), intent(out) :: status

    integer :: j, cmd_nargs

    call status%init (FC_STATUS_OK)
    cmd_nargs = size(cmd_args)

    if (allocated(cmd_values)) deallocate (cmd_values)
    allocate (cmd_values(ptr_arg%nargs))

    j = 0
    do while (j < ptr_arg%nargs)
        ! check that the number of command line arguments is not too low
        if (offset + j > cmd_nargs) then
            status = FC_STATUS_INVALID_STATE
            status%msg = "Argument '" // cmd_name  // "': expected " // &
                str(ptr_arg%nargs) // " arguments, found " // str(j-1)
            return
        end if

        cmd_values(j+1) = cmd_args(offset+j)
        j = j + 1
    end do

end subroutine

subroutine argparser_parse_unmapped (self, cmd_arg, status)
    class (argparser), intent(inout) :: self
    class (str), intent(in) :: cmd_arg

    type (str) :: cmd_name
    class (argument), pointer :: ptr_arg
    type (status_t), intent(out) :: status

    nullify (ptr_arg)
    
    cmd_name = str(UNMAPPED_ARG_NAME)
    ! Nothing should go wrong here since this is an argument object we
    ! added ourselves.
    call self%find_arg (cmd_name, ptr_arg, status, is_abbrev=.false.)

    call ptr_arg%set (cmd_name, cmd_arg)

end subroutine

pure subroutine process_argument_status (name, status)
    type (str), intent(in) :: name
    type (status_t), intent(inout) :: status

    ! prepend argument name to any non-empty error message received
    ! from argument type.
    if (status%msg /= "") then
        status%msg = "Argument '" // name // "': " // status%msg
    end if
end subroutine



function argparser_help_present (self, cmd_args) result(res)
    !*  HELP_PRESENT parses all command line arguments in order to determine
    !   whether the HELP argument is present. Errors encountered while
    !   parsing other arguments that might be present are ignored.

    class (argparser), intent(inout) :: self
    class (str), intent(in), dimension(:) :: cmd_args
    logical :: res
        !*  On exit this value is set to true if --help/-h was passed,
        !   and to false otherwise.

    integer :: i, cmd_nargs
    type (str) :: cmd_arg
    logical :: help_present
    class (argument), pointer :: ptr_arg
    type (status_t) :: lstatus

    help_present = .false.

    cmd_nargs = size(cmd_args)

    ! Process all command line arguments even though we are only looking
    ! for --help/-h. However, in principle --help or -h could be values
    ! to some other argument that accepts user-provided values, so use
    ! the full parsing infrastructure to handle those cases.
    ! We also ignore any parsing errors occurring when processing
    ! arguments other than --help/-h (no errors can occur with --help itself).
    i = 1
    do while (i <= cmd_nargs)
        cmd_arg = cmd_args(i)

        ! find associated argument object, either using the long name or the
        ! abbreviation
        if (cmd_arg%startswith ('--')) then
            call self%parse_long (cmd_args, i, lstatus)
        else if (cmd_arg%startswith ('-')) then
            call self%parse_abbrev (cmd_args, i, lstatus)
        else
            call self%parse_unmapped (cmd_arg, lstatus)
            ! Process unmapped arguments only one at a time.
            i = i + 1
        end if

        ! NB: i is incremented in parse_long / parse_abbrev routines
        ! as necessary.
    end do

    call self%find_arg (str(HELP_ARGNAME), ptr_arg)
    ! Since --help is added automatically when parser is Initialization,
    ! the pointer should in all cases point to a valid object!
    res = ptr_arg%is_present

    ! Reset args again
    call self%reset_args ()

end function


subroutine argparser_print_help (self)
    !*  PRINT_HELP prints the help message for each defined argument
    !   to standard output.

    class (argparser), intent(in) :: self

    class (iterator), allocatable :: iter
    class (*), pointer :: ptr_item
    class (argument), pointer :: ptr_arg

    type (str) :: progname, name, abbrev, help, line, help_line
    type (str) :: fmt_first_text, fmt_first_arg, fmt_cont, fmt_first_text_pos
    integer :: nargs, ifrom, ito, max_len, i

    integer, parameter :: INDENT = 2
    integer, parameter :: FIRST_MIN_TW = 30
    integer, parameter :: FIRST_TW = 50
        !   Text width on first line if there is at least FIRST_TW + PAD_WIDTH
        !   columns of space left
    integer, parameter :: CONT_TW = 50
        !   Text width on continuation lines
    integer, parameter :: PAD_WIDTH = 3

    nullify (ptr_arg, ptr_item)

    call self%args%get_iter (iter)

    fmt_first_text_pos = "(t" // str(INDENT + 1, "i0") // ", a, t" &
        // str(LINEWIDTH - FIRST_TW + 1, "i0") // ", a)"
    fmt_first_text = "(t" // str(INDENT+1, "i0") // ", a, tr" &
        // str(PAD_WIDTH, "i0") // ", a)"
    fmt_first_arg = "(t" // str(INDENT + 1, "i0") // ", a)"
    fmt_cont = "(t" // str(LINEWIDTH - CONT_TW + 1, "i0") // ", a)"


    if (self%description /= "") then
        write (OUTPUT_UNIT, "(a)") self%description%to_char()
    end if

    progname = self%progname
    if (progname == "") progname = "program_name"

    write (OUTPUT_UNIT, '(a, tr1, a, tr1, a)') "Usage: ", progname%to_char(), "[OPTIONS]"
    write (OUTPUT_UNIT, '(/,a)') "Supported options:"

    do while (iter%has_next())
        ptr_item => iter%item ()
        call dynamic_cast (ptr_item, ptr_arg)

        name = ptr_arg%get_name ()
        if (name == UNMAPPED_ARG_NAME) cycle

        help = ptr_arg%help
        nargs = ptr_arg%nargs

        line = ""
        do i = 1, size(ptr_arg%abbrevs)
            abbrev = ptr_arg%abbrevs(i)
            if (len(abbrev) > 0) then
                line = "-" // abbrev
                if (nargs >= 1) then
                    line = line // " <value>"
                end if
                line = line // ", "
            end if
        end do

        ! Append long names (incl. aliases)
        do i = 1, size(ptr_arg%names)
            name = ptr_arg%names(i)
            if (len(name) > 0) then
                line = "--" // name
                if (nargs >= 1) then
                    line = line // "=<value>"
                end if
                line = line // ", "

                ! Append TOGGLE prefix if applicable
                if (ptr_arg%action == ARGPARSE_ACTION_TOGGLE) then
                    name = ptr_arg%get_toggle_off_name (name)
                    line = line // "--" // name // ", "
                end if
            end if
        end do

        ! Adjust text to desired line length
        if (len(line) < LINEWIDTH - INDENT - PAD_WIDTH - FIRST_MIN_TW) then
            ! Plot argument abbrev, name and (first fragment of) help text.
            ifrom = 1
            if (len(line) < LINEWIDTH - INDENT - PAD_WIDTH - FIRST_TW)  then
                ! There is enough space to place FIRST_TW columns of help text
                call word_boundary (help, ifrom, FIRST_TW, ito)
                help_line = help%substring(ifrom, ito)
                write (OUTPUT_UNIT, fmt_first_text_pos%to_char()) line%to_char(), &
                    trim(help_line%to_char())
            else
                ! Not enough space to place FIRST_TW columns of help text;
                ! trim help text to fit line.
                max_len = LINEWIDTH - INDENT - PAD_WIDTH - len(line) + 1
                call word_boundary (help, ifrom, max_len, ito)
                help_line = help%substring(ifrom, ito)
                write (OUTPUT_UNIT, fmt_first_text%to_char()) line%to_char(), &
                    trim(help_line%to_char())
            end if
        else
            ! Plot only argument abbrev and name on the first line,
            ! no space left for help text.
            write (OUTPUT_UNIT, fmt_first_arg%to_char()) line%to_char()
            ito = 0
        end if

        ifrom = ito + 1
        do while (ifrom <= len(help))
            call word_boundary (help, ifrom, CONT_TW, ito)
            help_line = help%substring(ifrom, ito)
            write (OUTPUT_UNIT, fmt_cont%to_char()) trim(help_line%to_char())
            ifrom = ito + 1
        end do

    end do

contains
    subroutine word_boundary (s, ifrom, max_len, ito)
        type (str), intent(in) :: s
        integer, intent(in) :: ifrom
        integer, intent(in) :: max_len
        integer, intent(inout) :: ito

        if (len(s) - ifrom + 1 <= max_len) then
            ito = len(s)
        else
            ito = ifrom + max_len - 1
            do while (ito > ifrom .and. s%substring(ito,ito) /= " ")
                ito = ito - 1
            end do
        end if
    end subroutine
end subroutine


! ------------------------------------------------------------------------------
! Operators

subroutine argparser_assign (self, rhs)
    class (argparser), intent(inout) :: self
    class (argparser), intent(in) :: rhs

    class (iterator), allocatable :: iter
    class (*), pointer :: ptr_item
    class (argument), pointer :: ptr_arg_lhs, ptr_arg_rhs
    type (argument) :: arg

    self%progname = rhs%progname
    self%description = rhs%description
    self%status = rhs%status

    if (allocated(self%args)) deallocate (self%args)
    if (.not. allocated(rhs%args)) return

    ! At this point RHS%ARGS is allocated
    ! Ideally we would use LINKED_LIST's defined assignment to just copy
    ! over all ARGUMENT object, but this crashes with IFORT, so we do it
    ! manually.
    allocate (self%args)

    call rhs%args%get_iter (iter)

    do while (iter%has_next())
        ptr_item => iter%item ()
        call dynamic_cast (ptr_item, ptr_arg_rhs)

        ! Append "emtpy" ARGUMENT object that will be populated later.
        ! Note that this creates a copy of ARG within the linked list.
        call self%args%append (arg)
        ! Get pointer to last inserted ARGUMENT object
        ptr_item => self%args%item(size(self%args))
        call dynamic_cast (ptr_item, ptr_arg_lhs)

        ! Populate attributes using polymorphic defined assignment
        ptr_arg_lhs = ptr_arg_rhs

        nullify (ptr_item, ptr_arg_rhs, ptr_arg_lhs)
    end do

    if (allocated(iter)) deallocate (iter)

end subroutine

end module
