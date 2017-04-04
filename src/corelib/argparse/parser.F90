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

    use corelib_argparse_argument
    use corelib_argparse_actions

    implicit none
    private

    ! Status codes for internal use
    ! in intial state, no arguments added
    integer (CL_ENUM_KIND), parameter :: ARGPARSE_STATUS_INIT = 0
    integer (CL_ENUM_KIND), parameter :: ARGPARSE_STATUS_EMPTY_CMDLINE = ishft(1, 1)
    integer (CL_ENUM_KIND), parameter :: ARGPARSE_STATUS_PARSE_ERROR = ishft(1, 2)
    integer (CL_ENUM_KIND), parameter :: ARGPARSE_STATUS_PARSED = ishft(1, 3)

    ! only status code that needs to be communicated to client code
    public :: ARGPARSE_STATUS_PARSE_ERROR

    ! buffer size used to read in command line arguments
    integer, parameter :: CMD_BUFFER_SIZE = 1024
    ! buffer for printing error messages to stderr
    integer, parameter :: MSG_LENGTH = 100
    ! seperator used to split a list of arguments into individual tokens
    character (1), parameter :: LIST_SEP = ','

    ! placeholder to be used if user does not specify an abbreviated argument name
    ! Note that names and abbrevs are required to be alpha-numeric, so
    ! this can never be a valid user-given value.
    character (1), parameter :: UNDEFINED_ABBREV = '-'
    character (*), parameter :: UNMAPPED_ARG_NAME = '::UNMAPPED'

    type, public :: argparser
        private

        type (linked_list), allocatable :: args
        type (str) :: description
        integer :: status = ARGPARSE_STATUS_INIT
    contains
        procedure, pass :: argparser_add_argument_str
        procedure, pass :: argparser_add_argument_array_default_str
        procedure, pass :: argparser_add_argument_scalar_default_str
        procedure, pass :: argparser_add_argument_char
        procedure, pass :: argparser_add_argument_array_default_char
        procedure, pass :: argparser_add_argument_scalar_default_char
        generic, public :: add_argument => argparser_add_argument_str, &
            argparser_add_argument_array_default_str, &
            argparser_add_argument_scalar_default_str, &
            argparser_add_argument_array_default_char, &
            argparser_add_argument_char, &
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

        procedure, pass :: argparser_get_unmapped_array
        procedure, pass :: argparser_get_unmapped_scalar
        generic, public :: get_unmapped => argparser_get_unmapped_array, &
            argparser_get_unmapped_scalar

        procedure, pass :: argparser_parse_array
        procedure, pass :: argparser_parse_cmd
        generic, public :: parse => argparser_parse_array, argparser_parse_cmd

        procedure, pass :: append => argparser_append
        procedure, pass :: find_arg => argparser_find_arg
        procedure, pass :: parse_long => argparser_parse_long
        procedure, pass :: parse_abbrev => argparser_parse_abbrev
        procedure, pass :: parse_unmapped => argparser_parse_unmapped
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

        procedure, pass :: argparser_get_nvals_str
        procedure, pass :: argparser_get_nvals_char
        generic, public :: get_nvals => argparser_get_nvals_str, &
            argparser_get_nvals_char
        procedure, pass, public :: get_num_unmapped => argparser_get_num_unmapped
    end type

    interface len
        module procedure argparser_len
    end interface

    public :: len

contains

! ------------------------------------------------------------------------------
! Initialization

subroutine argparser_init_str (self, description)
    class (argparser), intent(in out) :: self
    class (str), intent(in), optional :: description

    type (argument) :: arg
    type (str) :: name
    type (status_t) :: status

    call self%reset ()

    if (present(description)) self%description = description

    ! Add "fake" argument object that will be used to store all unmapped
    ! command line arguments.
    name = str(UNMAPPED_ARG_NAME)
    call arg%init (name=name, action=ARGPARSE_ACTION_APPEND, status=status, &
        allow_empty=.true.)

    call self%append (arg)

end subroutine

subroutine argparser_init_char (self, description)
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

    class (argparser), intent(in out) :: self
    class (str), intent(in) :: name
    class (str), intent(in), optional :: abbrev
    integer (CL_ENUM_KIND), intent(in), optional :: action
    integer, intent(in), optional :: nargs
    logical, intent(in), optional :: required
    class (str), intent(in), optional :: help
    type (status_t), intent(out), optional :: status
    procedure (fcn_validator), optional :: validator
    class (*), intent(in), dimension(:), target :: default
    class (*), intent(in), dimension(:), optional, target :: const

    class (*), dimension(:), pointer :: ptr_default, ptr_const
    type (argument) :: arg
    type (status_t) :: lstatus

    nullify (ptr_default, ptr_const)

    call self%check_input (name, abbrev, action, nargs, lstatus)
    if (lstatus /= CL_STATUS_OK) goto 100

    call sanitize_argument_data_array (default, ptr_default)

    if (present(const)) then
        call sanitize_argument_data_array (const, ptr_const)
        call arg%init (name, abbrev, action, required, nargs, help, lstatus, &
            ptr_default, ptr_const, validator=validator)
    else
        call arg%init (name, abbrev, action, required, nargs, help, lstatus, &
            ptr_default, validator=validator)
    end if

    call process_argument_status (name, lstatus)
    if (lstatus == CL_STATUS_OK) call self%append (arg)

100 continue
    if (present(status)) status = lstatus
    ! Clean up memory that was potentially allocated to copy character-type
    ! default/const data into str
    call dealloc_argument_data_array (default, ptr_default)
    call dealloc_argument_data_array (const, ptr_const)
end subroutine

subroutine argparser_add_argument_str (self, name, abbrev, &
        action, nargs, required, help, status, validator)
    !*  Add argument definition using the following interface:
    !       1. str meta-data
    !       2. array values
    !       3. no 'default', no 'const'

    class (argparser), intent(in out) :: self
    class (str), intent(in) :: name
    class (str), intent(in), optional :: abbrev
    integer (CL_ENUM_KIND), intent(in), optional :: action
    integer, intent(in), optional :: nargs
    logical, intent(in), optional :: required
    class (str), intent(in), optional :: help
    type (status_t), intent(out), optional :: status
    procedure (fcn_validator), optional :: validator

    type (argument) :: arg
    type (status_t) :: lstatus

    call self%check_input (name, abbrev, action, nargs, lstatus)
    if (lstatus /= CL_STATUS_OK) goto 100

    call arg%init (name, abbrev, action, required, nargs, help, lstatus, &
        validator=validator)

    call process_argument_status (name, lstatus)
    if (lstatus == CL_STATUS_OK) call self%append (arg)

100 continue
    if (present(status)) status = lstatus
end subroutine

subroutine argparser_add_argument_scalar_default_str (self, name, abbrev, action, &
        nargs, required, help, status, validator, default, const)
    !*  Add argument definition using the following interface:
    !       1. str meta-data
    !       2. scalar values
    !       3. mandatory 'default', optional 'const'

    class (argparser), intent(in out) :: self
    class (str), intent(in) :: name
    class (str), intent(in), optional :: abbrev
    integer (CL_ENUM_KIND), intent(in), optional :: action
    integer, intent(in), optional :: nargs
    logical, intent(in), optional :: required
    class (str), intent(in), optional :: help
    type (status_t), intent(out), optional :: status
    procedure (fcn_validator), optional :: validator
    class (*), intent(in) :: default
    class (*), intent(in), optional :: const

    type (argument) :: arg
    type (status_t) :: lstatus
    class (*), dimension(:), pointer :: ptr_default, ptr_const

    nullify (ptr_default, ptr_const)

    call self%check_input (name, abbrev, action, nargs, lstatus)
    if (lstatus /= CL_STATUS_OK) goto 100

    call sanitize_argument_data_scalar (default, ptr_default)

    if (present(const)) then
        call sanitize_argument_data_scalar (const, ptr_const)
        call arg%init (name, abbrev, action, required, nargs, help, lstatus, &
            ptr_default, ptr_const, validator=validator)
    else
        call arg%init (name, abbrev, action, required, nargs, help, lstatus, &
            ptr_default, validator=validator)
    end if

    call process_argument_status (name, lstatus)
    if (lstatus == CL_STATUS_OK) call self%append (arg)

100 continue
    if (present(status)) status = lstatus
    if (associated(ptr_default)) deallocate (ptr_default)
    if (associated(ptr_const)) deallocate (ptr_const)

end subroutine

subroutine argparser_add_argument_scalar_default_char (self, name, abbrev, action, &
        nargs, required, help, status, validator, default, const)
    !*  Add argument definition using the following interface:
    !       1. character meta-data
    !       2. scalar values
    !       3. mandatory 'default', optional 'const'

    class (argparser), intent(in out) :: self
    character (*), intent(in) :: name
    character (*), intent(in), optional :: abbrev
    integer (CL_ENUM_KIND), intent(in), optional :: action
    integer, intent(in), optional :: nargs
    logical, intent(in), optional :: required
    character (*), intent(in), optional :: help
    type (status_t), intent(out), optional :: status
    procedure (fcn_validator), optional :: validator
    class (*), intent(in) :: default
    class (*), intent(in), optional :: const

    type (str) :: lhelp, labbrev, lname
    type (argument) :: arg
    type (status_t) :: lstatus
    class (*), dimension(:), pointer :: ptr_const, ptr_default

    nullify (ptr_default, ptr_const)

    call self%check_input (name, abbrev, action, nargs, lstatus)
    if (lstatus /= CL_STATUS_OK) goto 100

    call char_to_str_input (name, abbrev, help, lname, labbrev, lhelp)
    call sanitize_argument_data_scalar (default, ptr_default)

    if (present(const)) then
        call sanitize_argument_data_scalar (const, ptr_const)
        call arg%init (lname, labbrev, action, required, nargs, lhelp, lstatus, &
            ptr_default, ptr_const, validator=validator)
    else
        call arg%init (lname, labbrev, action, required, nargs, lhelp, lstatus, &
            ptr_default, validator=validator)
    end if

    call process_argument_status (lname, lstatus)
    if (lstatus == CL_STATUS_OK) call self%append (arg)

100 continue
    if (present(status)) status = lstatus
    if (associated(ptr_default)) deallocate (ptr_default)
    if (associated(ptr_const)) deallocate (ptr_const)
end subroutine

subroutine argparser_add_argument_array_default_char (self, name, abbrev, action, &
        nargs, required, help, status, validator, default, const)
    !*  Add argument definition using the following interface:
    !       1. character meta-data
    !       2. array values
    !       3. mandatory 'default', optional 'const'

    class (argparser), intent(in out) :: self
    character (*), intent(in) :: name
    character (*), intent(in), optional :: abbrev
    integer (CL_ENUM_KIND), intent(in), optional :: action
    integer, intent(in), optional :: nargs
    logical, intent(in), optional :: required
    character (*), intent(in), optional :: help
    type (status_t), intent(out), optional :: status
    procedure (fcn_validator), optional :: validator
    class (*), intent(in), dimension(:), target :: default
    class (*), intent(in), dimension(:), optional, target :: const

    class (*), dimension(:), pointer :: ptr_default, ptr_const

    type (str) :: lhelp, labbrev, lname
    type (argument) :: arg
    type (status_t) :: lstatus

    nullify (ptr_default, ptr_const)

    call self%check_input (name, abbrev, action, nargs, lstatus)
    if (lstatus /= CL_STATUS_OK) goto 100

    call char_to_str_input (name, abbrev, help, lname, labbrev, lhelp)
    call sanitize_argument_data_array (default, ptr_default)

    if (present(const)) then
        call sanitize_argument_data_array (const, ptr_const)
        call arg%init (lname, labbrev, action, required, nargs, lhelp, lstatus, &
            ptr_default, ptr_const, validator=validator)
    else
        call arg%init (lname, labbrev, action, required, nargs, lhelp, lstatus, &
            ptr_default, validator=validator)
    end if

    call process_argument_status (lname, lstatus)
    if (lstatus == CL_STATUS_OK) call self%append (arg)

100 continue
    if (present(status)) status = lstatus
    ! Clean up memory that was potentially allocated to copy character-type
    ! default/const data into str
    call dealloc_argument_data_array (default, ptr_default)
    call dealloc_argument_data_array (const, ptr_const)
end subroutine

subroutine argparser_add_argument_char (self, name, abbrev, action, &
        nargs, required, help, status, validator)
    !*  Add argument definition using the following interface:
    !       1. character meta-data
    !       2. array values
    !       3. no 'default', no 'const'

    class (argparser), intent(in out) :: self
    character (*), intent(in) :: name
    character (*), intent(in), optional :: abbrev
    integer (CL_ENUM_KIND), intent(in), optional :: action
    integer, intent(in), optional :: nargs
    logical, intent(in), optional :: required
    character (*), intent(in), optional :: help
    type (status_t), intent(out), optional :: status
    procedure (fcn_validator), optional :: validator

    type (str) :: lhelp, labbrev, lname
    type (argument) :: arg
    type (status_t) :: lstatus

    call self%check_input (name, abbrev, action, nargs, lstatus)
    if (lstatus /= CL_STATUS_OK) goto 100

    call char_to_str_input (name, abbrev, help, lname, labbrev, lhelp)

    call arg%init (lname, labbrev, action, required, nargs, lhelp, lstatus, &
        validator=validator)

    call process_argument_status (lname, lstatus)
    if (lstatus == CL_STATUS_OK) call self%append (arg)

100 continue
    if (present(status)) status = lstatus
end subroutine


subroutine sanitize_argument_data_scalar (src, ptr)
    !*  SANITIZE_ARGUMENT_DATA_SCALAR returns a pointer to a valid representation
    !   of the data contained in src. For all types other than character,
    !   an array of size 1 is allocated and src is copied into its first element.
    !   On exit, ptr points to this array.
    !
    !   If src is of type character, a str(1) array is allocated
    !   and the contents of src are copied into this array. On exit, ptr
    !   points to the newly allocated str array.
    !
    !   NOTE: User code is responsible for cleaning ptr in case it
    !   points to memory allocated in this routine!
    class (*), intent(in), target :: src
    class (*), intent(out), dimension(:), pointer :: ptr

    select type (src)
    type is (character (*))
        allocate (ptr(1), source=str(src))
    class default
        allocate (ptr(1), source=src)
    end select

end subroutine

subroutine sanitize_argument_data_array (src, ptr)
    !*  SANITIZE_ARGUMENT_DATA_ARRAY returns a pointer to a valid representation
    !   of the data contained in src. For all types other than character,
    !   ptr simply points to src on exit.
    !
    !   If src is of type character, a str array is allocated
    !   and the contents of src are copied into this array. On exit, ptr
    !   points to the newly allocated str array.
    !
    !   NOTE: User code is responsible for cleaning ptr in case it
    !   points to memory allocated in this routine!
    class (*), intent(in), dimension(:), target :: src
    class (*), intent(out), dimension(:), pointer :: ptr

    type (str), dimension(:), pointer :: ptr_str
    integer :: n, i

    select type (src)
    type is (character (*))
        n = size(src)
        allocate (ptr_str(n))
        do i = 1, n
            ptr_str(i) = src(i)
        end do
        ptr => ptr_str
    class default
        ptr => src
    end select
end subroutine

subroutine dealloc_argument_data_array (src, ptr)
    !*  DEALLOC_ARGUMENT_DATA_ARRAY conditionally deallocates the memory
    !   pointed to by ptr if: (1) src is present; and (2) ptr is
    !   with an object OTHER THAN src.
    !   This routine should be used to deallocate memory allocated by
    !   SANITIZE_ARGUMENT_DATA_ARRAY.
    class (*), intent(in), dimension(:), optional, target :: src
    class (*), intent(out), dimension(:), pointer :: ptr

    if (present(src)) then
        if (associated(ptr) .and. .not. associated(ptr, src))  then
            deallocate (ptr)
        end if
    end if

end subroutine


subroutine char_to_str_input (cname, cabbrev, chelp, name, abbrev, help)
    !*  CHAR_TO_STR_INPUT converts user-given character-type argument meta-data
    !   name/abbrev/etc. to str values.
    character (*), intent(in) :: cname
    character (*), intent(in), optional :: cabbrev, chelp
    type (str), intent(out) :: name, abbrev, help

    name = trim(cname)
    abbrev = str(UNDEFINED_ABBREV)
    if (present(cabbrev)) abbrev = trim(cabbrev)
    help = ""
    if (present(chelp)) help = trim(chelp)

end subroutine


!-------------------------------------------------------------------------------
! Input validation

subroutine argparser_check_input_str (self, name, abbrev, action, nargs, status)
    class (argparser), intent(in) :: self
    class (str), intent(in) :: name
    class (str), intent(in), optional :: abbrev
    integer (CL_ENUM_KIND), intent(in), optional :: action
    integer, intent(in), optional :: nargs
    type (status_t), intent(out) :: status

    ! by default return invalid input status
    call status%init (CL_STATUS_VALUE_ERROR)

    call validate_identifier (name, status)
    if (status /= CL_STATUS_OK) then
        status%msg = "Invalid name: " // status%msg
        return
    end if

    ! check whether argument with this name is already defined
    if (self%is_defined (name)) then
        status%msg = "Invalid name '" // name // &
            "': argument with same name already defined"
        return
    end if

    if (present(abbrev)) then
        if (len(abbrev) /= 1) then
            status = CL_STATUS_VALUE_ERROR
            status%msg = "Invalid abbrev '" // abbrev // &
                "': value must be character of length 1"
            return
        end if

        call validate_identifier (abbrev, status)
        if (status /= CL_STATUS_OK) then
            status%msg = "Invalid abbrev '" // abbrev // "': " // status%msg
            return
        end if

        ! check whether abbreviation of this name already exists
        if (self%is_defined (abbrev, is_abbrev=.true.)) then
            status%msg = "Invalid abbrev '" // abbrev // &
                "' : argument with save abbrev already defined"
            return
        end if
    end if

    call validate_action (action, status)
    if (status /= CL_STATUS_OK) then
        status%msg = "Invalid action for argument '" // name // "'"
        return
    end if

    if (present(nargs)) then
        if (nargs < 0) then
            status%msg = "Invalid nargs for argument '" // name // "'"
            return
        end if
    end if

    call status%init (CL_STATUS_OK)
end subroutine

subroutine argparser_check_input_char (self, name, abbrev, action, nargs, status)
    class (argparser), intent(in) :: self
    character (*), intent(in) :: name
    character (*), intent(in), optional :: abbrev
    integer (CL_ENUM_KIND), intent(in), optional :: action
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

    call status%init (CL_STATUS_OK)

    n = len(s)
    if (n == 0) then
        status = CL_STATUS_VALUE_ERROR
        status%msg = "Empty value not allowed"
        return
    end if
    if (s%startswith('-')) then
        status = CL_STATUS_VALUE_ERROR
        status%msg = "String value must not start with '-'"
    end if

    allocate (character (n) :: work)
    work = s

    do i = 1, n
        is_valid = any(work(i:i) == valid_special) .or. is_alnum (work(i:i))
        if (.not. is_valid) then
            status = CL_STATUS_VALUE_ERROR
            status%msg = "Invalid character encountered: '" // work(i:i) // "'"
        end if
    end do

end subroutine


subroutine validate_action (action, status)
    !*  VALIDATE_ACTION verifies that a user-provided integer value corresponds
    !   to a valid ARGPARSER action.
    integer (CL_ENUM_KIND), intent(in), optional :: action
    type (status_t), intent(out) :: status

    call status%init (CL_STATUS_OK)
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
        status = CL_STATUS_VALUE_ERROR
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
    class (*), intent(in out), dimension(:) :: val
    type (status_t), intent(out), optional :: status

    class (argument), pointer :: ptr_arg
    type (status_t) :: lstatus

    call lstatus%init (CL_STATUS_OK)

    call validate_identifier (name, lstatus)
    if (lstatus /= CL_STATUS_OK) goto 100

    call argparser_get_check_state (self, lstatus)
    if (lstatus /= CL_STATUS_OK) goto 100

    call self%find_arg (name, ptr_arg, lstatus)
    if (lstatus /= CL_STATUS_OK) goto 100

    ! at this point ptr_arg points to the argument identified by name.
    ! Retrieve stored argument value
    call ptr_arg%parse (val, lstatus)

    call process_argument_status (name, lstatus)

100 continue
    if (present(status)) status = lstatus
end subroutine

subroutine argparser_get_scalar_str (self, name, val, status)
    class (argparser), intent(in out) :: self
    type (str), intent(in) :: name
    class (*), intent(in out) :: val
    type (status_t), intent(out), optional :: status

    class (argument), pointer :: ptr_arg
    type (status_t) :: lstatus

    call lstatus%init (CL_STATUS_OK)

    call validate_identifier (name, lstatus)
    if (lstatus /= CL_STATUS_OK) goto 100

    call argparser_get_check_state (self, lstatus)
    if (lstatus /= CL_STATUS_OK) goto 100

    call self%find_arg (name, ptr_arg, lstatus)
    if (lstatus /= CL_STATUS_OK) goto 100

    ! at this point ptr_arg points to the argument identified by name.
    ! Retrieve stored argument value
    call ptr_arg%parse (val, lstatus)

    call process_argument_status (name, lstatus)

100 continue
    if (present(status)) status = lstatus
end subroutine

subroutine argparser_get_array_char (self, name, val, status)
    class (argparser), intent(in out) :: self
    character (*), intent(in) :: name
    class (*), intent(out), dimension(:) :: val
    type (status_t), intent(out), optional :: status

    call self%get (str(name), val, status)
end subroutine

subroutine argparser_get_scalar_char (self, name, val, status)
    class (argparser), intent(in out) :: self
    character (*), intent(in) :: name
    class (*), intent(out) :: val
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
    class (*), intent(in out), dimension(:) :: val
    type (status_t), intent(out), optional :: status

    class (argument), pointer :: ptr_arg
    type (status_t) :: lstatus
    type (str) :: name
    integer :: nvals

    call lstatus%init (CL_STATUS_OK)

    call argparser_get_check_state (self, lstatus)
    if (lstatus /= CL_STATUS_OK) goto 100

    name = UNMAPPED_ARG_NAME
    call self%find_arg (name, ptr_arg)

    ! Handle dimension check upfront, even though argument::parse also does this.
    nvals = ptr_arg%get_nvals ()
    if (nvals == 0) then
        lstatus = CL_STATUS_INVALID_STATE
        lstatus%msg = "No unmapped command line arguments present."
        goto 100
    else if (size(val) < nvals) then
        lstatus = CL_STATUS_VALUE_ERROR
        lstatus%msg = "Array size insufficient to store all unmapped arguments"
        goto 100
    end if

    call ptr_arg%parse (val, lstatus)

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
    class (*), intent(in out) :: val
    type (status_t), intent(out), optional :: status

    class (argument), pointer :: ptr_arg
    type (status_t) :: lstatus
    type (str) :: name
    integer :: nvals

    call lstatus%init (CL_STATUS_OK)

    call argparser_get_check_state (self, lstatus)
    if (lstatus /= CL_STATUS_OK) goto 100

    name = UNMAPPED_ARG_NAME
    call self%find_arg (name, ptr_arg)

    ! Handle dimension check upfront, even though argument::parse also does this.
    nvals = ptr_arg%get_nvals ()
    if (nvals == 0) then
        lstatus = CL_STATUS_INVALID_STATE
        lstatus%msg = "No unmapped command line arguments present."
        goto 100
    else if (nvals > 1) then
        lstatus = CL_STATUS_VALUE_ERROR
        lstatus%msg = "Array size insufficient to store all unmapped arguments"
        goto 100
    end if

    call ptr_arg%parse (val, lstatus)

100 continue
    if (present(status)) status = lstatus
end subroutine


subroutine argparser_get_check_state (self, status)
    class (argparser), intent(in) :: self
    type (status_t), intent(out) :: status

    call status%init (CL_STATUS_OK)

    if (len(self) == 0) then
        status = CL_STATUS_INVALID_STATE
        status%msg = "No arguments defined"
        return
    else if (self%status == ARGPARSE_STATUS_PARSE_ERROR) then
        status = CL_STATUS_INVALID_STATE
        status%msg = "Cannot retrieve argument due to previous parsing error"
        return
    else if (self%status /= ARGPARSE_STATUS_PARSED) then
        status = CL_STATUS_UNKNOWN
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

    logical :: lis_abbrev

    lis_abbrev = .false.
    if (present(is_abbrev)) lis_abbrev = is_abbrev

    nullify (ptr_arg)

    if (len(self) == 0) then
        if (present(status)) then
            status = CL_STATUS_INVALID_STATE
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

        if (lis_abbrev) then
            if (ptr_arg%abbrev == name) exit
        else
            if (ptr_arg%name == name) exit
        end if
        nullify (ptr_arg)
    end do

    if (present(status)) then
        call status%init (CL_STATUS_OK)
        if (.not. associated (ptr_arg)) then
            status = CL_STATUS_VALUE_ERROR
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
    type (status_t), intent(out), optional :: status

    integer :: i, cmd_nargs
    type (status_t) :: lstatus
    type (str) :: cmd_arg

    call lstatus%init (CL_STATUS_OK)

    if (len(self) == 0) then
        status = CL_STATUS_INVALID_STATE
        status%msg = "Need to define arguments before parsing"
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
            if (lstatus /= CL_STATUS_OK) goto 100
        else if (cmd_arg%startswith ('-')) then
            call self%parse_abbrev (cmd_args, i, lstatus)
            if (lstatus /= CL_STATUS_OK) goto 100
        else
            call self%parse_unmapped (cmd_arg, lstatus)
            if (lstatus /= CL_STATUS_OK) goto 100
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
    if (lstatus /= CL_STATUS_OK) self%status = ARGPARSE_STATUS_PARSE_ERROR

end subroutine


subroutine argparser_parse_cmd (self, status)
    !*  ARGPARSE_PARSE_CMD parses arguments provided at the command line
    class (argparser), intent(in out) :: self
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
    class (argparser), intent(in out) :: self
    _POLYMORPHIC_ARRAY (str), intent(in), dimension(:) :: cmd_args
    integer, intent(in out) :: offset
    type (status_t), intent(out) :: status

    type (str) :: cmd_name, str_value, cmd_arg
    type (str), dimension(:), allocatable :: cmd_values
    class (argument), pointer :: ptr_arg

    integer :: j, cmd_nargs

    call status%init (CL_STATUS_OK)
    cmd_nargs = size(cmd_args)

    ! remove leading -- from long syntax
    cmd_arg = cmd_args(offset)%substring (3, -1)

    ! check whether there is an = and separate token in that case
    j = index (cmd_arg, "=")
    if (j > 0) then
        cmd_name = cmd_arg%substring (j-1)
    else
        cmd_name = cmd_arg
    end if

    ! find corresponding argument object
    call self%find_arg (cmd_name, ptr_arg, status, is_abbrev=.false.)
    if (status /= CL_STATUS_OK) goto 200

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
        status%msg = "Argument '" // ptr_arg%name // &
            "': received one argument value, expected " // str(ptr_arg%nargs)
        goto 200
    end if

    ! store command line arguments in argument object
    if (allocated (cmd_values)) then
        call ptr_arg%set (cmd_values, status)
        if (status /= CL_STATUS_OK) goto 100
    else
        ! collect the number of requested arguments from the following commands
        ! This also supports collecting 0 values (e.g. if action is
        ! STORE_CONST), then cmd_values is allocated to cmd_values(1:0).
        call self%collect_values (cmd_args, offset+1, ptr_arg, cmd_values, status)
        if (status /= CL_STATUS_OK) goto 100

        ! store command line arguments in argument object
        call ptr_arg%set (cmd_values, status)
        if (status /= CL_STATUS_OK) goto 100

        ! skip the next nargs arguments, those were used as values
        offset = offset + ptr_arg%nargs
    end if

    ! move to next command line argument
    offset = offset + 1

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
    class (argparser), intent(in out) :: self
    _POLYMORPHIC_ARRAY (str), intent(in), dimension(:) :: cmd_args
    integer, intent(in out) :: offset
    type (status_t), intent(out) :: status

    type (str) :: cmd_name, cmd_arg
    type (str), dimension(:), allocatable :: cmd_values
    class (argument), pointer :: ptr_arg

    integer :: j, cmd_nargs

    call status%init (CL_STATUS_OK)
    cmd_nargs = size(cmd_args)

    ! remove leading - from argument (list)
    cmd_arg = cmd_args(offset)%substring (2, -1)

    ! loop through all characters; note that argument values can
    ! only be specified for the very last argument, ie
    ! -xyz foo bar will assign arguments foo, bar as values to z
    do j = 1, len(cmd_arg)
        cmd_name = cmd_arg%substring (j, j)
        call self%find_arg (cmd_name, ptr_arg, status, is_abbrev=.true.)
        if (status /= CL_STATUS_OK) goto 200

        if (ptr_arg%nargs > 0 .and. j < len(cmd_arg)) then
            ! cannot satisfy any positive number of values for abbrev.
            ! arguments that are not the last character
            status = CL_STATUS_INVALID_STATE
            status%msg = "Argument '" // ptr_arg%name  // "': expected " // &
                str(ptr_arg%nargs) // " arguments, found 0"
            goto 200
        else if (ptr_arg%nargs == 0) then
            ! can safely discard the status argument return value if
            ! no user-provided data is present, as nothing can go wrong.
            call ptr_arg%set (status=status)
            if (status /= CL_STATUS_OK) goto 100
        else if (ptr_arg%nargs > 0) then
            ! need to collect argument values
            call self%collect_values (cmd_args, offset+1, ptr_arg, &
                cmd_values, status)
            if (status /= CL_STATUS_OK) goto 100

            ! store command line arguments in argument object
            call ptr_arg%set (cmd_values, status)
            if (status /= CL_STATUS_OK) goto 100

            ! skip the next nargs arguments, those were used as values
            offset = offset + ptr_arg%nargs

            if (allocated(cmd_values)) deallocate (cmd_values)
        end if
    end do

    ! move to next command line argument
    offset = offset + 1

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

subroutine argparser_collect_values (self, cmd_args, offset, ptr_arg, &
        cmd_values, status)

    class (argparser), intent(in out) :: self
    _POLYMORPHIC_ARRAY (str), intent(in), dimension(:) :: cmd_args
    integer, intent(in) :: offset
    class (argument), intent(in), pointer :: ptr_arg
    type (str), intent(out), dimension(:), allocatable :: cmd_values
    type (status_t), intent(out) :: status

    integer :: j, cmd_nargs

    call status%init (CL_STATUS_OK)
    cmd_nargs = size(cmd_args)

    if (allocated(cmd_values)) deallocate (cmd_values)
    allocate (cmd_values(ptr_arg%nargs))

    j = 0
    do while (j < ptr_arg%nargs)
        ! check that the number of command line arguments is not too low
        if (offset + j > cmd_nargs) then
            status = CL_STATUS_INVALID_STATE
            status%msg = "Argument '" // ptr_arg%name  // "': expected " // &
                str(ptr_arg%nargs) // " arguments, found " // str(j-1)
            return
        end if

        cmd_values(j+1) = cmd_args(offset+j)
        j = j + 1
    end do

end subroutine

subroutine argparser_parse_unmapped (self, cmd_arg, status)
    class (argparser), intent(in out) :: self
    class (str), intent(in) :: cmd_arg

    type (str) :: cmd_name
    class (argument), pointer :: ptr_arg
    type (status_t), intent(out) :: status

    cmd_name = str(UNMAPPED_ARG_NAME)
    ! Nothing should go wrong here since this is an argument object we
    ! added ourselves.
    call self%find_arg (cmd_name, ptr_arg, status, is_abbrev=.false.)

    call ptr_arg%set (cmd_arg)

end subroutine

pure subroutine process_argument_status (name, status)
    type (str), intent(in) :: name
    type (status_t), intent(in out) :: status

    ! prepend argument name to any non-empty error message received
    ! from argument type.
    if (status%msg /= "") then
        status%msg = "Argument '" // name // "': " // status%msg
    end if
end subroutine

end module
