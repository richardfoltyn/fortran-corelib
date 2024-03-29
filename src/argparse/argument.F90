
#include <fcore.h>


module fcore_argparse_argument

    use iso_fortran_env
    use fcore_common
    use fcore_common_status_helpers
    use fcore_common_input_validation
    use fcore_collections
    use fcore_argparse_actions
    use fcore_argparse_argument_data

    implicit none
    private

    public :: fcn_validator
    public :: dynamic_cast
    public :: argument_data_alloc

    character (*), parameter :: ARGUMENT_ACTION_TOGGLE_OFF_PREFIX = 'no'


    type, public :: argument
        private
        type (str), dimension(:), allocatable, public :: names
        type (str), dimension(:), allocatable, public :: abbrevs
        type (argument_data) :: default
        type (argument_data) :: const
        integer (FC_ENUM_KIND), public :: action = ARGPARSE_ACTION_STORE
        logical :: required = .false.
        logical, public :: is_present = .false.
        integer, public :: nargs = 1
        type (str), dimension(:), allocatable :: passed_values
        type (str), public :: help
        procedure (fcn_validator), nopass, pointer :: validator => null()
    contains
        procedure, pass :: init_data => argument_init_data
        procedure, pass :: init_default => argument_init_default
        generic, public :: init => init_data, init_default

        procedure, public, pass :: reset => argument_reset

        procedure, public, pass :: matches => argument_matches

        procedure, pass :: set_scalar => argument_set_scalar
        procedure, pass :: set_array => argument_set_array
        generic, public :: set => set_scalar, set_array

        procedure, pass :: process_cmd_value => argument_process_cmd_value

        procedure, pass :: argument_parse_array_int32
        procedure, pass :: argument_parse_array_int64
        procedure, pass :: argument_parse_array_real32
        procedure, pass :: argument_parse_array_real64
        procedure, pass :: argument_parse_array_logical
        procedure, pass :: argument_parse_array_str
        procedure, pass :: argument_parse_array_char
        procedure, pass :: argument_parse_scalar_int32
        procedure, pass :: argument_parse_scalar_int64
        procedure, pass :: argument_parse_scalar_real32
        procedure, pass :: argument_parse_scalar_real64
        procedure, pass :: argument_parse_scalar_logical
        procedure, pass :: argument_parse_scalar_str
        procedure, pass :: argument_parse_scalar_char
        generic :: parse => argument_parse_array_int32, &
            argument_parse_array_int64, &
            argument_parse_array_real32, &
            argument_parse_array_real64, &
            argument_parse_array_logical, &
            argument_parse_array_str, &
            argument_parse_array_char, &
            argument_parse_scalar_int32, &
            argument_parse_scalar_int64, &
            argument_parse_scalar_real32, &
            argument_parse_scalar_real64, &
            argument_parse_scalar_logical, &
            argument_parse_scalar_str, &
            argument_parse_scalar_char

        procedure, pass :: argument_poly_parse_array
        procedure, pass :: argument_poly_parse_scalar
        generic, public :: poly_parse => argument_poly_parse_scalar, &
            argument_poly_parse_array

        procedure, pass :: argument_parse_check_input_scalar
        procedure, pass :: argument_parse_check_input_array
        generic :: parse_check_input => argument_parse_check_input_scalar, &
            argument_parse_check_input_array

        procedure, pass :: get_nvals => argument_get_nvals

        procedure, public, pass :: get_name => argument_get_name
        procedure, public, nopass :: get_toggle_off_name => argument_get_toggle_off_name

        procedure, pass :: argument_assign
        generic, public :: assignment(=) => argument_assign

    end type

    interface dynamic_cast
        module procedure cast_any_to_argument
    end interface

    abstract interface
        subroutine fcn_validator (val, status)
            import str, status_t
            type (str), intent(in) :: val
            type (status_t), intent(out) :: status
        end subroutine
    end interface

    contains

! ------------------------------------------------------------------------------
! Initialization

subroutine argument_init_default (self)
    !*  ARGUMENT_INIT_DEFAULT initializes argument objects to its pristine
    !   state.

    class (argument), intent(inout) :: self

    if (allocated (self%passed_values)) deallocate (self%passed_values)
    if (allocated (self%names)) deallocate (self%names)
    if (allocated (self%abbrevs)) deallocate (self%abbrevs)

    call argument_data_reset (self%default)
    call argument_data_reset (self%const)

    self%action = ARGPARSE_ACTION_STORE
    self%is_present = .false.
    self%required = .false.
    self%nargs = 1
    self%validator => null()
end subroutine


subroutine argument_init_data (self, names, abbrevs, action, required, nargs, &
        help, status, default, const, validator)
    !*  ARGUMENT_INIT_DATA initializes argument object from user-provided data.

    class (argument), intent(inout) :: self
    type(str), intent(in), dimension(:) :: names
    type(str), intent(in), dimension(:), optional :: abbrevs
    integer (FC_ENUM_KIND), intent(in), optional :: action
    logical, intent(in), optional :: required
    integer, intent(in), optional :: nargs
    class (str), intent(in), optional :: help
    type (argument_data), intent(in), optional :: default
    type (status_t), intent(out) :: status
    type (argument_data), intent(in), optional :: const
    procedure (fcn_validator), optional :: validator

    integer :: laction, lnargs
    logical :: lrequired
    integer :: n
    
    ! default values
    call status%init (FC_STATUS_OK)
    laction = ARGPARSE_ACTION_STORE
    lrequired = .false.
    lnargs = 1

    ! overwrite with provided arguments
    if (present(action)) laction = action
    if (present(nargs)) lnargs = nargs
    if (present(required)) lrequired = required

    ! Validate input arguments. Permissible arguments vary by action!
    ! Call with local action but user-provided nargs, default, const, ...
    ! to properly handle default action.
    call argument_init_validate_input (names, abbrevs, laction, required, nargs, &
        help, default, const, validator, status)

    ! Default initalization
    call self%init ()

    select case (laction)
    case (ARGPARSE_ACTION_STORE_TRUE)
        lnargs = 0
        laction = ARGPARSE_ACTION_STORE_CONST
        call argument_data_alloc (self%const, .true.)
        call argument_data_alloc (self%default, .false.)

    case (ARGPARSE_ACTION_STORE_FALSE)
        lnargs = 0
        laction = ARGPARSE_ACTION_STORE_CONST
        call argument_data_alloc (self%const, .false.)
        call argument_data_alloc (self%default, .true.)

    case (ARGPARSE_ACTION_STORE_CONST)
        ! no user-provided CLI arguments expected when requested to store const
        lnargs = 0
        ! Note: we check above that DEFAULT and CONST are present for this
        ! action type.
        self%default = default
        self%const = const

    case (ARGPARSE_ACTION_TOGGLE)
        lnargs = 0
        ! If no default value specified, assume that switch
        ! should be ON by default.
        if (present(default)) then
            self%default = default
        else
            call argument_data_alloc (self%default, .true.)
        end if

    case (ARGPARSE_ACTION_APPEND)
        ! NB: this should be guaranteed by input checking above
        lnargs = 1
        if (present(default)) self%default = default

    case default
        if (present(default)) self%default = default

    end select

    n = size(names)
    allocate (self%names(n), source=names)

    self%action = laction
    self%nargs = lnargs
    self%required = lrequired

    ! Ensure that abbrevs array is allocated, even if no abbrevs are specified
    if (present(abbrevs)) then
        n = size(abbrevs)
        allocate (self%abbrevs(n), source=abbrevs)
    else
        allocate (self%abbrevs(0))
    end if

    if (present(help)) self%help = help
    if (present(validator)) self%validator => validator

end subroutine

subroutine argument_init_validate_input (names, abbrevs, action, required, nargs, &
        help, default, const, validator, status)
    !*  ARGUMENT_INIT_VALIDATE_INPUT performs input validation for user-provided
    !   subroutine arguments used to build argument object.

    type(str), intent(in), dimension(:) :: names
    type(str), intent(in), dimension(:), optional :: abbrevs
    integer (FC_ENUM_KIND), intent(in), optional :: action
    logical, intent(in), optional :: required
    integer, intent(in), optional :: nargs
    class (str), intent(in), optional :: help
    type (argument_data), intent(in), optional :: default
    type (status_t), intent(out) :: status
    type (argument_data), intent(in), optional :: const
    procedure (fcn_validator), optional :: validator

    type (str) :: argname

    argname = ''

    call status%init (FC_STATUS_VALUE_ERROR)

    if (size(names) == 0) then
        status%msg = "No argument name specified"
        goto 100
    end if

    call argument_init_check_names (names, status)
    if (status /= FC_STATUS_OK) goto 100
    call argument_init_check_names (abbrevs, status)
    if (status /= FC_STATUS_OK) goto 100

    select case (action)
    case (ARGPARSE_ACTION_STORE_TRUE)
        argname = 'nargs'
        if (present(nargs)) goto 10

        argname = 'validator'
        if (present(validator)) goto 10

        argname = 'default'
        if (present(default)) goto 10

        argname = 'const'
        if (present(const)) goto 10

     case (ARGPARSE_ACTION_STORE_FALSE)
        argname = 'nargs'
        if (present(nargs)) goto 10

        argname = 'validator'
        if (present(validator)) goto 10

        argname = 'default'
        if (present(default)) goto 10

        argname = 'const'
        if (present(const)) goto 10

    case (ARGPARSE_ACTION_STORE_CONST)
        argname = 'nargs'
        if (present(nargs)) goto 10

        argname = 'validator'
        if (present(validator)) goto 10

        argname = 'default'
        if (.not. present(default)) goto 20

        argname = 'const'
        if (.not. present(const)) goto 20

    case (ARGPARSE_ACTION_TOGGLE)
        ! Do not allow abbrev. with TOGGLE action, as turning switch off
        ! would need to be called as a 'long' argument, ie. --no-x, 
        ! which is inconsistent.
        argname = 'abbrevs'
        if (present(abbrevs)) goto 10

        argname = 'nargs'
        if (present(nargs)) goto 10

        argname = 'validator'
        if (present(validator)) goto 10

        argname = 'const'
        if (present(const)) goto 10

    case (ARGPARSE_ACTION_APPEND)
        argname = 'nargs'
        if (present(nargs)) goto 10

    end select

    ! No errors encountered at this point, return
    status = FC_STATUS_OK
    return

10  continue
    ! Error handler for present argument when none is allows
    status%msg = "Action " // get_action_label (action) &
        // " does not support '" // argname // "' argument"
    return

20 continue
    ! Error handler for missing argument that is required for particular action.
    status%msg = "Argument " // argname // " required for action " &
            // get_action_label (action)
    return

100 continue

end subroutine


subroutine argument_init_check_names (names, status)
    type (str), intent(in), dimension(:), optional :: names
    type (status_t), intent(out) :: status

    integer :: i

    status = FC_STATUS_OK

    if (present(names)) then
        do i = 1, size(names)
            if (len(names(i)) == 0) then
                status = FC_STATUS_VALUE_ERROR
                status%msg = "Empty argument names not permitted"
                return
            end if
        end do
    end if

end subroutine

! ------------------------------------------------------------------------------
! RESET method

subroutine argument_reset (self)
    !*  ARGUMENT_RESET resets argument object to its pre-parsing state, ie
    !   reverts changes performed by SET().
    !   Note: Does not reset instance to its initial state, ie. the setup
    !   performed by INIT() remains unchanged.

    class (argument), intent(inout) :: self

    if (allocated (self%passed_values)) deallocate (self%passed_values)

    self%is_present = .false.
end subroutine

! ------------------------------------------------------------------------------
! SET methods

subroutine argument_set_scalar (self, name, val, status)
    !*  ARGUMENT_SET_SCALAR is a wrapper around ARGUMENT_SET_ARRAY
    !   for scalar command line argument values.

    class (argument), intent(inout) :: self
    type (str), intent(in) :: name
    type (str), intent(in) :: val
    type (status_t), intent(out), optional :: status

    type (str), dimension(:), allocatable :: work

    allocate (work(1))
    work(1) = val
    call self%set (name, work, status)
    deallocate (work)

end subroutine

subroutine argument_set_array (self, name, val, status)
    !*  ARGUMENT_SET_ARRAY stores any command line arguments passed
    !   in as values to this argument instance. Performs input
    !   validation if requested.
    !
    !   Implementation detail: the argument value is store as type(str),
    !   as the eventual data type that the user will request is
    !   not known at this point.

    class (argument), intent(inout) :: self
    type (str), intent(in) :: name
    type (str), intent(in), dimension(:), optional :: val
    type (status_t), intent(out), optional :: status

    type (str) :: noname, alias
    type (status_t) :: lstatus

    integer :: i

    lstatus = FC_STATUS_OK

    if (self%action == ARGPARSE_ACTION_TOGGLE) then
        do i = 1, size(self%names)
            alias = self%names(i)
            noname = argument_get_toggle_off_name (alias)
            if (name == noname) then
                call argument_data_alloc (self%const, .false.)
                exit
            else if (name == alias) then
                call argument_data_alloc (self%const, .true.)
                exit
            end if
        end do
    end if

    ! Note: no further code needed to deal with ACTION_STORE_CONST,
    ! will automatically read the default or stored const value when
    ! get() routine is called.

    if (present(val)) then
        if (size(val) > 0) then
            ! Delegate actuall processing of non-zero-length value
            ! array.
            call self%process_cmd_value (val, lstatus)
            if (lstatus /= FC_STATUS_OK) goto 100
        end if
    end if

    ! Argument was present in command line invocation
    self%is_present = .true.

100 continue
    if (present(status)) status = lstatus
end subroutine

subroutine argument_process_cmd_value (self, val, status)
    !*  ARGUMENT_PROCESS_CMD_VALUE performs the actual input
    !   validation and storing of argument values passed in via
    !   one of the front-end routines.
    !
    !   Implementation detail: this routine should only be called for
    !   ACTION_STORE and ACTION_APPEND, as none of the other actions
    !   accepts user-provided arguments.

    class (argument), intent(inout) :: self
    type (str), intent(in), dimension(:) :: val
    type (status_t), intent(out) :: status

    type (str), dimension(:), allocatable :: work
    integer :: n, m, i

    status = FC_STATUS_OK
    n = size(val)

    ! For actions STORE_TRUE, STORE_FALSE and STORE_CONST, no values
    ! are permitted.
    if (self%nargs == 0) then
        status = FC_STATUS_UNSUPPORTED_OP
        status%msg = "Argument does not accept user-provided values"
        goto 100
    end if

    if (associated(self%validator)) then
        do i = 1, n
            call self%validator (val(i), status)
            if (status /= FC_STATUS_OK) goto 100
        end do
    end if

    select case (self%action)
    ! if requested to append repeated appearances of argument on command
    ! line, allocate or reallocate passed_values array as required
    case (ARGPARSE_ACTION_APPEND)
        if (.not. allocated(self%passed_values)) then
            allocate (self%passed_values(n), source=val)
        else
            ! need to reallocate new array that is large enough to hold
            ! existing data as well as val
            m = size(self%passed_values)
            allocate (work(m+n))
            work(1:m) = self%passed_values
            work(m+1:m+n) = val
            call move_alloc (work, self%passed_values)
        end if
    ! in all other cases, discard any previous values and store only val
    case default
        if (allocated(self%passed_values)) deallocate (self%passed_values)
        allocate (self%passed_values(size(val)), source=val)
    end select

100 continue
end subroutine


! ------------------------------------------------------------------------------
! MATCHES method

function argument_matches (self, name, is_abbrev) result(res)
    class (argument), intent(in) :: self
    class (str), intent(in) :: name
    logical, intent(in), optional :: is_abbrev
    logical :: res

    integer :: i
    type (str) :: noname, id_str
    logical :: lis_abbrev

    res = .false.

    lis_abbrev = .false.
    if (present(is_abbrev)) lis_abbrev = is_abbrev

    if (lis_abbrev) then
        do i = 1, size(self%abbrevs)
            id_str = self%abbrevs(i)
            if (id_str == name) then
                res = .true.
                return
            end if
        end do
    else
        do i = 1, size(self%names)
            id_str = self%names(i)
            if (id_str == name) then
                res = .true.
                return
            end if
            ! For TOGGLE action, check whether name matches after adding the
            ! OFF prefix
            if (self%action == ARGPARSE_ACTION_TOGGLE) then
                noname = argument_get_toggle_off_name (id_str)
                if (noname == name) then
                    res = .true.
                    return
                end if
            end if
        end do
    end if
end function

! ------------------------------------------------------------------------------
! PARSE dispatch methods

subroutine argument_poly_parse_array (self, val, status)
    class (argument), intent(in) :: self
    class (*), intent(inout), dimension(:), target :: val
    type (status_t), intent(out) :: status

    call status%init (FC_STATUS_OK)

    call self%parse_check_input (val, status)
    if (status /= FC_STATUS_OK) return

    select type (val)
    type is (integer(int32))
        call self%parse (val, status)
    type is (integer(int64))
        call self%parse (val, status)
    type is (real(real32))
        call self%parse (val, status)
    type is (real(real64))
        call self%parse (val, status)
    type is (logical)
        call self%parse (val, status)
    type is (character (*))
        call self%parse (val, status)
    class is (str)
        call self%parse (val, status)
    class default
        status = FC_STATUS_VALUE_ERROR
        status%msg = "Unsupported argument type"
    end select

end subroutine

subroutine argument_poly_parse_scalar (self, val, status)
    class (argument), intent(in) :: self
    class (*), intent(inout), target :: val
    type (status_t), intent(out) :: status

    call status%init (FC_STATUS_OK)

    call self%parse_check_input (val, status)
    if (status /= FC_STATUS_OK) return

    select type (val)
    type is (integer(int32))
        call self%parse (val, status)
    type is (integer(int64))
        call self%parse (val, status)
    type is (real(real32))
        call self%parse (val, status)
    type is (real(real64))
        call self%parse (val, status)
    type is (logical)
        call self%parse (val, status)
    type is (character (*))
        call self%parse (val, status)
    class is (str)
        call self%parse (val, status)
    class default
        status = FC_STATUS_VALUE_ERROR
        status%msg = "Unsupported argument type"
    end select

end subroutine

! ------------------------------------------------------------------------------
! PARSE implementation

subroutine argument_parse_array_int32 (self, val, status)
    integer (int32), intent(inout), dimension(:) :: val
    integer (int32), dimension(:), pointer :: ptr

#include "include/argument_parse_array.f90"
end subroutine

subroutine argument_parse_array_int64 (self, val, status)
    integer, parameter :: INTSIZE = int64
    integer (INTSIZE), intent(inout), dimension(:) :: val
    integer (INTSIZE), dimension(:), pointer :: ptr

#include "include/argument_parse_array.f90"
end subroutine

subroutine argument_parse_array_real32 (self, val, status)
    integer, parameter :: PREC = real32
    real (PREC), intent(inout), dimension(:) :: val
    real (PREC), dimension(:), pointer :: ptr

#include "include/argument_parse_array.f90"
end subroutine

subroutine argument_parse_array_real64 (self, val, status)
    integer, parameter :: PREC = real64
    real (PREC), intent(inout), dimension(:) :: val
    real (PREC), dimension(:), pointer :: ptr

#include "include/argument_parse_array.f90"
end subroutine

subroutine argument_parse_array_logical (self, val, status)
    logical, intent(inout), dimension(:) :: val
    logical, dimension(:), pointer :: ptr

#include "include/argument_parse_array.f90"
end subroutine

! NB: Handle str seperately as we want to be able to convert stored const or
! default values of type character
subroutine argument_parse_array_str (self, val, status)
    class (argument), intent(in), target :: self
    class (str), intent(inout), dimension(:) :: val
    type (status_t), intent(out) :: status

    class (str), dimension(:), pointer :: ptr
    class (*), dimension(:), pointer :: ptr_stored

    integer :: i

    nullify (ptr_stored)
    nullify (ptr)

    call status%init (FC_STATUS_OK)

    if (self%is_present) then
        select case (self%action)
        case (ARGPARSE_ACTION_STORE_CONST)
            ptr_stored => self%const%data_array
        case default

            if (size(val) < self%get_nvals()) then
                status = FC_STATUS_VALUE_ERROR
                status%msg = "Array size insufficient to store value(s)"
                return
            end if

            do i = 1, self%get_nvals()
                call self%passed_values(i)%parse (val(i), status)

                if (status /= FC_STATUS_OK) then
                    status = FC_STATUS_INVALID_STATE
                    status%msg = "Could not convert str to desired type"
                    return
                end if
            end do

            return
        end select
    else if (allocated (self%default%data_array)) then
        ptr_stored => self%default%data_array
    end if

    if (.not. associated(ptr_stored)) then
        status = FC_STATUS_INVALID_STATE
        status%msg = "Argument not present and no default value provided"
        return
    end if

    call dynamic_cast (ptr_stored, ptr, status)
    if (status == FC_STATUS_OK) then
        val = ptr
    else
        select type (ptr_stored)
        type is (character (*))
            val = ptr_stored
            status = FC_STATUS_OK
        class default
            status = FC_STATUS_INVALID_STATE
            status%msg = "Argument type incompatible with stored default value"
        end select
    end if
end subroutine

! NB: Handle str separately as we want to be able to convert stored const or
! default values of type character
subroutine argument_parse_array_char (self, val, status)
    class (argument), intent(in), target :: self
    character (*), intent(inout), dimension(:) :: val
    type (status_t), intent(inout) :: status

    character (len(val)), dimension(:), pointer :: ptr
    class (*), dimension(:), pointer :: ptr_stored

    integer :: i

    nullify (ptr_stored)
    nullify (ptr)

    call status%init (FC_STATUS_OK)

    if (self%is_present) then
        select case (self%action)
        case (ARGPARSE_ACTION_STORE_CONST)
            ptr_stored => self%const%data_array
        case default

            if (size(val) < self%get_nvals()) then
                status = FC_STATUS_VALUE_ERROR
                status%msg = "Array size insufficient to store value(s)"
                return
            end if

            do i = 1, self%get_nvals()
                call self%passed_values(i)%parse (val(i), status)
                if (status /= FC_STATUS_OK) then
                    status = FC_STATUS_INVALID_STATE
                    status%msg = "Could not convert str to desired type"
                    return
                end if
            end do

            return
        end select
    else if (allocated (self%default%data_array)) then
        ptr_stored => self%default%data_array
    end if

    if (.not. associated(ptr_stored)) then
        status = FC_STATUS_INVALID_STATE
        status%msg = "Argument not present and no default value provided"
        return
    end if

    call dynamic_cast (ptr_stored, ptr, status)
    if (status == FC_STATUS_OK) then
        val = ptr
    else
        select type (ptr_stored)
        class is (str)
            val = ptr_stored
            status = FC_STATUS_OK
        class default
            status = FC_STATUS_INVALID_STATE
            status%msg = "Argument type incompatible with stored value"
        end select
    end if
end subroutine

subroutine argument_parse_scalar_int32 (self, val, status)
    integer, parameter :: INTSIZE = int32
    integer (INTSIZE), intent(out) :: val
    integer (INTSIZE), pointer :: ptr

#include "include/argument_parse_scalar.f90"
end subroutine

subroutine argument_parse_scalar_int64 (self, val, status)
    integer, parameter :: INTSIZE = int64
    integer (INTSIZE), intent(out) :: val
    integer (INTSIZE), pointer :: ptr

#include "include/argument_parse_scalar.f90"
end subroutine

subroutine argument_parse_scalar_real32 (self, val, status)
    integer, parameter :: PREC = real32
    real (PREC), intent(out) :: val
    real (PREC), pointer :: ptr

#include "include/argument_parse_scalar.f90"
end subroutine

subroutine argument_parse_scalar_real64 (self, val, status)
    integer, parameter :: PREC = real64
    real (PREC), intent(out) :: val
    real (PREC), pointer :: ptr

#include "include/argument_parse_scalar.f90"
end subroutine

subroutine argument_parse_scalar_logical (self, val, status)
    logical, intent(out) :: val
    logical, pointer :: ptr

#include "include/argument_parse_scalar.f90"
end subroutine

! NB: Handle str seperately as we want to be able to convert stored const or
! default values of type character
subroutine argument_parse_scalar_str (self, val, status)
    class (argument), intent(in), target :: self
    class (str), intent(inout) :: val
    type (status_t), intent(inout) :: status

    class (str), pointer :: ptr
    class (*), pointer :: ptr_stored

    nullify (ptr_stored)
    nullify (ptr)

    call status%init (FC_STATUS_OK)

    if (self%is_present) then
        select case (self%action)
        case (ARGPARSE_ACTION_STORE_CONST)
            ptr_stored => self%const%data_scalar
        case default
            val = self%passed_values(1)
            return
        end select
    else if (allocated (self%default%data_scalar)) then
        ptr_stored => self%default%data_scalar
    end if

    if (.not. associated(ptr_stored)) then
        status = FC_STATUS_INVALID_STATE
        status%msg = "Argument not present and no default value provided"
        return
    end if

    ! at this point we need to retrieve and convert the value stored in either
    ! const or default
    call dynamic_cast (ptr_stored, ptr, status)
    if (status == FC_STATUS_OK) then
        val = ptr
    else
        ! in case stored data is of type character and return value is of type str,
        ! cast will fail and we convert the character data to str
        select type (ptr_stored)
        type is (character (*))
            val = ptr_stored
            status = FC_STATUS_OK
        class default
            status = FC_STATUS_INVALID_STATE
            status%msg = "Argument type incompatible with stored default value"
        end select
    end if

end subroutine

! NB: Handle char return values seperately so we can transparently convert
! const and default values stored as str if necessary.
subroutine argument_parse_scalar_char (self, val, status)
    class (argument), intent(in), target :: self
    character (*), intent(out) :: val
    type (status_t), intent(out) :: status

    class (*), pointer :: ptr_stored
    character (len(val)), pointer :: ptr

    nullify (ptr_stored)
    nullify (ptr)

    call status%init (FC_STATUS_OK)

    if (self%is_present) then
        select case (self%action)
        case (ARGPARSE_ACTION_STORE_CONST)
            ptr_stored => self%const%data_scalar
        case default
            val = self%passed_values(1)
            return
        end select
    else if (allocated (self%default%data_scalar)) then
        ptr_stored => self%default%data_scalar
    end if

    if (.not. associated(ptr_stored)) then
        status = FC_STATUS_INVALID_STATE
        status%msg = "Argument not present and no default value provided"
        return
    end if

    ! at this point we need to retrieve and convert the value stored in either
    ! const or default
    call dynamic_cast (ptr_stored, ptr, status)
    if (status == FC_STATUS_OK) then
        val = ptr
    else
        ! in case stored data is of type character and return value is of type str,
        ! cast will fail and we convert the character data to str
        select type (ptr_stored)
        class is (str)
            val = ptr_stored
            status = FC_STATUS_OK
        class default
            status = FC_STATUS_INVALID_STATE
            status%msg = "Argument type incompatible with stored value"
        end select
    end if

end subroutine

! ------------------------------------------------------------------------------
! PARSE helpers

pure subroutine argument_parse_check_input_scalar (self, val, status)
    class (argument), intent(in) :: self
    class (*), intent(in) :: val
    type (status_t), intent(out) :: status

    call status%init (FC_STATUS_OK)

    if (self%action == ARGPARSE_ACTION_STORE_CONST) then
        if (.not. allocated (self%const%data_scalar)) then
            status = FC_STATUS_VALUE_ERROR
            status%msg = "Array size insufficient to store constant"
            return
        end if
    else if (self%action == ARGPARSE_ACTION_STORE) then
        if (self%get_nvals() > 1) then
            status = FC_STATUS_VALUE_ERROR
            status%msg = "Array size insufficient to store command line arguments"
            return
        end if
    end if
end subroutine


pure subroutine argument_parse_check_input_array (self, val, status)
    class (argument), intent(in) :: self
    class (*), intent(in), dimension(:) :: val
    type (status_t), intent(out) :: status

    integer :: n

    call status%init (FC_STATUS_OK)

    if (self%action == ARGPARSE_ACTION_STORE_CONST) then
        n = argument_data_get_nvals (self%const)
        if (size(val) < n) then
            status = FC_STATUS_VALUE_ERROR
            status%msg = "Array size insufficient to store constant"
            return
        end if
    else if (self%action == ARGPARSE_ACTION_STORE) then
        if (size(val) < self%get_nvals()) then
            status = FC_STATUS_VALUE_ERROR
            status%msg = "Array size insufficient to store command line arguments"
            return
        end if
    end if
end subroutine

! ! ------------------------------------------------------------------------------
! ! GET_IS_PRESENT method
!
! pure function get_is_present (self) result(res)
!     class (argument), intent(in) :: self
!     logical :: res
!
!     res = self%is_present
! end function
!
! ------------------------------------------------------------------------------
! GET_NVALS method

pure function argument_get_nvals (self) result(res)
    class (argument), intent(in) :: self
    integer :: res

    res = 0
    if (self%is_present) then
        select case (self%action)
        case (ARGPARSE_ACTION_STORE_CONST)
            res = argument_data_get_nvals (self%const)
        case default
            if (allocated (self%passed_values)) res = size(self%passed_values)
        end select
    else
        res = argument_data_get_nvals (self%default)
    end if
end function

!-------------------------------------------------------------------------------
! Methods for getting argument name

function argument_get_name (self) result(res)
    !*  ARGUMENT_GET_NAME returns the first (and usualy only) name
    !   associated with the argument object.
    class (argument), intent(in) :: self
    type (str) :: res

    res = self%names(1)
end function


function argument_get_toggle_off_name (name) result(res)
    class (str), intent(in) :: name
    type (str) :: res

    res = ARGUMENT_ACTION_TOGGLE_OFF_PREFIX // '-' // name

end function

! ------------------------------------------------------------------------------
! Operators

subroutine argument_assign (self, rhs)
    !*  ARGUMENT_ASSIGN implements the assignment operator for ARGUMENT
    !   objects.
    class (argument), intent(inout) :: self
    class (argument), intent(in) :: rhs

    call copy_alloc (rhs%names, self%names)
    call copy_alloc (rhs%abbrevs, self%abbrevs)

    self%default = rhs%default
    self%const = rhs%const

    call copy_alloc (rhs%passed_values, self%passed_values)

    self%action = rhs%action
    self%nargs = rhs%nargs
    self%is_present = rhs%is_present
    self%required = rhs%required
    self%help = rhs%help

    self%validator => rhs%validator

end subroutine


! ------------------------------------------------------------------------------
! Casts

subroutine cast_any_to_argument (tgt, ptr, status)
    class (*), intent(in), pointer :: tgt
    class (argument), intent(out), pointer :: ptr
    type (status_t), intent(out), optional :: status

    call status_set_ok (status)
    nullify (ptr)

    select type (tgt)
    class is (argument)
        ptr => tgt
    class default
        call status_set_cast_error (status, "argument")
    end select
end subroutine

end module
