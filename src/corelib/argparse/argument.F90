! gfortran up to v5.x incorrectly processes procedure calls if the actual argument
! is a temporary array of user-derived type, and the dummy argument is
! polymorphic; see https://gcc.gnu.org/bugzilla/show_bug.cgi?id=60322
#if __GFORTRAN__ && (__GNUC__  < 6)
#define _POLYMORPHIC_ARRAY(t) type (t)
#else
#define _POLYMORPHIC_ARRAY(t) class (t)
#endif

module corelib_argparse_argument

    use iso_fortran_env
    use corelib_common
    use corelib_argparse_actions

    implicit none
    private

    public :: fcn_validator
    public :: dynamic_cast

    type, public :: argument
        private
        type (str), public :: name, abbrev
        class (*), dimension(:), allocatable :: default
        class (*), dimension(:), allocatable :: const
        integer (CL_ENUM_KIND), public :: action = ARGPARSE_ACTION_STORE
        logical :: required = .false.
        logical, public :: is_present = .false.
        integer, public :: nargs = 1
        logical :: allow_empty = .false.
            !!  Allow empty strings as argument values. Disabled by default.
        type (str), dimension(:), allocatable :: passed_values
        type (str) :: help
        procedure (fcn_validator), nopass, pointer :: validator => null()
    contains
        procedure, pass :: init_array => argument_init_array
        generic, public :: init => init_array

        procedure, public, pass :: reset => argument_reset

        procedure, pass :: set_scalar => argument_set_scalar
        procedure, pass :: set_array => argument_set_array
        generic, public :: set => set_scalar, set_array

        procedure, pass :: store_const_scalar => argument_store_const_scalar
        procedure, pass :: store_const_array => argument_store_const_array
        generic :: store_const => store_const_scalar, store_const_array

        procedure, pass :: store_default_array => argument_store_default_array
        procedure, pass :: store_default_scalar => argument_store_default_scalar
        generic :: store_default => store_default_array, store_default_scalar

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
        generic :: parse_impl => argument_parse_array_int32, &
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

        procedure, pass :: argument_parse_array
        procedure, pass :: argument_parse_scalar
        generic, public :: parse => argument_parse_scalar, argument_parse_array

        procedure, pass :: argument_parse_check_input_scalar
        procedure, pass :: argument_parse_check_input_array
        generic :: parse_check_input => argument_parse_check_input_scalar, &
            argument_parse_check_input_array

        procedure, pass :: get_nvals => argument_get_nvals

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

subroutine argument_init_array (self, name, abbrev, action, required, nargs, &
        help, status, default, const, validator, allow_empty)

    class (argument), intent(in out) :: self
    class (str), intent(in) :: name
    class (str), intent(in), optional :: abbrev
    integer (CL_ENUM_KIND), intent(in), optional :: action
    logical, intent(in), optional :: required
    integer, intent(in), optional :: nargs
    class (str), intent(in), optional :: help
    class (*), intent(in), dimension(:), optional :: default
    type (status_t), intent(out) :: status
    class (*), intent(in), dimension(:), optional :: const
    procedure (fcn_validator), optional :: validator
    logical, intent(in), optional :: allow_empty

    integer :: laction, lnargs
    logical :: lrequired, supports_arg_values

    ! default values
    call status%init (CL_STATUS_OK)
    laction = ARGPARSE_ACTION_STORE
    lrequired = .false.
    lnargs = 1

    ! overwrite with provided arguments
    if (present(action)) laction = action
    if (present(nargs)) lnargs = nargs
    if (present(required)) lrequired = required

    ! Whether action supports user-provided argument values
    supports_arg_values = (laction == ARGPARSE_ACTION_APPEND .or. &
        laction == ARGPARSE_ACTION_STORE)

    ! Input validation: check before modifying argument object
    if (present(nargs)) then
        ! if action is APPEND we do not allow to specify number of nargs.
        ! In this case nargs must be 1, as argument::set() is assumed to be
        ! called for each instance of argument encountered.
        if (laction == ARGPARSE_ACTION_APPEND) then
            status = CL_STATUS_VALUE_ERROR
            status%msg = "Must not specify 'nargs' with action " &
                // get_action_label (ARGPARSE_ACTION_APPEND)
            return
        else if (present(default)) then
            ! Consistency of nargs and length of default array: assert these are
            ! of same length only if action is not APPEND
            if (size(default) /= nargs) then
                status = CL_STATUS_VALUE_ERROR
                status%msg = "Default array size must be identical to nargs"
                return
            end if
        end if
    end if

    ! require constant to be present and have the right array size
    if (laction == ARGPARSE_ACTION_STORE_CONST) then
        if (.not. present(const)) then
            status = CL_STATUS_VALUE_ERROR
            status%msg = "Action " // get_action_label (ARGPARSE_ACTION_STORE_CONST) &
                // " requires 'const' argument"
            return
        end if
    end if

    ! Validators are only supported for actions that let users actually pass
    ! in user-generated input
    if (.not. supports_arg_values) then
        if (present(validator)) then
            status = CL_STATUS_VALUE_ERROR
            status%msg = "Action " // get_action_label (laction) &
                // " does not support validators"
            return
        end if

        if (present(allow_empty)) then
            status = CL_STATUS_VALUE_ERROR
            status%msg = "Must not specify 'allow_empty' with action " &
                // get_action_label (ARGPARSE_ACTION_APPEND)
        end if
    end if

    select case (laction)
    case (ARGPARSE_ACTION_STORE_TRUE)
        lnargs = 0
        laction = ARGPARSE_ACTION_STORE_CONST
        call self%store_const (.true.)
        call self%store_default (.false.)
    case (ARGPARSE_ACTION_STORE_FALSE)
        lnargs = 0
        laction = ARGPARSE_ACTION_STORE_CONST
        call self%store_const (.false.)
        call self%store_default (.true.)
    case (ARGPARSE_ACTION_STORE_CONST)
        ! no arguments expected when requested to store const
        lnargs = 0
        call self%store_const (const)
        call self%store_default (default)
    case (ARGPARSE_ACTION_APPEND)
        ! this should be guaranteed by input checking above
        lnargs = 1
        call self%store_default (default)
    case default
        call self%store_default (default)
    end select

    self%name = name
    self%action = laction
    self%nargs = lnargs
    self%required = lrequired

    ! Attributes that are independent of anything else
    if (present(abbrev)) self%abbrev = abbrev
    if (present(help)) self%help = help
    if (present(validator)) self%validator => validator
    if (present(allow_empty)) self%allow_empty = allow_empty

end subroutine

! ------------------------------------------------------------------------------
! RESET method

! RESET reverses changed made by set()
pure subroutine argument_reset (self)
    class (argument), intent(in out) :: self

    if (allocated (self%passed_values)) deallocate (self%passed_values)
    self%is_present = .false.
end subroutine

! ------------------------------------------------------------------------------
! Routines to store const and default values

subroutine argument_store_const_array (self, val)
    class (argument), intent(in out) :: self
    class (*), intent(in), dimension(:), optional :: val

    integer :: n

    if (present(val)) then
        n = size(val)
        if (allocated(self%const)) deallocate (self%const)
        allocate (self%const(n), source=val)
    end if
end subroutine

subroutine argument_store_const_scalar (self, val)
    class (argument), intent(in out) :: self
    class (*), intent(in) :: val

    class (*), dimension(:), allocatable :: work

    allocate (work(1), source=val)
    call self%store_const (work)
end subroutine

subroutine argument_store_default_array (self, val)
    class (argument), intent(in out) :: self
    class (*), intent(in), dimension(:), optional :: val

    integer :: n

    if (present(val)) then
        n = size(val)
        if (allocated(self%default)) deallocate (self%default)
        allocate (self%default(n), source=val)
    end if
end subroutine

subroutine argument_store_default_scalar (self, val)
    class (argument), intent(in out) :: self
    class (*), intent(in) :: val

    class (*), dimension(:), allocatable :: work

    allocate (work(1), source=val)
    call self%store_default (work)
end subroutine

! ------------------------------------------------------------------------------
! SET methods

subroutine argument_set_scalar (self, val, status)
    !*  ARGUMENT_SET_SCALAR is a wrapper around ARGUMENT_SET_ARRAY
    !   for scalar command line argument values.

    class (argument), intent(in out) :: self
    type (str), intent(in) :: val
    type (status_t), intent(out), optional :: status

    type (str), dimension(:), allocatable :: work

    allocate (work(1))
    work(1) = val
    call self%set (work, status)
end subroutine

subroutine argument_set_array (self, val, status)
    !*  ARGUMENT_SET_ARRAY stores any command line arguments passed
    !   in as values to this argument instance. Performs input
    !   validation if requested.
    !
    !   Implementation detail: the argument value is store as type(str),
    !   as the eventual data type that the user will request is
    !   not known at this point.

    class (argument), intent(in out) :: self
    type (str), intent(in), dimension(:), optional :: val
    type (status_t), intent(out), optional :: status

    type (status_t) :: lstatus

    lstatus = CL_STATUS_OK

    ! Note: no further code needed to deal with ACTION_STORE_CONST,
    ! will automatically read the default or stored const value when
    ! get() routine is called.

    if (present(val)) then
        if (size(val) > 0) then
            ! Delegate actuall processing of non-zero-length value
            ! array.
            call self%process_cmd_value (val, lstatus)
            if (lstatus /= CL_STATUS_OK) goto 100
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

    class (argument), intent(in out) :: self
    type (str), intent(in), dimension(:) :: val
    type (status_t), intent(out) :: status

    type (str), dimension(:), allocatable :: work
    integer :: n, m, i

    status = CL_STATUS_OK
    n = size(val)

    ! For actions STORE_TRUE, STORE_FALSE and STORE_CONST, no values
    ! are permitted.
    if (self%nargs == 0) then
        status = CL_STATUS_UNSUPPORTED_OP
        status%msg = "Argument does not accept user-provided values"
        goto 100
    end if

    if (associated(self%validator)) then
        do i = 1, n
            call self%validator (val(i), status)
            if (status /= CL_STATUS_OK) goto 100
        end do
    end if

    ! Check for empty strings if these are not permitted
    if (.not. self%allow_empty) then
        do i = 1, n
            if (len(val(i)) == 0) then
                status = CL_STATUS_VALUE_ERROR
                status%msg = "Invalid value; non-empty string not allowed"
                goto 100
            end if
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
! PARSE dispatched methods

subroutine argument_parse_array (self, val, status)
    class (argument), intent(in) :: self
    class (*), intent(out), dimension(:), target :: val
    type (status_t), intent(out) :: status

    call status%init (CL_STATUS_OK)

    call self%parse_check_input (val, status)
    if (status /= CL_STATUS_OK) return

    select type (val)
    type is (integer(int32))
        call self%parse_impl (val, status)
    type is (integer(int64))
        call self%parse_impl (val, status)
    type is (real(real32))
        call self%parse_impl (val, status)
    type is (real(real64))
        call self%parse_impl (val, status)
    type is (logical)
        call self%parse_impl (val, status)
    type is (character (*))
        call self%parse_impl (val, status)
    class is (str)
        call self%parse_impl (val, status)
    class default
        status = CL_STATUS_VALUE_ERROR
        status%msg = "Unsupported argument type"
    end select

end subroutine

subroutine argument_parse_scalar (self, val, status)
    class (argument), intent(in) :: self
    class (*), intent(out), target :: val
    type (status_t), intent(out) :: status

    call status%init (CL_STATUS_OK)

    call self%parse_check_input (val, status)
    if (status /= CL_STATUS_OK) return

    select type (val)
    type is (integer(int32))
        call self%parse_impl (val, status)
    type is (integer(int64))
        call self%parse_impl (val, status)
    type is (real(real32))
        call self%parse_impl (val, status)
    type is (real(real64))
        call self%parse_impl (val, status)
    type is (logical)
        call self%parse_impl (val, status)
    type is (character (*))
        call self%parse_impl (val, status)
    class is (str)
        call self%parse_impl (val, status)
    class default
        status = CL_STATUS_VALUE_ERROR
        status%msg = "Unsupported argument type"
    end select

end subroutine

! ------------------------------------------------------------------------------
! PARSE implementation

subroutine argument_parse_array_int32 (self, val, status)
    integer (int32), intent(out), dimension(:) :: val
    integer (int32), dimension(:), pointer :: ptr

    include "include/argument_parse_array.f90"
end subroutine

subroutine argument_parse_array_int64 (self, val, status)
    integer, parameter :: INTSIZE = int64
    integer (INTSIZE), intent(out), dimension(:) :: val
    integer (INTSIZE), dimension(:), pointer :: ptr

    include "include/argument_parse_array.f90"
end subroutine

subroutine argument_parse_array_real32 (self, val, status)
    integer, parameter :: PREC = real32
    real (PREC), intent(out), dimension(:) :: val
    real (PREC), dimension(:), pointer :: ptr

    include "include/argument_parse_array.f90"
end subroutine

subroutine argument_parse_array_real64 (self, val, status)
    integer, parameter :: PREC = real64
    real (PREC), intent(out), dimension(:) :: val
    real (PREC), dimension(:), pointer :: ptr

    include "include/argument_parse_array.f90"
end subroutine

subroutine argument_parse_array_logical (self, val, status)
    logical, intent(out), dimension(:) :: val
    logical, dimension(:), pointer :: ptr
    include "include/argument_parse_array.f90"
end subroutine

! NB: Handle str seperately as we want to be able to convert stored const or
! default values of type character
subroutine argument_parse_array_str (self, val, status)
    _POLYMORPHIC_ARRAY (str), intent(in out), dimension(:) :: val
    _POLYMORPHIC_ARRAY (str), dimension(:), pointer :: ptr

    class (argument), intent(in), target :: self
    type (status_t), intent(out) :: status
    class (*), dimension(:), pointer :: ptr_stored

    integer :: i

    nullify (ptr_stored, ptr)
    call status%init (CL_STATUS_OK)

    if (self%is_present) then
        select case (self%action)
        case (ARGPARSE_ACTION_STORE_CONST)
            ptr_stored => self%const
        case default

            if (size(val) < self%get_nvals()) then
                status = CL_STATUS_VALUE_ERROR
                status%msg = "Array size insufficient to store value(s)"
                return
            end if

            do i = 1, self%get_nvals()
                call self%passed_values(i)%parse (val(i), status)

                if (status /= CL_STATUS_OK) then
                    status = CL_STATUS_INVALID_STATE
                    status%msg = "Could not convert str to desired type"
                    return
                end if
            end do

            return
        end select
    else if (allocated (self%default)) then
        ptr_stored => self%default
    end if

    if (.not. associated(ptr_stored)) then
        status = CL_STATUS_INVALID_STATE
        status%msg = "Argument not present and no default value provided"
        return
    end if

    call dynamic_cast (ptr_stored, ptr, status)
    if (status == CL_STATUS_OK) then
        val = ptr
    else
        select type (ptr_stored)
        type is (character (*))
            val = ptr_stored
            status = CL_STATUS_OK
        class default
            status = CL_STATUS_INVALID_STATE
            status%msg = "Argument type incompatible with stored default value"
        end select
    end if
end subroutine

! NB: Handle str seperately as we want to be able to convert stored const or
! default values of type character
subroutine argument_parse_array_char (self, val, status)
    character (*), intent(out), dimension(:) :: val
    character (len(val)), dimension(:), pointer :: ptr

    class (argument), intent(in), target :: self
    type (status_t), intent(in out) :: status
    class (*), dimension(:), pointer :: ptr_stored

    integer :: i

    nullify (ptr_stored, ptr)
    call status%init (CL_STATUS_OK)

    if (self%is_present) then
        select case (self%action)
        case (ARGPARSE_ACTION_STORE_CONST)
            ptr_stored => self%const
        case default

            if (size(val) < self%get_nvals()) then
                status = CL_STATUS_VALUE_ERROR
                status%msg = "Array size insufficient to store value(s)"
                return
            end if

            do i = 1, self%get_nvals()
                call self%passed_values(i)%parse (val(i), status)
                if (status /= CL_STATUS_OK) then
                    status = CL_STATUS_INVALID_STATE
                    status%msg = "Could not convert str to desired type"
                    return
                end if
            end do

            return
        end select
    else if (allocated (self%default)) then
        ptr_stored => self%default
    end if

    if (.not. associated(ptr_stored)) then
        status = CL_STATUS_INVALID_STATE
        status%msg = "Argument not present and no default value provided"
        return
    end if

    call dynamic_cast (ptr_stored, ptr, status)
    if (status == CL_STATUS_OK) then
        val = ptr
    else
        select type (ptr_stored)
        class is (str)
            val = ptr_stored
            status = CL_STATUS_OK
        class default
            status = CL_STATUS_INVALID_STATE
            status%msg = "Argument type incompatible with stored value"
        end select
    end if
end subroutine

subroutine argument_parse_scalar_int32 (self, val, status)
    integer (int32), intent(out) :: val
    integer (int32), dimension(:), pointer :: ptr

    include "include/argument_parse_scalar.f90"
end subroutine

subroutine argument_parse_scalar_int64 (self, val, status)
    integer, parameter :: INTSIZE = int64
    integer (INTSIZE), intent(out) :: val
    integer (INTSIZE), dimension(:), pointer :: ptr

    include "include/argument_parse_scalar.f90"
end subroutine

subroutine argument_parse_scalar_real32 (self, val, status)
    integer, parameter :: PREC = real32
    real (PREC), intent(out) :: val
    real (PREC), dimension(:), pointer :: ptr

    include "include/argument_parse_scalar.f90"
end subroutine

subroutine argument_parse_scalar_real64 (self, val, status)
    integer, parameter :: PREC = real64
    real (PREC), intent(out) :: val
    real (PREC), dimension(:), pointer :: ptr

    include "include/argument_parse_scalar.f90"
end subroutine

subroutine argument_parse_scalar_logical (self, val, status)
    logical, intent(out) :: val
    logical, dimension(:), pointer :: ptr
    include "include/argument_parse_scalar.f90"
end subroutine

! NB: Handle str seperately as we want to be able to convert stored const or
! default values of type character
subroutine argument_parse_scalar_str (self, val, status)
    class (str), intent(in out) :: val
    _POLYMORPHIC_ARRAY (str), dimension(:), pointer :: ptr

    class (argument), intent(in), target :: self
    type (status_t), intent(in out) :: status
    class (*), dimension(:), pointer :: ptr_stored

    nullify (ptr_stored)
    call status%init (CL_STATUS_OK)

    if (self%is_present) then
        select case (self%action)
        case (ARGPARSE_ACTION_STORE_CONST)
            ptr_stored => self%const
        case default
            val = self%passed_values(1)
            return
        end select
    else if (allocated (self%default)) then
        ptr_stored => self%default
    end if

    if (.not. associated(ptr_stored)) then
        status = CL_STATUS_INVALID_STATE
        status%msg = "Argument not present and no default value provided"
        return
    end if

    ! at this point we need to retrieve and convert the value stored in either
    ! const or default
    call dynamic_cast (ptr_stored, ptr, status)
    if (status == CL_STATUS_OK) then
        val = ptr(1)
    else
        ! in case stored data is of type character and return value is of type str,
        ! cast will fail and we convert the character data to str
        select type (ptr_stored)
        type is (character (*))
            val = ptr_stored(1)
            status = CL_STATUS_OK
        class default
            status = CL_STATUS_INVALID_STATE
            status%msg = "Argument type incompatible with stored default value"
        end select
    end if

end subroutine

! NB: Handle char return values seperately so we can transparently convert
! const and default values stored as str if necessary.
subroutine argument_parse_scalar_char (self, val, status)
    character (*), intent(out) :: val
    character (len(val)), dimension(:), pointer :: ptr

    class (argument), intent(in), target :: self
    type (status_t), intent(out) :: status
    class (*), dimension(:), pointer :: ptr_stored

    nullify (ptr_stored)
    call status%init (CL_STATUS_OK)

    if (self%is_present) then
        select case (self%action)
        case (ARGPARSE_ACTION_STORE_CONST)
            ptr_stored => self%const
        case default
            val = self%passed_values(1)
            return
        end select
    else if (allocated (self%default)) then
        ptr_stored => self%default
    end if

    if (.not. associated(ptr_stored)) then
        status = CL_STATUS_INVALID_STATE
        status%msg = "Argument not present and no default value provided"
        return
    end if

    ! at this point we need to retrieve and convert the value stored in either
    ! const or default
    call dynamic_cast (ptr_stored, ptr, status)
    if (status == CL_STATUS_OK) then
        val = ptr(1)
    else
        ! in case stored data is of type character and return value is of type str,
        ! cast will fail and we convert the character data to str
        select type (ptr_stored)
        class is (str)
            val = ptr_stored(1)
            status = CL_STATUS_OK
        class default
            status = CL_STATUS_INVALID_STATE
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

    call status%init (CL_STATUS_OK)

    if (self%action == ARGPARSE_ACTION_STORE_CONST) then
        if (size(self%const) > 1) then
            status = CL_STATUS_VALUE_ERROR
            status%msg = "Array size insufficient to store constant"
            return
        end if
    else if (self%action == ARGPARSE_ACTION_STORE) then
        if (self%get_nvals() > 1) then
            status = CL_STATUS_VALUE_ERROR
            status%msg = "Array size insufficient to store command line arguments"
            return
        end if
    end if
end subroutine

pure subroutine argument_parse_check_input_array (self, val, status)
    class (argument), intent(in) :: self
    class (*), intent(in), dimension(:) :: val
    type (status_t), intent(out) :: status

    call status%init (CL_STATUS_OK)

    if (self%action == ARGPARSE_ACTION_STORE_CONST) then
        if (size(val) < size(self%const)) then
            status = CL_STATUS_VALUE_ERROR
            status%msg = "Array size insufficient to store constant"
            return
        end if
    else if (self%action == ARGPARSE_ACTION_STORE) then
        if (size(val) < self%get_nvals()) then
            status = CL_STATUS_VALUE_ERROR
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
            res = size(self%const)
        case default
            if (allocated (self%passed_values)) res = size(self%passed_values)
        end select
    else
        if (allocated (self%default)) res = size(self%default)
    end if
end function

! ------------------------------------------------------------------------------
! Casts

subroutine cast_any_to_argument (tgt, ptr, status)
    class (*), intent(in), pointer :: tgt
    class (argument), intent(out), pointer :: ptr
    type (status_t), intent(out), optional :: status

    select type (tgt)
    class is (argument)
        ptr => tgt
    class default
        if (present(status)) then
            status = CL_STATUS_UNSUPPORTED_OP
            status%msg = "Casting to type(argument) not supported"
        end if
    end select
end subroutine

end module
