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
    use corelib_argparse_constants
    use corelib_strings
    use corelib_common

    implicit none
    private

    type, public :: argument
        private
        type (str), public :: name, abbrev
        class (*), dimension(:), allocatable :: default
        class (*), dimension(:), allocatable :: const
        integer, public :: action = ARGPARSE_ACTION_STORE
        logical :: required = .false.
        logical, public :: is_present = .false.
        integer, public :: nargs = 1
        type (str), dimension(:), allocatable :: passed_values
        type (str) :: help
    contains
        procedure, pass :: init_array => argument_init_array
        procedure, pass :: init_scalar => argument_init_scalar
        procedure, pass :: init_scalar_default => argument_init_scalar_default
        generic, public :: init => init_array, init_scalar, init_scalar_default

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

        procedure, pass :: get_cmd_nargs => argument_get_cmd_nargs

    end type

    interface dynamic_cast
        module procedure cast_any_to_argument
    end interface

    public :: dynamic_cast

contains

! ------------------------------------------------------------------------------
! Initialization

subroutine argument_init_array (self, name, abbrev, action, required, nargs, &
        help, status, default, const)

    class (argument), intent(in out) :: self
    type (str), intent(in) :: name
    type (str), intent(in), optional :: abbrev
    integer, intent(in), optional :: action
    logical, intent(in), optional :: required
    integer, intent(in), optional :: nargs
    class (*), intent(in), dimension(:), optional :: const
    class (*), intent(in), dimension(:), optional :: default
    type (str), intent(in), optional :: help
    integer, intent(out), optional :: status

    integer :: lstatus, laction, lnargs
    logical :: lrequired
    character (100) :: msg

    msg = ""
    ! default values
    lstatus = STATUS_INVALID_INPUT
    laction = ARGPARSE_ACTION_STORE
    lrequired = .false.
    lnargs = 1

    ! overwrite with provided arguments
    if (present(action)) laction = action
    if (present(nargs)) lnargs = nargs
    if (present(required)) lrequired = required

    ! Input validation: check before modifying argument object
    if (present(nargs)) then
        ! if action is APPEND we do not allow to specify number of nargs.
        ! In this case nargs must be 1, as argument::set() is assumed to be
        ! called for each instance of argument encountered.
        if (laction == ARGPARSE_ACTION_APPEND) then
            msg = "Must not specify nargs with ACTION_APPEND"
            goto 100
        else if (present(default)) then
            ! Consistency of nargs and length of default array: assert these are
            ! of same length only if action is not APPEND
            if (size(default) /= nargs) then
                msg = "default array size must be identical to nargs"
                goto 100
            end if
        end if
    end if

    ! require constant to be present and have the right array size
    if (laction == ARGPARSE_ACTION_STORE_CONST) then
        if (.not. present(const)) then
            msg = "Action STORE_CONST required const argument to be specified"
            goto 100
        end if
    end if

    ! preliminary input validation succeeded, store data in attributes
    lstatus = STATUS_OK

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

100 continue
    if (present(status)) status = lstatus
    if (len_trim(msg) > 0) write (ERROR_UNIT, fmt=*) msg

end subroutine

! Scalar initializer for arguments that have at most 1 arg
subroutine argument_init_scalar_default (self, name, abbrev, action, required, nargs, &
        help, status, default)
    class (argument), intent(in out) :: self
    type (str), intent(in) :: name
    type (str), intent(in), optional :: abbrev
    integer, intent(in), optional :: action
    logical, intent(in), optional :: required
    integer, intent(in), optional :: nargs
    type (str), intent(in), optional :: help
    integer, intent(out), optional :: status
    class (*), intent(in) :: default

    class (*), dimension(:), allocatable :: work1
    integer :: lstatus
    character (100) :: msg

    lstatus = STATUS_OK
    msg = ""

    if (present(nargs)) then
        if (nargs /= 1) then
            lstatus = STATUS_INVALID_INPUT
            msg = "Invalid argument: nargs != 1"
            goto 100
        end if
    end if

    allocate (work1(1), source=default)

    ! if action is to store const, then const must be present and
    ! we interpret 'default' argument to be this constant scalar.
    if (present(action)) then
        if (action == ARGPARSE_ACTION_STORE_CONST) then
            call self%init (name, abbrev, action, required, 1, &
                help, status, const=work1)
            return
        end if
    end if

    ! In all other scenarios (no action present, or action != store_const)
    ! interpret 'default' argument as actual default argument
    call self%init (name, abbrev, action, required, 1, &
        help, status, default=work1)

100 continue
    if (present(status)) status = lstatus
    if (len_trim(msg) > 0) write (ERROR_UNIT, fmt=*) msg
end subroutine

! Scalar initializer for arguments that have at most 1 arg
subroutine argument_init_scalar (self, name, abbrev, action, required, nargs, &
        help, status, default, const)
    class (argument), intent(in out) :: self
    type (str), intent(in) :: name
    type (str), intent(in), optional :: abbrev
    integer, intent(in), optional :: action
    logical, intent(in), optional :: required
    integer, intent(in), optional :: nargs
    type (str), intent(in), optional :: help
    integer, intent(out), optional :: status
    class (*), intent(in) :: default
    class (*), intent(in) :: const

    class (*), dimension(:), allocatable :: work1, work2

    integer :: lstatus
    character (100) :: msg

    lstatus = STATUS_OK
    msg = ""

    if (present(nargs)) then
        if (nargs /= 1) then
            lstatus = STATUS_INVALID_INPUT
            msg = "Invalid argument: nargs != 1"
            goto 100
        end if
    end if

    allocate (work1(1), source=default)
    allocate (work2(1), source=const)

    call self%init (name, abbrev, action, required, 1, help, lstatus, work1, work2)

100 continue
    if (present(status)) status = lstatus
    if (len_trim(msg) > 0) write (ERROR_UNIT, fmt=*) msg
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

subroutine argument_set_scalar (self, val)
    class (argument), intent(in out) :: self
    type (str), intent(in) :: val

    type (str), dimension(:), allocatable :: work

    allocate (work(1))
    work(1) = val
    call self%set (work)
end subroutine

subroutine argument_set_array (self, val)
    class (argument), intent(in out) :: self
    type (str), intent(in), dimension(:), optional :: val

    type (str), dimension(:), allocatable :: work
    integer :: n, m

    if (present(val)) then

        n = size(val)

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
    end if

    ! Argument was present in command line invocation
    self%is_present = .true.
end subroutine

! ------------------------------------------------------------------------------
! PARSE dispatched methods

subroutine argument_parse_array (self, val, status, msg)
    class (argument), intent(in) :: self
    class (*), intent(out), dimension(:), target :: val
    integer, intent(out), optional :: status
    character (*), intent(out), optional :: msg

    integer :: lstatus

    lstatus = STATUS_OK

    call self%parse_check_input (val, status, msg)
    if (status /= STATUS_OK) goto 100

    select type (val)
    type is (integer(int32))
        call self%parse_impl (val, lstatus, msg)
    type is (integer(int64))
        call self%parse_impl (val, lstatus, msg)
    type is (real(real32))
        call self%parse_impl (val, lstatus, msg)
    type is (real(real64))
        call self%parse_impl (val, lstatus, msg)
    type is (logical)
        call self%parse_impl (val, lstatus, msg)
    type is (character (*))
        call self%parse_impl (val, lstatus, msg)
    class is (str)
        call self%parse_impl (val, lstatus, msg)
    class default
        lstatus = STATUS_INVALID_INPUT
        if (present(msg)) msg = "Unsupported argument type"
    end select

100 continue
    if (present(status)) status = lstatus

end subroutine

subroutine argument_parse_scalar (self, val, status, msg)
    class (argument), intent(in) :: self
    class (*), intent(out), target :: val
    integer, intent(out), optional :: status
    character (*), intent(out), optional :: msg

    integer :: lstatus

    lstatus = STATUS_OK

    call self%parse_check_input (val, status, msg)
    if (status /= STATUS_OK) goto 100

    select type (val)
    type is (integer(int32))
        call self%parse_impl (val, lstatus, msg)
    type is (integer(int64))
        call self%parse_impl (val, lstatus, msg)
    type is (real(real32))
        call self%parse_impl (val, lstatus, msg)
    type is (real(real64))
        call self%parse_impl (val, lstatus, msg)
    type is (logical)
        call self%parse_impl (val, lstatus, msg)
    type is (character (*))
        call self%parse_impl (val, lstatus, msg)
    class is (str)
        call self%parse_impl (val, lstatus, msg)
    class default
        lstatus = STATUS_INVALID_INPUT
        if (present(msg)) msg = "Unsupported argument type"
    end select

100 continue
    if (present(status)) status = lstatus

end subroutine

! ------------------------------------------------------------------------------
! PARSE implementation

subroutine argument_parse_array_int32 (self, val, status, msg)
    integer (int32), intent(out), dimension(:) :: val
    integer (int32), dimension(:), pointer :: ptr

    include "include/argument_parse_array.f90"
end subroutine

subroutine argument_parse_array_int64 (self, val, status, msg)
    integer, parameter :: INTSIZE = int64
    integer (INTSIZE), intent(out), dimension(:) :: val
    integer (INTSIZE), dimension(:), pointer :: ptr

    include "include/argument_parse_array.f90"
end subroutine

subroutine argument_parse_array_real32 (self, val, status, msg)
    integer, parameter :: PREC = real32
    real (PREC), intent(out), dimension(:) :: val
    real (PREC), dimension(:), pointer :: ptr

    include "include/argument_parse_array.f90"
end subroutine

subroutine argument_parse_array_real64 (self, val, status, msg)
    integer, parameter :: PREC = real64
    real (PREC), intent(out), dimension(:) :: val
    real (PREC), dimension(:), pointer :: ptr

    include "include/argument_parse_array.f90"
end subroutine

subroutine argument_parse_array_logical (self, val, status, msg)
    logical, intent(out), dimension(:) :: val
    logical, dimension(:), pointer :: ptr
    include "include/argument_parse_array.f90"
end subroutine

! NB: Handle str seperately as we want to be able to convert stored const or
! default values of type character
subroutine argument_parse_array_str (self, val, status, msg)
    _POLYMORPHIC_ARRAY (str), intent(in out), dimension(:) :: val
    _POLYMORPHIC_ARRAY (str), dimension(:), pointer :: ptr

    class (argument), intent(in), target :: self
    integer, intent(out) :: status
    character (*) , intent(out), optional :: msg
    class (*), dimension(:), pointer :: ptr_stored

    integer :: i

    nullify (ptr_stored, ptr)

    if (self%is_present) then
        select case (self%action)
        case (ARGPARSE_ACTION_STORE_CONST)
            ptr_stored => self%const
        case default
            do i = 1, self%get_cmd_nargs()
                call self%passed_values(i)%parse (val(i), status)
            end do
            return
        end select
    else if (allocated (self%default)) then
        ptr_stored => self%default
    end if

    if (associated (ptr_stored)) then
        call dynamic_cast (ptr_stored, ptr, status)
        if (status == STATUS_OK) then
            val = ptr
        else
            select type (ptr_stored)
            type is (character (*))
                val = ptr_stored
            class default
                if (present(msg)) &
                    msg = "Argument type incompatible with stored default value"
            end select
        end if
    else
        status = STATUS_INVALID_STATE
        if (present(msg)) msg = "Argument not present and no default value provided"
    end if

end subroutine

! NB: Handle str seperately as we want to be able to convert stored const or
! default values of type character
subroutine argument_parse_array_char (self, val, status, msg)
    character (*), intent(out), dimension(:) :: val
    character (len(val)), dimension(:), pointer :: ptr

    class (argument), intent(in), target :: self
    integer, intent(out) :: status
    character (*) , intent(out), optional :: msg
    class (*), dimension(:), pointer :: ptr_stored

    integer :: i

    nullify (ptr_stored, ptr)

    if (self%is_present) then
        select case (self%action)
        case (ARGPARSE_ACTION_STORE_CONST)
            ptr_stored => self%const
        case default
            do i = 1, self%get_cmd_nargs()
                call self%passed_values(i)%parse (val(i), status)
            end do
            return
        end select
    else if (allocated (self%default)) then
        ptr_stored => self%default
    end if

    if (associated(ptr_stored)) then
        call dynamic_cast (ptr_stored, ptr, status)
        if (status == STATUS_OK) then
            val = ptr
        else
            select type (ptr_stored)
            class is (str)
                val = ptr_stored
            class default
                if (present(msg)) &
                    msg = "Argument type incompatible with stored value"
            end select
        end if
    else
        status = STATUS_INVALID_STATE
        if (present(msg)) msg = "Argument not present and no default value provided"
    end if

end subroutine

subroutine argument_parse_scalar_int32 (self, val, status, msg)
    integer (int32), intent(out) :: val
    integer (int32), pointer :: ptr

    include "include/argument_parse_scalar.f90"
end subroutine

subroutine argument_parse_scalar_int64 (self, val, status, msg)
    integer, parameter :: INTSIZE = int64
    integer (INTSIZE), intent(out) :: val
    integer (INTSIZE), pointer :: ptr

    include "include/argument_parse_scalar.f90"
end subroutine

subroutine argument_parse_scalar_real32 (self, val, status, msg)
    integer, parameter :: PREC = real32
    real (PREC), intent(out) :: val
    real (PREC), pointer :: ptr

    include "include/argument_parse_scalar.f90"
end subroutine

subroutine argument_parse_scalar_real64 (self, val, status, msg)
    integer, parameter :: PREC = real64
    real (PREC), intent(out) :: val
    real (PREC), pointer :: ptr

    include "include/argument_parse_scalar.f90"
end subroutine

subroutine argument_parse_scalar_logical (self, val, status, msg)
    logical, intent(out) :: val
    logical, pointer :: ptr
    include "include/argument_parse_scalar.f90"
end subroutine

! NB: Handle str seperately as we want to be able to convert stored const or
! default values of type character
subroutine argument_parse_scalar_str (self, val, status, msg)
    class (str), intent(in out) :: val
    class (str), pointer :: ptr

    class (argument), intent(in), target :: self
    integer, intent(out) :: status
    character (*), intent(out), optional :: msg
    class (*), pointer :: ptr_stored

    if (self%is_present) then
        select case (self%action)
        case (ARGPARSE_ACTION_STORE_CONST)
            ptr_stored => self%const(1)
        case default
            val = self%passed_values(1)
            return
        end select
    else if (allocated (self%default)) then
        ptr_stored => self%default(1)
    end if

    ! at this point we need to retrieve and convert the value stored in either
    ! const or default
    call dynamic_cast (ptr_stored, ptr, status)
    if (status == STATUS_OK) then
        val = ptr
    else
        ! in case stored data is of type character and return value is of type str,
        ! cast will fail and we convert the character data to str
        select type (ptr_stored)
        type is (character (*))
            val = ptr_stored
        class default
            if (present(msg)) &
                msg = "Argument type incompatible with stored default value"
        end select
    end if

end subroutine

! NB: Handle char return values seperately so we can transparently convert
! const and default values stored as str if necessary.
subroutine argument_parse_scalar_char (self, val, status, msg)
    character (*), intent(out) :: val
    character (len(val)), pointer :: ptr

    class (argument), intent(in), target :: self
    integer, intent(out) :: status
    character (*), intent(out), optional :: msg
    class (*), pointer :: ptr_stored

    if (self%is_present) then
        select case (self%action)
        case (ARGPARSE_ACTION_STORE_CONST)
            ptr_stored => self%const(1)
        case default
            val = self%passed_values(1)
            return
        end select
    else if (allocated (self%default)) then
        ptr_stored => self%default(1)
    end if

    ! at this point we need to retrieve and convert the value stored in either
    ! const or default
    call dynamic_cast (ptr_stored, ptr, status)
    if (status == STATUS_OK) then
        val = ptr
    else
        ! in case stored data is of type character and return value is of type str,
        ! cast will fail and we convert the character data to str
        select type (ptr_stored)
        class is (str)
            val = ptr_stored
        class default
            if (present(msg)) &
                msg = "Argument type incompatible with stored value"
        end select
    end if

end subroutine

! ------------------------------------------------------------------------------
! PARSE helpers

pure subroutine argument_parse_check_input_scalar (self, val, status, msg)
    class (argument), intent(in) :: self
    class (*), intent(in) :: val
    integer, intent(out) :: status
    character (*), intent(out), optional :: msg

    status = STATUS_OK

    if (self%action == ARGPARSE_ACTION_STORE_CONST) then
        if (size(self%const) > 1) then
            status = STATUS_INVALID_INPUT
            if (present(msg)) msg = "Array size insufficient to store constant"
            return
        end if
    else if (self%action == ARGPARSE_ACTION_STORE) then
        if (self%get_cmd_nargs() > 1) then
            status = STATUS_INVALID_INPUT
            if (present(msg)) &
                msg = "Array size insufficient to store command line arguments"
            return
        end if
    end if
end subroutine

pure subroutine argument_parse_check_input_array (self, val, status, msg)
    class (argument), intent(in) :: self
    class (*), intent(in), dimension(:) :: val
    integer, intent(out) :: status
    character (*), intent(out), optional :: msg

    status = STATUS_OK

    if (self%action == ARGPARSE_ACTION_STORE_CONST) then
        if (size(val) < size(self%const)) then
            status = STATUS_INVALID_INPUT
            if (present(msg)) msg = "Array size insufficient to store constant"
            return
        end if
    else if (self%action == ARGPARSE_ACTION_STORE) then
        if (size(val) < self%get_cmd_nargs()) then
            status = STATUS_INVALID_INPUT
            if (present(msg)) &
                msg = "Array size insufficient to store command line arguments"
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
! GET_CMD_NARGS method

pure function argument_get_cmd_nargs (self) result(res)
    class (argument), intent(in) :: self
    integer :: res

    res = 0
    if (allocated (self%passed_values)) res = size(self%passed_values)
end function

! ------------------------------------------------------------------------------
! Casts

subroutine cast_any_to_argument (tgt, ptr)
    class (*), intent(in), pointer :: tgt
    class (argument), intent(out), pointer :: ptr

    select type (tgt)
    class is (argument)
        ptr => tgt
    class default
        error stop "Unsupported cast"
    end select
end subroutine

end module
