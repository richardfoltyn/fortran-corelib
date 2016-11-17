! gfortran up to v5.x incorrectly processes procedure calls if the actual argument
! is a temporary array of user-derived type, and the dummy argument is
! polymorphic; see https://gcc.gnu.org/bugzilla/show_bug.cgi?id=60322
#ifdef __GFORTRAN__
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
        integer :: action = ARGPARSE_ACTION_STORE
        logical :: required = .false.
        logical :: is_present = .false.
        integer, public :: nargs = 1
        type (str), dimension(:), allocatable :: passed_values
        type (str) :: help
    contains
        procedure, pass :: init_array => argument_init_array
        procedure, pass :: init_scalar => argument_init_scalar
        procedure, pass :: init_scalar_default => argument_init_scalar_default
        generic, public :: init => init_array, init_scalar, init_scalar_default

        procedure, pass :: set_scalar => argument_set_scalar
        procedure, pass :: set_array => argument_set_array
        generic, public :: set => set_scalar, set_array

        procedure, pass :: store_const_scalar => argument_store_const_scalar
        procedure, pass :: store_const_array => argument_store_const_array
        generic :: store_const => store_const_scalar, store_const_array

        procedure, pass :: store_default_array => argument_store_default_array
        procedure, pass :: store_default_scalar => argument_store_default_scalar
        generic :: store_default => store_default_array, store_default_scalar

        procedure, pass :: get_array => argument_get_array
        procedure, pass :: get_scalar => argument_get_scalar
        generic, public :: get => get_array, get_scalar

        procedure, pass :: argument_parse_array_int32
        procedure, pass :: argument_parse_array_int64
        procedure, pass :: argument_parse_array_real32
        procedure, pass :: argument_parse_array_real64
        procedure, pass :: argument_parse_array_logical
        procedure, pass :: argument_parse_array_str
        procedure, pass :: argument_parse_scalar_int32
        procedure, pass :: argument_parse_scalar_int64
        procedure, pass :: argument_parse_scalar_real32
        procedure, pass :: argument_parse_scalar_real64
        procedure, pass :: argument_parse_scalar_logical
        procedure, pass :: argument_parse_scalar_str
        generic, public :: parse => argument_parse_array_int32, &
            argument_parse_array_int64, &
            argument_parse_array_real32, &
            argument_parse_array_real64, &
            argument_parse_array_logical, &
            argument_parse_array_str, &
            argument_parse_scalar_int32, &
            argument_parse_scalar_int64, &
            argument_parse_scalar_real32, &
            argument_parse_scalar_real64, &
            argument_parse_scalar_logical, &
            argument_parse_scalar_str

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
    integer, intent(in) :: nargs
    class (*), intent(in), dimension(:), optional :: const
    class (*), intent(in), dimension(:), optional :: default
    type (str), intent(in), optional :: help
    integer, intent(out), optional :: status

    integer :: lstatus, laction, lnargs
    logical :: lrequired

    ! default values
    lstatus = STATUS_INVALID_INPUT
    laction = ARGPARSE_ACTION_STORE
    lrequired = .false.

    ! overwrite with provided arguments
    if (present(action)) laction = action
    lnargs = nargs
    if (present(required)) lrequired = required

    ! Input validation: check before modifying argument object
    if (present(default)) then
        if (size(default) /= nargs) goto 100
    end if

    ! if not required and action needs an argument, check that default value
    ! is specified
    if (.not. lrequired .and. .not. present(default)) goto 100

    ! require constant to be present and have the right array size
    if (laction == ARGPARSE_ACTION_STORE_CONST) then
        if (.not. present(const)) goto 100
        if (size(const) /= nargs) goto 100
    end if

    ! preliminary input validation succeeded, store data in attributes
    lstatus = ARGPARSE_STATUS_OK

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
    case default
        if (present(default)) then
            call self%store_default (default)
            ! nargs was stored in lnargs above
        else
            ! assume one argument by default, no default value given
            lnargs = 1
        end if
    end select

    self%name = name
    self%action = laction
    self%nargs = lnargs
    self%required = lrequired

    ! Attributes that are independent of anything else
    if (present(abbrev)) self%abbrev = abbrev
    if (present(help)) self%help = help

    return

100 continue
    if (present(status)) status = lstatus

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
            call self%init (name, abbrev, action, required, 1, const=work1, &
                help=help, status=status)
            return
        end if
    end if

    ! In all other scenarios (no action present, or action != store_const)
    ! interpret 'default' argument as actual default argument
    call self%init (name, abbrev, action, required, 1, default=work1, &
        help=help, status=status)

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

    if (present(nargs)) then
        if (nargs /= 1) then
            lstatus = STATUS_INVALID_INPUT
            msg = "Invalid argument: nargs != 1"
            goto 100
        end if
    end if

    allocate (work1(1), source=const)
    allocate (work2(1), source=default)

    call self%init (name, abbrev, action, required, 1, help, lstatus, work1, work2)

100 continue
    if (present(status)) status = lstatus
    if (len_trim(msg) > 0) write (ERROR_UNIT, fmt=*) msg
end subroutine

! ------------------------------------------------------------------------------
! Routines to store const and default values

subroutine argument_store_const_array (self, val)
    class (argument), intent(in out) :: self
    class (*), intent(in), dimension(:) :: val

    integer :: n

    n = size(val)
    if (allocated(self%const)) deallocate (self%const)
    allocate (self%const(n), source=val)
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
    class (*), intent(in), dimension(:) :: val

    integer :: n

    n = size(val)
    if (allocated(self%default)) deallocate (self%default)
    allocate (self%default(n), source=val)
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

    if (present(val)) then
        if (allocated(self%passed_values)) deallocate (self%passed_values)
        allocate (self%passed_values(size(val)), source=val)
    end if

    ! Argument was present in command line invocation
    self%is_present = .true.
end subroutine


! ------------------------------------------------------------------------------
! GET methods

subroutine argument_get_array (self, val, status)
    class (argument), intent(in) :: self
    class (*), intent(out), dimension(:), target :: val
    integer, intent(out), optional :: status

    integer :: lstatus
    character (100) :: msg

    ! pointers to supported data types
    integer (int32), dimension(:), pointer :: ptr_int32
    integer (int64), dimension(:), pointer :: ptr_int64
    real (real32), dimension(:), pointer :: ptr_real32
    real (real64), dimension(:), pointer :: ptr_real64
    logical, dimension(:), pointer :: ptr_logical
    _POLYMORPHIC_ARRAY (str), dimension(:), pointer :: ptr_str

    lstatus = STATUS_OK

    if (self%action == ARGPARSE_ACTION_STORE_CONST) then
        if (size(val) < size(self%const)) then
            lstatus = STATUS_INVALID_INPUT
            msg = "Array size insufficient to store constant"
            goto 100
        end if
    else if (self%action == ARGPARSE_ACTION_STORE) then
        if (size(val) < self%nargs) then
            lstatus = STATUS_INVALID_INPUT
            msg = "Array size insufficient to store command line arguments"
            goto 100
        end if
    end if

    select type (val)
    type is (integer(int32))
        ptr_int32 => val
        call self%parse (ptr_int32, lstatus)
    type is (integer(int64))
        ptr_int64 => val
        call self%parse (ptr_int64, lstatus)
    type is (real(real32))
        ptr_real32 => val
        call self%parse (ptr_real32, lstatus)
    type is (real(real64))
        ptr_real64 => val
        call self%parse (ptr_real64, lstatus)
    type is (logical)
        ptr_logical => val
        call self%parse (ptr_logical, lstatus)
    class is (str)
        ptr_str => val
        call self%parse (ptr_str, lstatus)
    class default
        lstatus = STATUS_INVALID_INPUT
        msg = "Unsupported argument type"
    end select

100 continue
    if (present(status)) status = lstatus
    if (len(msg) > 0) write(ERROR_UNIT, fmt=*) msg

end subroutine

subroutine argument_get_scalar (self, val, status)
    class (argument), intent(in) :: self
    class (*), intent(out), target :: val
    integer, intent(out), optional :: status

    integer :: lstatus
    character (100) :: msg

    ! pointers to supported data types
    integer (int32), pointer :: ptr_int32
    integer (int64), pointer :: ptr_int64
    real (real32), pointer :: ptr_real32
    real (real64), pointer :: ptr_real64
    logical, pointer :: ptr_logical
    class (str), pointer :: ptr_str

    lstatus = STATUS_OK

    if (self%action == ARGPARSE_ACTION_STORE_CONST) then
        if (size(self%const) > 1) then
            lstatus = STATUS_INVALID_INPUT
            msg = "Array size insufficient to store constant"
            goto 100
        end if
    else if (self%action == ARGPARSE_ACTION_STORE) then
        if (self%nargs > 1) then
            lstatus = STATUS_INVALID_INPUT
            msg = "Array size insufficient to store command line arguments"
            goto 100
        end if
    end if

    select type (val)
    type is (integer(int32))
        ptr_int32 => val
        call self%parse (ptr_int32, lstatus)
    type is (integer(int64))
        ptr_int64 => val
        call self%parse (ptr_int64, lstatus)
    type is (real(real32))
        ptr_real32 => val
        call self%parse (ptr_real32, lstatus)
    type is (real(real64))
        ptr_real64 => val
        call self%parse (ptr_real64, lstatus)
    type is (logical)
        ptr_logical => val
        call self%parse (ptr_logical, lstatus)
    class is (str)
        ptr_str => val
        call self%parse (ptr_str, lstatus)
    class default
        lstatus = STATUS_INVALID_INPUT
        msg = "Unsupported argument type"
    end select

100 continue
    if (present(status)) status = lstatus
    if (len(msg) > 0) write(ERROR_UNIT, fmt=*) msg

end subroutine


! ------------------------------------------------------------------------------
! PARSE method

subroutine argument_parse_array_int32 (self, val, status)
    integer (int32), intent(out), dimension(:) :: val
    integer (int32), dimension(:), pointer :: ptr

    include "include/argument_parse_array_impl.f90"
end subroutine

subroutine argument_parse_array_int64 (self, val, status)
    integer, parameter :: INTSIZE = int64
    integer (INTSIZE), intent(out), dimension(:) :: val
    integer (INTSIZE), dimension(:), pointer :: ptr

    include "include/argument_parse_array_impl.f90"
end subroutine

subroutine argument_parse_array_real32 (self, val, status)
    integer, parameter :: PREC = real32
    real (PREC), intent(out), dimension(:) :: val
    real (PREC), dimension(:), pointer :: ptr

    include "include/argument_parse_array_impl.f90"
end subroutine

subroutine argument_parse_array_real64 (self, val, status)
    integer, parameter :: PREC = real64
    real (PREC), intent(out), dimension(:) :: val
    real (PREC), dimension(:), pointer :: ptr

    include "include/argument_parse_array_impl.f90"
end subroutine

subroutine argument_parse_array_logical (self, val, status)
    logical, intent(out), dimension(:) :: val
    logical, dimension(:), pointer :: ptr
    include "include/argument_parse_array_impl.f90"
end subroutine

subroutine argument_parse_array_str (self, val, status)
    _POLYMORPHIC_ARRAY (str), intent(out), dimension(:) :: val
    _POLYMORPHIC_ARRAY (str), dimension(:), pointer :: ptr
    include "include/argument_parse_array_impl.f90"
end subroutine

subroutine argument_parse_scalar_int32 (self, val, status)
    integer (int32), intent(out) :: val
    integer (int32), pointer :: ptr

    include "include/argument_parse_scalar.f90"
end subroutine

subroutine argument_parse_scalar_int64 (self, val, status)
    integer, parameter :: INTSIZE = int64
    integer (INTSIZE), intent(out) :: val
    integer (INTSIZE), pointer :: ptr

    include "include/argument_parse_scalar.f90"
end subroutine

subroutine argument_parse_scalar_real32 (self, val, status)
    integer, parameter :: PREC = real32
    real (PREC), intent(out) :: val
    real (PREC), pointer :: ptr

    include "include/argument_parse_scalar.f90"
end subroutine

subroutine argument_parse_scalar_real64 (self, val, status)
    integer, parameter :: PREC = real64
    real (PREC), intent(out) :: val
    real (PREC), pointer :: ptr

    include "include/argument_parse_scalar.f90"
end subroutine

subroutine argument_parse_scalar_logical (self, val, status)
    logical, intent(out) :: val
    logical, pointer :: ptr
    include "include/argument_parse_scalar.f90"
end subroutine

subroutine argument_parse_scalar_str (self, val, status)
    class (str), intent(out) :: val
    class (str), pointer :: ptr
    include "include/argument_parse_scalar.f90"
end subroutine

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
