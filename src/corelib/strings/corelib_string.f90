! define implementation-indep. function to determine whether string
! has been initialized with some value
#define _VALID(obj) (allocated(obj%value))
#define _CLEAR(obj) if (allocated(obj%value)) deallocate(obj%value)

module corelib_string

    use iso_fortran_env

    implicit none
    private

    character (*), parameter :: EMPTY_VALUE = ""

    type :: str
        private
        character (len=:), allocatable :: value
    contains

        procedure, pass :: reset
        procedure, public, pass :: to_char

        ! inquiry functions
        procedure, pass :: startswith_str
        procedure, pass :: startswith_char
        generic, public :: startswith => startswith_str, startswith_char

        procedure, pass :: endswith_str
        procedure, pass :: endswith_char
        generic, public :: endswith => endswith_str, endswith_char

        procedure, pass :: substring_to_int64
        procedure, pass :: substring_to_int32
        procedure, pass :: substring_from_to_int64
        procedure, pass :: substring_from_to_int32
        generic, public :: substring => substring_to_int64, &
            substring_from_to_int64, substring_to_int32, substring_from_to_int32

        ! string generators
        procedure, pass :: join_str
        procedure, pass :: join_char
        generic, public :: join => join_str, join_char

        ! Finalizers
        ! final :: finalize
    end type

    interface str
        module procedure ctor_char, ctor_real64, ctor_real, ctor_int64, &
            ctor_int32
    end interface

    interface str_array
        module procedure str_array_1d_int32, str_array_1d_int64, &
            str_array_1d_real32, str_array_1d_real64
    end interface

    ! allow assignment from str to character
    interface assignment (=)
        module procedure assign_str_char, assign_char_str, assign_str_str
    end interface

    ! concatenation // with character as lhs operand
    interface operator (//)
        module procedure concat_str_str, concat_char_str, concat_str_char
    end interface

    ! addition with character as lhs operand
    interface operator (+)
        module procedure concat_str_str, concat_char_str, concat_str_char
    end interface

    ! multiply by integers to minic what repeat() does
    interface operator (*)
        module procedure repeat_str_int64, repeat_str_int32, repeat_int32_str, &
            repeat_int64_str
    end interface

    ! equality
    interface operator (==)
        module procedure eq_str_str, eq_str_char, eq_char_str
    end interface

    interface operator (/=)
        module procedure neq_str_str, neq_str_char, neq_char_str
    end interface

    ! overload instrinsic len()
    interface len
        module procedure len_str
    end interface

    ! overload instrinsic repeat() to operator on str types
    interface repeat
        module procedure repeat_str_int64, repeat_str_int32, repeat_int32_str, &
            repeat_int64_str
    end interface

    public :: str, str_array, len, repeat
    public :: operator (+), operator (//), operator (/=), operator (==), operator (*)
    public :: assignment (=)
contains

! *****************************************************************************
! Initialization

elemental function ctor_char (ch) result(res)
    character (len=*), intent(in) :: ch
    type (str) :: res

    allocate (res%value, source=ch)

end function

elemental function ctor_real64 (from_real, fmt) result(res)
    real (real64), intent(in) :: from_real
    character (len=*), intent(in), optional :: fmt
    type (str) :: res

    integer, parameter :: LEN_BUFFER = 100
    character (len=LEN_BUFFER) :: buf

    if (present(fmt)) then
        write (unit=buf, fmt=pad_format(fmt)) from_real
    else
        write (buf, *) from_real
    end if

    allocate (res%value, source=trim(buf))
end function

elemental function ctor_real (from_real, fmt) result(res)
    real, intent(in) :: from_real
    character (len=*), intent(in), optional :: fmt
    type (str) :: res

    integer, parameter :: LEN_BUFFER = 100
    character (len=LEN_BUFFER) :: buf

    if (present(fmt)) then
        write (unit=buf, fmt=pad_format(fmt)) from_real
    else
        write (buf, *) from_real
    end if

    allocate (res%value, source=trim(buf))
end function

elemental function ctor_int64 (from_int, fmt) result(res)
    integer (int64), intent(in) :: from_int
    character (len=*), intent(in), optional :: fmt
    type (str) :: res

    integer, parameter :: LEN_BUFFER = 100
    character (len=:), allocatable :: buf

    allocate (character (len=LEN_BUFFER) :: buf)

    if (present(fmt)) then
        write (unit=buf, fmt=pad_format(fmt)) from_int
    else
        write (unit=buf, fmt="(i0)") from_int
    end if

    allocate (res%value, source=trim(buf))
end function

elemental function ctor_int32 (from_int, fmt) result(res)
    integer, parameter :: INTSIZE = int32

    integer (INTSIZE), intent(in) :: from_int
    character (len=*), intent(in), optional :: fmt
    type (str) :: res

    integer, parameter :: LEN_BUFFER = 100
    character (len=:), allocatable :: buf

    allocate (character (len=LEN_BUFFER) :: buf)

    if (present(fmt)) then
        write (unit=buf, fmt=pad_format(fmt)) from_int
    else
        write (unit=buf, fmt="(i0)") from_int
    end if

    allocate (res%value, source=trim(buf))
end function

pure function pad_format (fmt) result(res)
    character (len=*), intent(in) :: fmt
    character (len=len(fmt) + 2) :: res

    integer :: n

    res = adjustl (fmt)

    if (res(1:1) /= "(") then
        res = "(" // res
    end if

    n = len_trim (res)
    if (res(n:n) /= ")") then
        res = trim(res) // ")"
    end if
end function

! *****************************************************************************
! String representations of arrays
pure function str_array_1d_int32 (x, fmt) result(res)
    integer (int32), intent(in), dimension(:) :: x
    include "include/str_array_1d.f90"
end function

pure function str_array_1d_int64 (x, fmt) result(res)
    integer (int64), intent(in), dimension(:) :: x
    include "include/str_array_1d.f90"
end function

pure function str_array_1d_real32 (x, fmt) result(res)
    real (real32), intent(in), dimension(:) :: x
    include "include/str_array_1d.f90"
end function

pure function str_array_1d_real64 (x, fmt) result(res)
    real (real64), intent(in), dimension(:) :: x
    include "include/str_array_1d.f90"
end function

! *****************************************************************************
! Public attributes

elemental function len_str (self) result(res)
    class (str), intent(in) :: self
    integer :: res

    res = 0
    if (_VALID(self)) res = len(self%value)
end function

! *****************************************************************************
! Conversion routines

pure function to_char(self) result(res)
    class (str), intent(in) :: self
    character (len=len(self)) :: res

    if (_VALID(self)) then
        res = self%value
    else
        res = EMPTY_VALUE
    end if

end function

! *****************************************************************************
! Formatting


! *****************************************************************************
! STARTSWITH method

elemental function startswith_char(self, prefix) result(res)
    class (str), intent(in) :: self
    character (len=*), intent(in) :: prefix
    logical :: res

    res = .false.

    if (_VALID(self) .and. len(self) >= len(prefix)) then
        res = self%value(1:len(prefix)) == prefix
    end if

end function

elemental function startswith_str(self, prefix) result(res)
    class (str), intent(in) :: self, prefix
    logical :: res

    res = .false.
    if (_VALID(self) .and. _VALID(prefix)) then
        res = startswith_char(self, prefix%value)
    end if

end function

! *****************************************************************************
! ENDSWITH method

elemental function endswith_char (self, suffix) result(res)
    class (str), intent(in) :: self
    character (len=*), intent(in) :: suffix
    logical :: res

    integer :: ifrom, ito

    res = .false.

    if (_VALID(self) .and. (len(self) >= len(suffix))) then
        ifrom = len(self)-len(suffix) + 1
        ito = len(self)
        res = self%value(ifrom:ito) == suffix
    end if
end function

elemental function endswith_str(self, suffix) result(res)
    class (str), intent(in) :: self, suffix
    logical :: res

    res = .false.
    if (_VALID(self) .and. _VALID(suffix)) then
        res = endswith_char(self, suffix%value)
    end if

end function

! *****************************************************************************
! ASSIGNMENT operator
elemental subroutine assign_str_str(lhs, rhs)
    type (str), intent(out) :: lhs
    class (str), intent(in) :: rhs

    integer :: n

    ! lhs%value should be automatically deallocated due to intent(out),
    ! but make sure this is the case
    call lhs%reset ()

    if (_VALID(rhs)) then
        n = len(rhs)
        allocate (character (len=n) :: lhs%value)
        lhs%value = rhs%value
    end if
end subroutine

elemental subroutine assign_str_char(lhs, rhs)
    type (str), intent(out) :: lhs
    character (len=*), intent(in) :: rhs

    ! lhs%value should be automatically deallocated due to intent(out)
    allocate (lhs%value, source=rhs)
end subroutine

elemental subroutine assign_char_str (lhs, rhs)
    character (len=*), intent(in out) :: lhs
    class (str), intent(in) :: rhs

    if (_VALID(rhs)) then
        lhs = rhs%value
    else
        lhs = EMPTY_VALUE
    end if
end subroutine

! *****************************************************************************
! EQUALITY operator

elemental function eq_str_str (lhs, rhs) result(res)
    class (str), intent(in) :: lhs, rhs
    logical :: res

    res = .false.
    if ((_VALID(lhs) .and. _VALID(rhs)) .and. (len(lhs) == len(rhs))) then
        res = (lhs%value == rhs%value)
    else
        ! in all other cases it's sufficient if both strings have length zero,
        ! which is the case if either of them is unallocated and the other
        ! one is a zero-length string
        res = (len(rhs) == 0) .and. (len(lhs) == 0)
    end if
end function

elemental function eq_str_char (lhs, rhs) result(res)
    class (str), intent(in) :: lhs
    character (len=*), intent(in) :: rhs
    logical :: res

    res = .false.
    if (_VALID(lhs)) then
        if (len(lhs) == len(rhs)) res = (lhs%value == rhs)
    else
        ! non-initialized str is equal to zero-length char
        res = (rhs == "")
    end if
end function

elemental function eq_char_str (lhs, rhs) result(res)
    character (len=*), intent(in) :: lhs
    class (str), intent(in) :: rhs
    logical :: res

    res = eq_str_char (rhs, lhs)
end function

! *****************************************************************************
! INEQUALITY operator

elemental function neq_str_str (lhs, rhs) result(res)
    class (str), intent(in) :: lhs, rhs
    logical :: res

    res = .not. eq_str_str (lhs, rhs)
end function

elemental function neq_str_char(lhs, rhs) result(res)
    class (str), intent(in) :: lhs
    character (len=*), intent(in) :: rhs
    logical :: res

    res = .not. eq_str_char(lhs, rhs)
end function

elemental function neq_char_str(lhs, rhs) result(res)
    character (len=*), intent(in) :: lhs
    class (str), intent(in) :: rhs
    logical :: res

    res = neq_str_char (rhs, lhs)
end function

! *****************************************************************************
! CONCATENATION operator

pure function concat_str_str (lhs, rhs) result(res)
    class (str), intent(in) :: lhs, rhs
    type (str) :: res

    if (_VALID(lhs) .and. _VALID(rhs)) then
        res = lhs%value // rhs%value
    else if (_VALID(lhs)) then
        res = lhs
    else
        res = rhs
    end if

end function

pure function concat_str_char(lhs, rhs) result(res)
    class (str), intent(in) :: lhs
    character (len=*), intent(in) :: rhs
    type (str) :: res

    if (_VALID(lhs)) then
        res = lhs%value // rhs
    else
        res = rhs
    end if

end function

pure function concat_char_str (lhs, rhs) result(res)
    character (len=*), intent(in) :: lhs
    class (str), intent(in) :: rhs
    type (str) :: res

    if (_VALID(rhs)) then
        res = lhs // rhs%value
    else
        res = lhs
    end if

end function


! *****************************************************************************
! SUBSTRING method

elemental function substring_to_int64 (self, ito) result(res)
    class (str), intent(in) :: self
    integer (int64), intent(in) :: ito
    type (str) :: res

    call substring_impl (self, 1_int64, ito, res)
end function

elemental function substring_from_to_int64 (self, ifrom, ito) result(res)
    class (str), intent(in) :: self
    integer (int64), intent(in) :: ifrom, ito
    type (str) :: res

   call substring_impl (self, ifrom, ito, res)
end function

elemental function substring_to_int32 (self, ito) result(res)
    class (str), intent(in) :: self
    integer (int32), intent(in) :: ito
    type (str) :: res

    call substring_impl (self, 1_int64, int(ito, int64), res)
end function

elemental function substring_from_to_int32 (self, ifrom, ito) result(res)
    class (str), intent(in) :: self
    integer (int32), intent(in) :: ifrom, ito
    type (str) :: res

   call substring_impl (self, int(ifrom, int64), int(ito, int64), res)
end function

elemental subroutine substring_impl (self, ifrom, ito, res)
    class (str), intent(in) :: self
    integer (int64), intent(in) :: ifrom, ito
    type (str), intent(out) :: res

    integer (int64) :: lifrom, lito

    if (_VALID(self)) then
        if (self /= "") then
            lifrom = adj_bound (self, ifrom)
            lito = adj_bound (self, ito)

            call res%reset ()
            res = self%value(lifrom:lito)
        end if
    end if

end subroutine

pure function adj_bound (self, i) result(res)
    class (str), intent(in) :: self
    integer (int64), intent(in) :: i
    integer (int64) :: res

    res = i
    if (i == 0) then
        res = 1
    else if (i > len(self) .and. len(self) > 0) then
        res = len(self)
    else if (i < 0) then
        ! negative integer interpreted as indexing backwards with -1
        ! referencing the last element.
        ! For too small (negative) i we bound the return index to be 1.
        res = 1 + max(0_int64, len(self) + i)
    end if

end function

! *****************************************************************************
! JOIN method

function join_str(self, str_list) result(res)
    class (str) :: self
    class (str), intent(in), dimension(:) :: str_list
    type (str) :: res

    call join_impl(self, str_list, res)

end function

function join_char(self, char_list, trim_blanks) result(res)
    class (str) :: self
    character (len=*), intent(in), dimension(:) :: char_list
    type (str) :: res
    logical, intent(in), optional :: trim_blanks

    logical :: ltrim
    type (str), dimension(size(char_list)) :: str_list

    integer :: i

    ltrim = .true.
    if (present(trim_blanks)) ltrim = trim_blanks

    ! convert to list or string objects
    do i = 1, size(str_list)
        if (ltrim) then
            str_list(i) = trim(char_list(i))
        else
            str_list(i) = char_list(i)
        end if
    end do

    call join_impl (self, str_list, res)

end function

pure subroutine join_impl(self, str_list, res)
    class (str), intent(in) :: self
    class (str), intent(in), dimension(:) :: str_list
    type (str), intent(out) :: res

    ! automatically deallocated on subroutine exit
    character (len=:), allocatable :: sep, work
    integer :: n, i, ifrom, ito
    integer :: res_len, sep_len, lengths(size(str_list))

    sep_len = len(self)
    ! gfortran does not support components as source parameters!
    allocate (character (len=sep_len) :: sep)
    sep = self

    n = size(str_list)
    lengths = len(str_list)

    ! determine the length of resulting string
    res_len = sum(lengths) + max(sep_len * (n - 1), 0)

    ! write result string in work array, as allocating character
    ! component with dynamic length crashes gfortran
    allocate (character (len=res_len) :: work)

    ifrom = 1
    do i = 1,n
        ! insert separator
        if (i > 1) then
            ito = ifrom + sep_len - 1
            work(ifrom:ito) = sep
            ifrom = ito + 1
        end if

        if (lengths(i) > 0) then
            ito = ifrom + lengths(i) - 1
            work(ifrom:ito) = str_list(i)%to_char()
        end if

        ifrom = ito + 1
    end do

    ! implementation-dependent!
    call move_alloc (work, res%value)

end subroutine

! *****************************************************************************
! REPEAT method (equivalently multiplication operator)

function repeat_str_int32 (lhs, rhs) result(res)
    class (str), intent(in) :: lhs
    integer (int32), intent(in) :: rhs
    type (str) :: res

    call repeat_impl (lhs, int(rhs, int64), res)

end function

function repeat_str_int64 (lhs, rhs) result(res)
    class (str), intent(in) :: lhs
    integer (int64), intent(in) :: rhs
    type (str) :: res

    call repeat_impl (lhs, rhs, res)
end function

function repeat_int32_str (lhs, rhs) result(res)
    integer, intent(in) :: lhs
    class (str), intent(in) :: rhs
    type (str) :: res

    call repeat_impl (rhs, int(lhs, int64), res)
end function

function repeat_int64_str (lhs, rhs) result(res)
    integer (int64), intent(in) :: lhs
    class (str), intent(in) :: rhs
    type (str) :: res

    call repeat_impl (rhs, lhs, res)
end function

subroutine repeat_impl (s, n, res)
    class (str), intent(in) :: s
    integer (int64), intent(in) :: n
    type (str), intent(out) :: res

    ! if s is unallocated, return an unallocated string
    res = repeat(s%to_char(), n)

end subroutine

! *****************************************************************************
! Finalization

! RESET reverts string object to initial state, ie. as if it had not been
! assigned a value.
! This procedure depends on how the string value is implemented, so no
! abstraction is gained by using preprocessor definitions
elemental subroutine reset(self)
    ! Note: unlike a finalizer, argument should be defined as polymorphic
    class (str), intent(in out) :: self

    if (allocated(self%value)) deallocate (self%value)
end subroutine

end module
