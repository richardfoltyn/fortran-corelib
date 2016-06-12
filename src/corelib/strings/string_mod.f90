module string_mod

    use iso_fortran_env

    implicit none
    private

    integer, parameter :: UNALLOCATED = -1

    type :: str
        private

        integer :: n = UNALLOCATED
        character (len=:), allocatable :: value

    contains

        procedure, pass :: is_valid

        procedure, public, pass :: to_char
        procedure, public, pass :: length

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
        final :: finalize
    end type

    interface str
        module procedure ctor_char, ctor_real64, ctor_real, ctor_int64, ctor_int32
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

    public :: str, len, repeat
    public :: operator (+), operator (//), operator (/=), operator (==), operator (*)
    public :: assignment (=)
contains

! *****************************************************************************
! Initialization

elemental function ctor_char (ch) result(res)
    character (len=*), intent(in) :: ch
    type (str) :: res

    allocate (res%value, source=ch)
    res%n = len(res%value)

end function

elemental function ctor_real64 (from_real, fmt) result(res)
    real (real64), intent(in) :: from_real
    character (len=*), intent(in), optional :: fmt
    type (str) :: res

    integer, parameter :: LEN_BUFFER = 100
    character (len=LEN_BUFFER) :: buf

    if (present(fmt)) then
        write (unit=buf, fmt=fmt) from_real
    else
        write (buf, *) from_real
    end if

    allocate (res%value, source=trim(buf))
    res%n = len(res%value)

end function

elemental function ctor_real (from_real, fmt) result(res)
    real, intent(in) :: from_real
    character (len=*), intent(in), optional :: fmt
    type (str) :: res

    integer, parameter :: LEN_BUFFER = 100
    character (len=LEN_BUFFER) :: buf

    if (present(fmt)) then
        write (unit=buf, fmt=fmt) from_real
    else
        write (buf, *) from_real
    end if

    allocate (res%value, source=trim(buf))
    res%n = len(res%value)

end function

elemental function ctor_int64 (from_int, fmt) result(res)
    integer (int64), intent(in) :: from_int
    character (len=*), intent(in), optional :: fmt
    type (str) :: res

    integer, parameter :: LEN_BUFFER = 100
    character (len=:), allocatable :: buf

    allocate (character (len=LEN_BUFFER) :: buf)

    if (present(fmt)) then
        write (unit=buf, fmt=fmt) from_int
    else
        write (unit=buf, fmt="(i0)") from_int
    end if

    allocate (res%value, source=trim(buf))
    res%n = len(res%value)

end function

elemental function ctor_int32 (from_int, fmt) result(res)
    integer (int32), intent(in) :: from_int
    character (len=*), intent(in), optional :: fmt
    type (str) :: res

    integer, parameter :: LEN_BUFFER = 100
    character (len=:), allocatable :: buf

    allocate (character (len=LEN_BUFFER) :: buf)

    if (present(fmt)) then
        write (unit=buf, fmt=fmt) from_int
    else
        write (unit=buf, fmt="(i0)") from_int
    end if

    allocate (res%value, source=trim(buf))
    res%n = len(res%value)

end function

elemental function is_valid (self) result(res)
    class (str), intent(in) :: self
    logical :: res

    res = self%n >= 0 .and. allocated (self%value)
end function

! *****************************************************************************
! Public attributes

function length(self) result(res)
    class (str), intent(in) :: self
    integer :: res

    res = len(self)

end function

elemental function len_str (self) result(res)
    class (str), intent(in) :: self
    integer :: res

    res = self%n
end function

! *****************************************************************************
! Conversion routines

pure function to_char(self) result(res)
    class (str), intent(in) :: self
    character (len=len(self)) :: res

    res = ""
    if (allocated(self%value)) res = self%value

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

    if (self%is_valid() .and. len(self) >= len(prefix)) then
        res = self%value(1:len(prefix)) == prefix
    end if

end function

elemental function startswith_str(self, prefix) result(res)
    class (str), intent(in) :: self, prefix
    logical :: res

    res = .false.
    if (self%is_valid() .and. prefix%is_valid()) then
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

    if (self%is_valid() .and. (len(self) >= len(suffix))) then
        ifrom = len(self)-len(suffix) + 1
        ito = len(self)
        res = self%value(ifrom:ito) == suffix
    end if
end function

elemental function endswith_str(self, suffix) result(res)
    class (str), intent(in) :: self, suffix
    logical :: res

    res = .false.
    if (self%is_valid() .and. suffix%is_valid()) then
        res = endswith_char(self, suffix%value)
    end if

end function

! *****************************************************************************
! ASSIGNMENT operator
elemental subroutine assign_str_str(lhs, rhs)
    class (str), intent(out) :: lhs
    class (str), intent(in) :: rhs

    ! prevent unallocated rhs object to be used to allocate lhs, as then
    ! allocated (lhs%value) returns .TRUE.
    if (rhs%is_valid()) then
        ! lhs%value should be automatically deallocated due to intent(out)
        allocate (lhs%value, source=rhs%value)
    end if

    lhs%n = len(lhs%value)
end subroutine

elemental subroutine assign_str_char(lhs, rhs)
    class (str), intent(out) :: lhs
    character (len=*), intent(in) :: rhs

    ! lhs%value should be automatically deallocated due to intent(out)
    allocate (lhs%value, source=rhs)
    lhs%n = len(lhs%value)
end subroutine

elemental subroutine assign_char_str (lhs, rhs)
    character (len=*), intent(in out) :: lhs
    class (str), intent(in) :: rhs

    ! invoke intrinsic assignment if str%value is allocated, otherwise leave
    ! lhs character unchanged
    if (rhs%is_valid()) lhs = rhs%to_char()

end subroutine

! *****************************************************************************
! EQUALITY operator

elemental function eq_str_str (lhs, rhs) result(res)
    class (str), intent(in) :: lhs, rhs
    logical :: res

    res = .false.
    if ((lhs%is_valid().and. rhs%is_valid()) .and. &
        (lhs%n == rhs%n)) then

        res = (lhs%value == rhs%value)
    end if
end function

elemental function eq_str_char (lhs, rhs) result(res)
    class (str), intent(in) :: lhs
    character (len=*), intent(in) :: rhs
    logical :: res

    res = .false.
    if (lhs%is_valid() .and. (len(lhs) == len(rhs))) then
        res = (lhs%value == rhs)
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

    res = .false.
    if (lhs%is_valid() .and. rhs%is_valid()) res = .not. eq_str_str (lhs, rhs)
end function

elemental function neq_str_char(lhs, rhs) result(res)
    class (str), intent(in) :: lhs
    character (len=*), intent(in) :: rhs
    logical :: res

    res = .false.
    if (lhs%is_valid()) res = .not. eq_str_char(lhs, rhs)
end function

elemental function neq_char_str(lhs, rhs) result(res)
    character (len=*), intent(in) :: lhs
    class (str), intent(in) :: rhs
    logical :: res

    res = neq_str_char (rhs, lhs)
end function

! *****************************************************************************
! CONCATENATION operator

function concat_str_str (lhs, rhs) result(res)
    class (str), intent(in) :: lhs, rhs
    type (str) :: res

    if (rhs%is_valid() .and. rhs%is_valid()) then
        call concat_impl (lhs%value, rhs%value, res)
    else if (lhs%is_valid()) then
        res = lhs
    else
        res = rhs
    end if

end function

function concat_str_char(lhs, rhs) result(res)
    class (str), intent(in) :: lhs
    character (len=*), intent(in) :: rhs
    type (str) :: res

    if (lhs%is_valid()) then
        call concat_impl (lhs%value, rhs, res)
    else
        res = rhs
    end if

end function

function concat_char_str (lhs, rhs) result(res)
    character (len=*), intent(in) :: lhs
    class (str), intent(in) :: rhs
    type (str) :: res

    if (rhs%is_valid()) then
        call concat_impl (lhs, rhs%value, res)
    else
        res = lhs
    end if

end function

subroutine concat_impl(lhs, rhs, res)
    character (len=*), intent(in) :: lhs, rhs
    ! intent(out) will automatically deallocate value in res
    type (str), intent(out) :: res

    ! lhs%value should be automatically deallocated due to intent(out)
    allocate (res%value, source=lhs // rhs)
    res%n = len(res%value)

end subroutine

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

    if (self%is_valid()) then
        if (self == "") then
            ! treat empty string separately, any substring of an empty string can
            ! only be empty.
            res = ""
        else
            lifrom = adj_bound (self, ifrom)
            lito = adj_bound (self, ito)

            allocate (res%value, source=self%value(lifrom:lito))
            res%n = len(res%value)
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
    integer :: nstr, i, length, ifrom, ito
    logical, dimension(size(str_list)) :: is_valid

    ! separator: use empty string as default separator if self is not in
    ! valid state
    if (self%is_valid()) then
        allocate (sep, source=self%value)
    else
        allocate (sep, source="")
    end if

    is_valid = str_list%is_valid()

    ! determine the length of all components; ingore those not valid since
    ! they might have len < 0, but will be ignored in the concatenation
    length = sum(pack(str_list%n, is_valid))
    ! number of allocated strings to be joined
    nstr = count(is_valid)

    ! add length of separators
    length = length + max(len(sep) * nstr - 1, 0)

    ! write result string in work array, as allocating character
    ! component with dynamic length crashes gfortran
    allocate (character (len=length) :: work)

    ifrom = 1
    do i = 1,nstr
        if (str_list(i)%is_valid()) then
            ! insert separator
            if (i > 1) then
                ito = ifrom + len(sep) - 1
                work(ifrom:ito) = sep
                ifrom = ito + 1
            end if

            ito = ifrom + str_list(i)%n - 1
            work(ifrom:ito) = str_list(i)%value

            ifrom = ito + 1
        end if
    end do

    call move_alloc (work, res%value)
    res%n = len(res%value)

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
    if (s%is_valid()) then
        allocate (res%value, source=repeat(s%to_char(), n))
        res%n = len(res%value)
    end if

end subroutine

! *****************************************************************************
! Finalization

impure elemental subroutine finalize(self)
    type (str), intent(in out) :: self

    if (allocated(self%value)) deallocate (self%value)
    self%n = UNALLOCATED

end subroutine

end module
