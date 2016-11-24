! define implementation-indep. function to determine whether string
! has been initialized with some value
#define _VALID(obj) allocated(obj%value)
#define _CLEAR(obj) if (allocated(obj%value)) deallocate(obj%value)

! gfortran up to v5.x incorrectly processes procedure calls if the actual argument
! is a temporary array of user-derived type, and the dummy argument is
! polymorphic; see https://gcc.gnu.org/bugzilla/show_bug.cgi?id=60322
#if __GFORTRAN__ && (__GNUC__  < 6)
#define _POLYMORPHIC_ARRAY(t) type (t)
#else
#define _POLYMORPHIC_ARRAY(t) class (t)
#endif


module corelib_string_str_mod

    use iso_fortran_env
    use corelib_common

    implicit none
    private

    integer, parameter :: ASCII_TAB = 9
    ! 10, 11, and 12 are additional white space characters
    integer, parameter :: ASCII_CR = 13
    integer, parameter :: ASCII_SPACE = 32

    character (*), parameter :: EMPTY_VALUE = ""

    type :: str
        private
        character (len=:), allocatable :: value
    contains

        procedure, pass :: reset
        procedure, pass :: alloc_int
        procedure, pass :: alloc_char
        generic, public :: alloc => alloc_int, alloc_char

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

        procedure, pass :: split_str
        procedure, pass :: split_char
        generic, public :: split => split_str, split_char

        procedure, pass :: trim => str_trim

        procedure, pass :: count => str_count

        procedure, pass :: lower => str_lower
        ! procedure, pass :: upper => str_upper

        ! Parsers for other data types
        procedure, pass :: parse_int32
        procedure, pass :: parse_int64
        procedure, pass :: parse_real32
        procedure, pass :: parse_real64
        procedure, pass :: parse_str
        procedure, pass :: parse_char
        procedure, pass :: parse_logical
        generic, public :: parse => parse_int32, parse_int64, parse_real32, &
            parse_real64, parse_str, parse_char, parse_logical

        ! Finalizers
        ! final :: finalize
    end type

    interface str
        module procedure ctor_char, ctor_real64, ctor_real32, ctor_int64, &
            ctor_int32, ctor_logical
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

    interface len_trim
        module procedure len_trim_str
    end interface

    ! overload instrinsic repeat() to operator on str types
    interface repeat
        module procedure repeat_str_int64, repeat_str_int32, repeat_int32_str, &
            repeat_int64_str
    end interface

    interface index
        module procedure index_str_str, index_char_str, index_str_char
    end interface

    interface trim
        module procedure trim_str
    end interface

    interface dynamic_cast
        module procedure cast_any_to_str, cast_any_to_str_array
    end interface

    public :: str, str_array, len, len_trim, repeat, index, trim
    public :: operator (+), operator (//), operator (/=), operator (==), operator (*)
    public :: assignment (=)
    public :: dynamic_cast

contains

! *****************************************************************************
! Initialization

elemental function ctor_char (ch) result(res)
    character (len=*), intent(in) :: ch
    type (str) :: res

    call res%alloc (ch)

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

    call res%alloc (trim(buf))
end function

elemental function ctor_real32 (from_real, fmt) result(res)
    real (real32), intent(in) :: from_real
    character (len=*), intent(in), optional :: fmt
    type (str) :: res

    integer, parameter :: LEN_BUFFER = 100
    character (len=LEN_BUFFER) :: buf

    if (present(fmt)) then
        write (unit=buf, fmt=pad_format(fmt)) from_real
    else
        write (buf, *) from_real
    end if

    call res%alloc (trim(buf))
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

    call res%alloc (trim(buf))
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

    call res%alloc (trim(buf))
end function

elemental function ctor_logical (x) result(res)
    logical, intent(in) :: x
    type (str) :: res

    integer, parameter :: LEN_BUFFER = 10
    character (len=LEN_BUFFER) :: buf

    write (unit=buf, fmt=*) x
    call res%alloc (trim(buf))

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

elemental function len_str (obj) result(res)
    class (str), intent(in) :: obj
    integer :: res

    res = 0
    if (_VALID(obj)) res = len(obj%value)
end function

elemental function len_trim_str (obj) result(res)
    class (str), intent(in) :: obj
    integer :: res

    res = 0
    if (_VALID(obj)) res = len_trim(obj%value)
end function

! *****************************************************************************
! Conversion routines

pure function to_char(self) result(res)
    class (str), intent(in) :: self
    character (len=len(self)) :: res

    ! if self is not initialized, len(self) will return 0 and thus res
    ! will already be a zero-length character. Nothing else is needed to
    ! handle this case.
    if (_VALID(self)) then
        res = self%value
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

    if (_VALID(rhs)) then
        call lhs%alloc (rhs%value)
    end if
end subroutine

elemental subroutine assign_str_char(lhs, rhs)
    type (str), intent(out) :: lhs
    character (len=*), intent(in) :: rhs

    ! lhs%value should be automatically deallocated due to intent(out)
    call lhs%alloc (rhs)
end subroutine

elemental subroutine assign_char_str (lhs, rhs)
    class (str), intent(in) :: rhs
    character (len=*), intent(in out) :: lhs

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

pure function join_str(self, str_list) result(res)
    class (str), intent(in) :: self
    _POLYMORPHIC_ARRAY (str), intent(in), dimension(:) :: str_list
    type (str) :: res

    call join_impl(self, str_list, res)

end function

pure function join_char(self, char_list, trim_blanks) result(res)
    class (str), intent(in) :: self
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
    _POLYMORPHIC_ARRAY (str), intent(in), dimension(:) :: str_list
    type (str), intent(out) :: res

    ! automatically deallocated on subroutine exit
    character (len=:), allocatable :: sep
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

    call res%alloc (res_len)

    ifrom = 1
    do i = 1,n
        ! insert separator
        if (i > 1) then
            ito = ifrom + sep_len - 1
            res%value(ifrom:ito) = sep
            ifrom = ito + 1
        end if

        if (lengths(i) > 0) then
            ito = ifrom + lengths(i) - 1
            res%value(ifrom:ito) = str_list(i)%to_char()
            ifrom = ito + 1
        end if
    end do

end subroutine

! *****************************************************************************
! COUNT method

! COUNT returns the number of non-overlapping occurences of string 's' in
! string instance, optionally limited to the substring identified by start:end.
! The implementation is modelled after Python's string.count() function, and
! should thus have the same behavior in "pathological" cases, in which
! either the start:end range is invalid, or the string instance or the substring s
! are empty.
elemental function str_count (self, s, start, end) result(res)
    class (str), intent(in) :: self, s
    integer, intent(in), optional :: start, end
    integer :: res

    integer :: lstart, lend, n, ns, i

    lstart = 0
    lend = len(self)
    n = lend
    ns = len(s)
    ! default return value
    res = 0

    if (present(start)) lstart = start
    if (present(end)) lend = end

    ! gracefully exit if start is larger than end index, or if start index
    ! is outside of the valid range. Return 0
    if (lstart > lend .or. lstart > max(1, n)) return
    ! At this point we know that the start value is in the valid range 1:max(1,n);

    ! interpret negative indices as starting from the end of the string
    if (lstart < 0) lstart = n + lstart + 1
    if (lend < 0) lend = n + lend + 1

    ! if selected range is an empty string within a *valid* range of
    ! string instance, and substring is empty, return 1
    if (lstart <= max(1, n) .and. (lstart == lend + 1) .and. ns == 0) then
        res = 1
        return
    end if

    ! return 0 if requested range makes no sense in all other cases
    if (lstart > lend) return

    ! if s is the empty string, return the number of characters in the requested
    ! substring + 1; ie. an empty string separates each character and is also
    ! present and the start and end of the substring.
    if (ns == 0) then
        res = lend - lstart + 2
        return
    end if

    ! At this point we have a properly defined problem, it remains to cycle through
    ! the characters and count the number of non-overlapping occurences.
    i = lstart
    res = 0
    do while (i <= lend - ns + 1)
        if (self%value(i:i+ns) == s) then
            res = res + 1
            i = i + ns
            cycle
        end if
    end do

end function

! *****************************************************************************
! SPLIT method

pure subroutine split_str (self, str_list, sep, drop_empty, status)
    class (str), intent(in) :: self
    type (str), intent(in out), dimension(:), allocatable :: str_list
    class (str), intent(in), optional :: sep
    logical, intent(in), optional :: drop_empty
    integer, intent(out), optional :: status

    integer :: i, nsep, n, m, lstatus, nfound, is, ie, iend_last_sep
    integer, dimension(:), allocatable :: istart, iend
    logical :: ldrop_emtpy

    ! do not drop empty strings by default, thus replicating Python behavior
    ldrop_emtpy = .false.
    if (present(drop_empty)) ldrop_emtpy = drop_empty

    nsep = 0
    if (present(sep)) then
        nsep = len(sep)
    end if
    n = len(self)

    ! do not allow empty separators, not even if the string instance is empty
    if (present(sep) .and. nsep == 0) then
        lstatus = STATUS_INVALID_INPUT
        goto 100
    endif

    ! Emulate Python behavior: if string instance is empty, and substring
    ! other than empty string will yield a return list with empty string as
    ! only item.
    if (self == '') then
        if (ldrop_emtpy) then
            if (allocated(str_list)) deallocate(str_list)
            allocate (str_list(0))
        else
            if (allocated(str_list)) deallocate(str_list)
            allocate (str_list(1))
            str_list(1) = ''
        end if
        return
    end if

    ! array to store list of starting indexes of sep instances; start with
    ! array size of at most 10
    m = min(max(2, n), 10)
    allocate (istart(m), iend(m))

    ! find all starting indices of non-overlapping instances of sep
    nfound = 0

    ! if sep is present, find substrings (potentially zero-length)
    ! that are separated by non-overlapping sep.
    if (present(sep)) then

        ! position in string where last encountered separator ends (include)
        ! We pretent the string is sandwiched between two (virtual) separators.
        iend_last_sep = 0
        i = 1

        do while (i <= n - nsep + 1)
            if (self%value(i:i+nsep-1) == sep) then
                ! start of valid substring is one past previously encountered
                ! seperator
                is = iend_last_sep + 1
                ! end of valid substring is located at previous character
                ie = i - 1

                ! check whether the substring found is empty and include it
                ! only if not dropping empty substrings
                if (is <= ie .or. .not. ldrop_emtpy) then
                    nfound = nfound + 1
                    call add_substring (istart, iend, nfound, is, ie)
                end if

                ! point to end of current separator
                iend_last_sep = i + nsep - 1

                i = i + nsep
            else
                i = i + 1
            end if
        end do

        ! add last substring that extends from last found separator to the
        ! end of the string
        is = iend_last_sep + 1
        ie = n

        ! only add if non-empty, or if empty substrings permitted
        if (is <= ie .or. .not. ldrop_emtpy) then
            nfound = nfound + 1
            call add_substring (istart, iend, nfound, is, ie)
        end if
    else
        ! handle default case when all contiguous white space is merged
        ! to form a separator
        is = 1
        do while (is <= n)

            do while (is <= n)
                if (.not. is_ascii_whitespace(self%value(is:is))) exit
                is = is + 1
            end do

            ! Exit loop if we reached the end of the string, no more valid
            ! substrings to be expected
            if (is == n + 1) exit

            ! at this point we have reached a non-whitespace character;
            ! locate end of valid substring
            ie = is
            do while (ie <= n)
                if (is_ascii_whitespace(self%value(ie:ie))) exit
                ie = ie + 1
            end do

            ! stepped one character too far, current position is either whitespace
            ! or one element past the end of the string
            ie = ie - 1

            ! store substring start and end indices
            nfound = nfound + 1
            call add_substring (istart, iend, nfound, is, ie)

            ! if valid substring was at the end of string, terminate
            if (ie == n) exit
            ! otherwise continue with next character, which is white space
            is = ie + 1
        end do
    end if

    ! re-allocate if str_list is not exactly the size needed to
    ! hold result
    if (allocated(str_list)) then
        if (size(str_list) /= nfound) then
            deallocate(str_list)
            allocate (str_list(nfound))
        end if
    else
        allocate (str_list(nfound))
    end if

    do i = 1, nfound
        is = istart(i)
        ie = iend(i)
        str_list(i) = self%value(is:ie)
    end do

    lstatus = STATUS_OK

100 continue
    if (present(status)) status = lstatus

contains

    pure subroutine add_substring (istart, iend, ifound, is, ie)
        integer, intent(in out), dimension(:), allocatable :: istart, iend
        integer, intent(in) :: ifound, ie, is

        ! if required, allocate more space to hold indices
        call alloc_minsize (istart, ifound, 10)
        call alloc_minsize (iend, ifound, 10)

        istart(ifound) = is
        iend(ifound) = ie
    end subroutine
end subroutine

pure subroutine split_char (self, str_list, sep, drop_empty, status)
    class (str), intent(in) :: self
    type (str), intent(in out), dimension(:), allocatable :: str_list
    character (*), intent(in) :: sep
    logical, intent(in), optional :: drop_empty
    integer, intent(out), optional :: status

    type (str) :: lsep

    lsep = sep
    call self%split (str_list, lsep, drop_empty, status)
end subroutine

! IS_ASCII_WHITESPACE returns true if the first character in s
! is considered whitespace in the ASCII character set.
pure function is_ascii_whitespace (s) result(res)
    character (*), intent(in) :: s
    logical :: res

    integer :: i
    i = iachar(s)
    res = (i >= ASCII_TAB .and. i <= ASCII_CR) .or. i == ASCII_SPACE
end function

! *****************************************************************************
! REPEAT method (equivalently multiplication operator)

pure function repeat_str_int32 (lhs, rhs) result(res)
    class (str), intent(in) :: lhs
    integer (int32), intent(in) :: rhs
    type (str) :: res

    call repeat_impl (lhs, int(rhs, int64), res)

end function

pure function repeat_str_int64 (lhs, rhs) result(res)
    class (str), intent(in) :: lhs
    integer (int64), intent(in) :: rhs
    type (str) :: res

    call repeat_impl (lhs, rhs, res)
end function

pure function repeat_int32_str (lhs, rhs) result(res)
    integer, intent(in) :: lhs
    class (str), intent(in) :: rhs
    type (str) :: res

    call repeat_impl (rhs, int(lhs, int64), res)
end function

pure function repeat_int64_str (lhs, rhs) result(res)
    integer (int64), intent(in) :: lhs
    class (str), intent(in) :: rhs
    type (str) :: res

    call repeat_impl (rhs, lhs, res)
end function

pure subroutine repeat_impl (s, n, res)
    class (str), intent(in) :: s
    integer (int64), intent(in) :: n
    type (str), intent(out) :: res

    ! if s is unallocated, return an unallocated string
    res = repeat(s%to_char(), n)

end subroutine

! ******************************************************************************
! LOWER method

elemental function str_lower (self) result(res)
    class (str), intent(in) :: self
    type (str) :: res

    integer :: i, j, lb, ub, offset

    if (.not. _VALID(self)) return

    lb = iachar ("A")
    ub = iachar ("Z")
    offset = iachar ("a")

    res = self

    do i = 1, len(self)
        j = iachar (self%value(i:i))
        if (j >= lb .and. j <= ub) then
            res%value(i:i) = achar (offset + j - lb)
        end if
    end do

end function


! ******************************************************************************
! INDEX routine

elemental function index_str_str (s1, s2, back, kind) result(res)
    class (str), intent(in) :: s1, s2
    logical, intent(in), optional :: back
    integer, intent(in), optional :: kind
    integer :: res

    if (_VALID(s1) .and. _VALID(s2)) then
        res = index (s1%value, s2%value, back)
    else if (_VALID(s1)) then
        res = index (s1%value, "", back)
    else if (_VALID(s2)) then
        res = index ("", s2%value, back)
    else
        res = index ("", "", back)
    end if

end function

elemental function index_str_char (s, char, back, kind) result(res)
    class (str), intent(in) :: s
    character (*), intent(in) :: char
    logical, intent(in), optional :: back
    integer, intent(in), optional :: kind
    integer :: res

    if (_VALID(s)) then
        res = index (s%value, char, back)
    else
        res = index ("", char, back)
    end if
end function

elemental function index_char_str (char, s1, back, kind) result(res)
    character (*), intent(in) :: char
    class (str), intent(in) :: s1
    logical, intent(in), optional :: back
    integer, intent(in), optional :: kind
    integer :: res

    if (_VALID(s1)) then
        res = index (char, s1%value, back)
    else
        res = index (char, "", back)
    end if
end function

! *****************************************************************************
! TRIM method
elemental subroutine str_trim (self)
    class (str), intent(in out) :: self

    if (_VALID(self)) self = trim(self%value)
end subroutine

! TRIM_STR is an overload for trim() with argument of type str.
pure function trim_str (self) result(res)
    class (str), intent(in) :: self
    class (str), allocatable :: res

    allocate (res, source=self)
    call res%trim ()
end function

! *****************************************************************************
! Converstion to other native data types
subroutine parse_int32 (self, val, status)
    integer (int32) :: val
    include "include/str_parse.f90"
end subroutine

subroutine parse_int64 (self, val, status)
    integer (int64) :: val

    include "include/str_parse.f90"
end subroutine

subroutine parse_real32 (self, val, status)
    real (real32) :: val
    include "include/str_parse.f90"
end subroutine

subroutine parse_real64 (self, val, status)
    real (real64) :: val
    include "include/str_parse.f90"
end subroutine

subroutine parse_str(self, val, status)
    class (str), intent(in) :: self
    class (str), intent(out) :: val
    integer, intent(out), optional :: status

    if (present(status)) status = STATUS_OK

    if (_VALID(self)) then
        val = self%value
    else
        val = ""
    end if
end subroutine

subroutine parse_char(self, val, status)
    class (str), intent(in) :: self
    character (*), intent(out) :: val
    integer, intent(out), optional :: status

    if (present(status)) status = STATUS_OK

    if (_VALID(self)) then
        val = self%value
    else
        val = ""
    end if
end subroutine

subroutine parse_logical (self, val, status)
    logical :: val
    include "include/str_parse.f90"
end subroutine

! ******************************************************************************
! Casts to str polymorphic objects
subroutine cast_any_to_str (tgt, ptr, status)
    class (*), intent(in), target :: tgt
    class (str), intent(out), pointer :: ptr
    integer, intent(out), optional :: status

    integer :: lstatus

    lstatus = STATUS_UNSUPPORTED_OPERATION

    select type (tgt)
    class is (str)
        ptr => tgt
        lstatus = STATUS_OK
    end select

    if (present(status)) status = lstatus
end subroutine

subroutine cast_any_to_str_array (tgt, ptr, status)
    class (*), intent(in), dimension(:), target :: tgt
    _POLYMORPHIC_ARRAY (str), intent(out), dimension(:), pointer :: ptr
    integer, intent(out), optional :: status

    integer :: lstatus

    lstatus = STATUS_UNSUPPORTED_OPERATION

    select type (tgt)
    class is (str)
        ptr => tgt
        lstatus = STATUS_OK
    end select

    if (present(status)) status = lstatus
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

! ******************************************************************************
! Allocation
! Abstract away from how character value is stored internally but handling
! allocation in dedicated procedures.
pure subroutine alloc_int (self, length)
    class (str), intent(in out) :: self
    integer, intent(in) :: length

    if (allocated(self%value)) deallocate (self%value)
    allocate (character (length) :: self%value)

end subroutine

pure subroutine alloc_char (self, value)
    class (str), intent(in out) :: self
    character (*), intent(in) :: value

    integer :: length

    if (allocated(self%value)) deallocate (self%value)

    length = len(value)
    allocate (character (length) :: self%value)
    self%value = value

end subroutine

end module
