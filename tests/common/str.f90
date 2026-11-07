program test_strings

    use iso_fortran_env
    use fcore_common

    implicit none

    ! Some test data used to build strings
    character (len=*), parameter :: CHAR_VALUE1 = "test string"
    character (len=*), parameter :: CHAR_VALUE2 = "foo bar 123"
    character (len=*), parameter :: &
        CHAR_VALUE3 = "The quick brown fox jumps over the lazy dog"

    real (real64), parameter :: REAL_VALUE1 = 123.456
    integer, parameter :: INT_VALUE1 = 123

    call test_init ()
    call test_len ()
    call test_assign ()
    call test_equal ()
    call test_substring ()
    call test_concat ()
    call test_join ()
    call test_split ()
    call test_repeat ()
    call test_startswith ()
    call test_endswith ()
    call test_array ()
    call test_parse ()

contains

subroutine test_init ()
    !* Check string initialization from characters and numeric values.

    type (str) :: s1, s2

    ! test initialization from character
    s1 = str(CHAR_VALUE1)
    if (.not. ((s1 == CHAR_VALUE1) .and. len(s1) == len(CHAR_VALUE1))) &
        error stop "Init from character"

    ! initialization from other string
    s2 = s1
    if (.not. (s1 == s2 .and. len(s1) == len(s2))) error stop "Init from str"

    ! initialization from real
    s1 = str (REAL_VALUE1, fmt="(f0.2)")
    if (.not. (s1 == "123.46" .and. len(s1) == 6)) &
        error stop "Init from real (real64), F editing"

    ! initialization from integer
    s1 = str (INT_VALUE1)
    if (.not. (s1 == "123" .and. len(s1) == 3)) &
        error stop "Init from integer, no editing"

    s1 = str (INT_VALUE1, fmt="i0.6")
    if (.not. (s1 == "000123" .and. len(s1) == 6)) &
        error stop "Init from integer, fmt argument w/o parentheses"

end subroutine

! ******************************************************************************
! Tests for string representations of arrays

subroutine test_array ()
    !* Check string representations of empty and single-element arrays.

    integer (int32), parameter :: INT_VALUE1 = 1, INT_VALUE2 = int(1e6), &
        INT_VALUE3 = 1234567890

    real (real32), parameter :: REAL_VALUE1 = 0.123, REAL_VALUE2 = 1.23e10

    type (str) :: s1, s2

    ! test with empty array
    s1 = str_array([integer (int32) ::])
    if (.not. (s1 == "[]")) error stop "Empty int32 array"

    s1 = ""
    s1 = str_array([integer (int64) ::])
    if (.not. (s1 == "[]")) error stop "Empty int64 array"

    s1 = ""
    s1 = str_array([real (real32) ::])
    if (.not. (s1 == "[]")) error stop "Empty real32 array"

    s1 = ""
    s1 = str_array([real (real64) ::])
    if (.not. (s1 == "[]")) error stop "Empty real64 array"

    ! test 1d-arrays with one element
    s1 = ""
    s1 = str_array([INT_VALUE1])
    if (.not. (s1 == "[" // str(INT_VALUE1) // "]")) &
        error stop "String representation of a single-element int32 array"

    s1 = ""
    s1 = str_array([integer (int64) :: INT_VALUE1])
    if (.not. (s1 == "[" // str(INT_VALUE1) // "]")) &
        error stop "String representation of a single-element int64 array"

    s1 = ""
    s1 = str_array([REAL_VALUE1])
    s2 = str(REAL_VALUE1)
    if (.not. (s1 == "[" // s2 // "]")) &
        error stop "String representation of a single-element real32 array"

    s1 = ""
    s1 = str_array([real (real64) :: REAL_VALUE1])
    s2 = str(real(REAL_VALUE1, real64))
    if (.not. (s1 == "[" // s2 // "]")) &
        error stop "String representation of a single-element real64 array"

end subroutine

subroutine test_len ()
    !* Check lengths of initialized, empty, and uninitialized strings.

    type (str) :: s1, s2

    s1 = CHAR_VALUE1
    if (.not. (len(s1) == len(CHAR_VALUE1))) error stop "len(str) generic"

    s1 = ""
    if (.not. (len(s1) == 0)) error stop "len(str) generic, zero-length string"

    if (.not. (len(s2) == 0)) &
        error stop "len(str) generic, uninitialized string"

end subroutine

subroutine test_assign ()
    !* Check assignment between strings and character values.

    type (str) :: s1, s2, s99
    type (str), allocatable :: s3
    character (len=:), allocatable :: ch1, ch2
    character (len=0) :: ch3

    s1 = s2
    ! str = str assignment with non-initialized str objects
    if (.not. (s1 == s2)) error stop "str = str, not initialized"

    ! str = str assignment
    s1 = CHAR_VALUE1
    s2 = s1
    if (.not. ((s1 == s2) .and. (s1%to_char() == s2%to_char()))) &
        error stop "str = str"

    ! str = character assignment
    s1 = CHAR_VALUE1
    if (.not. (s1 == CHAR_VALUE1)) error stop "str = char"

    ! str = ""
    if (allocated(s3)) deallocate (s3)
    allocate (s3)
    s3 = ""
    if (.not. (s3 == "" .and. len(s3) == 0)) error stop "str = ''"

    ! str = character (zero length)
    if (allocated(s3)) deallocate (s3)
    allocate (s3)
    s3 = ch3
    if (.not. (s3 == "" .and. len(s3) == 0)) &
        error stop "str = char, rhs zero-length"

    ! character  = str assignment
    s1 = CHAR_VALUE1
    ! could use autoallocation from Fortran 2003
    if (allocated(ch1)) deallocate (ch1)
    allocate (character (len=len(CHAR_VALUE1)) :: ch1)
    ! erase LHS character
    ch1 = repeat(" ", len(ch1))
    ch1 = s1
    if (.not. (ch1 == s1)) error stop "char = str"

    ! character = str, not initialized str
    ch1 = repeat("x", len(ch1))
    allocate (ch2, source=ch1)
    ch1 = s99
    ! Check that assignment did not alter string contents
    if (.not. ((ch1 == ""))) error stop "char = str, rhs not initialized"

    ! character = str, zero-length str
    s1 = ""
    if (allocated(ch1)) deallocate (ch1)
    allocate (character (len=5) :: ch1)
    ch1 = s1
    if (.not. (ch1 == repeat(" ", len(ch1)))) &
        error stop "char = str, zero-length rhs"

end subroutine

subroutine test_equal ()
    !* Check string equality and inequality for initialized and empty values.

    type (str) :: s1, s2, s99
    character (len=:), allocatable :: ch1

    ! not initialized strings: should be equal
    if (.not. (s1 == s2)) error stop "str == str, both not initialized"
    if (s1 /= s2) error stop "str /= str, both not initialized"

    ! not initialized strings: one operand not initialized; should be not
    ! equal to non-zero string
    s1 = CHAR_VALUE1
    if (s2 == s1) error stop "str == str, lhs not initialized"
    if (s1 == s2) error stop "str == str, rhs not initialized"

    if (.not. (s2 /= s1)) error stop "str /= str, lhs not initialized"
    if (.not. (s1 /= s2)) error stop "str /= str, rhs not initialized"

    ! not initialized vs. zero-length: should be equal
    s1 = ""
    if (.not. (s2 == s1)) &
        error stop "str == str, lhs not initialized, rhs zero-length"
    if (.not. (s1 == s2)) &
        error stop "str == str, rhs not initialized, lhs zero-length"

    ! char vs. not initialized, zero-length char
    if (allocated(ch1)) deallocate (ch1)
    allocate (ch1, source="")
    if (.not. (s99 == ch1)) &
        error stop "str == char, lhs not initialized, rhs zero-length"
    if (.not. (ch1 == s99)) &
        error stop "str == str, rhs not initialized, lhs zero-length"

    if (s99 /= ch1) &
        error stop "str /= char, lhs not initialized, rhs zero-length"
    if (ch1 /= s99) &
        error stop "str /= str, rhs not initialized, lhs zero-length"

    ! both operands zero-length: should equal
    s1 = ""
    s2 = ""
    if (.not. (s1 == s2)) error stop "str == str, both zero-length"
    if (s1 /= s2) error stop "str /= str, both zero-length"

    ! zero-length str vs. zero-length character
    s1 = ""
    if (allocated(ch1)) deallocate (ch1)
    allocate (ch1, source="")
    if (.not. (s1 == ch1)) error stop "str == char, both zero-length"
    if (s1 /= ch1) error stop "str /= char, both zero-length"
    if (.not. (ch1 == s1)) error stop "char == str, both zero-length"
    if (ch1 /= s1) error stop "char /= str, both zero-length"

    ! non-zero str vs. non-zero str of equal length
    s1 = CHAR_VALUE1
    s2 = CHAR_VALUE1
    if (.not. (s1 == s2)) &
        error stop "str == str, both equal, both initialized, both non-zero"
    if (s1 /= s2) &
        error stop "str /= str, both equal, both initialized, both non-zero"

    ! non-zero vs. zero-length str, both initialized
    s1 = CHAR_VALUE1
    s2 = ""
    if (s1 == s2) error stop "str == str, both initialized, rhs zero-length"
    if (s2 == s1) error stop "str == str, both initialized, lhs zero-length"
    if (.not. (s1 /= s2)) &
        error stop "str /= str, both initialized, rhs zero-length"
    if (.not. (s2 /= s1)) &
        error stop "str /= str, both initialized, lhs zero-length"

    ! non-zero str vs. non-zero char, equal values
    s1 = CHAR_VALUE1
    if (.not. (s1 == CHAR_VALUE1)) &
        error stop "str == char, both equal, both non-zero"
    if (.not. (CHAR_VALUE1 == s1)) &
        error stop "char == str, both equal, both non-zero"
    if (s1 /= CHAR_VALUE1) error stop "str /= char, both equal, both non-zero"
    if (CHAR_VALUE1 /= s1) error stop "char /= str, both equal, both non-zero"

end subroutine

subroutine test_substring ()
    !* Check substrings with positive and negative indices.

    type (str) :: s1, s2, s3
    integer :: ito, ifrom

    ! not initialized string, not initialized substring
    s2 = s1%substring (1, 1)
    if (.not. (len(s2) == 0)) &
        error stop "str::substring(1, 1), not initialized str"

    ! zero-length substring
    s1 = ""
    s3 = s1%substring (1, 1)
    if (.not. (s3 == "" .and. len(s3) == 0)) &
        error stop "str::substring(1, 1), zero-length str"

    ! regular use of substring
    s1 = CHAR_VALUE1
    ifrom = 1
    ito = int(len(CHAR_VALUE1) / 2)
    s2 = CHAR_VALUE1(ifrom:ito)
    s3 = s1%substring (ifrom, ito)
    if (.not. (s3 == s2)) error stop "str::substring(n, m)"

    ! substring with negative indices
    s1 = CHAR_VALUE1
    ifrom = 1
    ito = len(CHAR_VALUE1)
    s2 = CHAR_VALUE1(ifrom:ito)
    s3 = s1%substring (1, -1)
    if (.not. (s3 == s2)) error stop "str::substring(n, -1)"

    s1 = CHAR_VALUE1
    s2 = CHAR_VALUE1(len(CHAR_VALUE1) - 1:len(CHAR_VALUE1))
    s3 = s1%substring (-2, -1)
    if (.not. (s2 == s3)) error stop "str::substring(-2, -1)"

end subroutine

subroutine test_concat ()
    !* Check concatenation with initialized, empty, and uninitialized strings.

    type (str) :: s1, s2, s3, s99
    character (len=:), allocatable :: ch1

    ! concatenate using //
    s2 = s1 // CHAR_VALUE2
    if (.not. (s2 == CHAR_VALUE2)) &
        error stop "str // character, LHS not initialized"

    s2 = CHAR_VALUE2 // s1
    if (.not. (s2 == CHAR_VALUE2)) &
        error stop "character // str, RHS not initialized"

    ! use CHARACTER as lhs operand
    s1 = CHAR_VALUE1
    s2 = CHAR_VALUE2 // s1
    ch1 = CHAR_VALUE2 // CHAR_VALUE1
    if (.not. (s2 == ch1)) error stop "character // str"

    ! concatenate non-initialized string
    s1 = CHAR_VALUE1
    s2 = ""
    s2 = s1 // s99
    if (.not. (s2 == s1)) error stop "str // str, rhs not initialized"

    s2 = ""
    s2 = s99 // s1
    if (.not. (s2 == s1)) error stop "str // str, lhs not initialized"

    ! concatenate zero-length string
    s1 = CHAR_VALUE1
    s2 = ""
    s3 = ""
    s3 = s1 // s2
    if (.not. (s3 == s1)) error stop "str // str, rhs zero-length"

    s3 = ""
    s3 = s2 // s1
    if (.not. (s3 == s1)) error stop "str // str, lhs zero-length"

end subroutine

subroutine test_join ()
    !* Check joining strings with different separators and empty arguments.

    type (str) :: s1, s2, s3, s4, s5, s99
    integer :: n
    character (len=:), allocatable :: ch1
    character (len=*), parameter :: sep1 = ",", sep2 = ""

    ! test joining multiple str instances
    s1 = CHAR_VALUE1
    s2 = CHAR_VALUE2
    s3 = CHAR_VALUE3
    ! separator
    s4 = sep1
    ! result string
    s5 = s4%join([s1, s2, s3])

    ! manually compute character result
    n = len(CHAR_VALUE1) + len(CHAR_VALUE2) + len(CHAR_VALUE3) + 2 * len(s4)
    allocate (character (len=n) :: ch1)

    ch1 = CHAR_VALUE1 // sep1 // CHAR_VALUE2 // sep1 // CHAR_VALUE3
    if (.not. (s5 == ch1 .and. len(ch1) == len(s5))) &
        error stop "str::join(), 3 arguments"

    ! test joining with a non-initialized separator string
    s5 = s99%join([s1, s2, s3])

    n = len(CHAR_VALUE1) + len(CHAR_VALUE2) + len(CHAR_VALUE3)
    if (allocated(ch1)) deallocate (ch1)
    allocate (character (len=n) :: ch1)
    ch1 = CHAR_VALUE1 // CHAR_VALUE2 // CHAR_VALUE3

    if (.not. (s5 == ch1 .and. len(ch1) == len(s5))) &
        error stop "str::join(), 3 arguments (not initialized separator)"

    ! test with empty string separator
    s4 = sep2
    s5 = s4%join([s1, s2, s3])

    if (.not. (s5 == ch1 .and. len(ch1) == len(s5))) &
        error stop "str::join(), 3 arguments (zero-length separator)"

    ! test join with not initialized string argument, empty string sep.
    s4 = sep2
    s5 = s4%join ([s1, s2, s3, s99])

    n = len(CHAR_VALUE1) + len(CHAR_VALUE2) + len(CHAR_VALUE3)
    if (allocated(ch1)) deallocate (ch1)
    allocate (character (len=n) :: ch1)
    ch1 = CHAR_VALUE1 // CHAR_VALUE2 // CHAR_VALUE3

    if (.not. (s5 == ch1 .and. len(ch1) == len(s5))) &
        error stop "str::join(), 4 arguments (not initialized argument)"

    ! test with a single argument
    s4 = sep1
    s5 = s4%join ([s1])

    if (.not. (s5 == s1 .and. len(s5) == len(s1))) &
        error stop "str::join(), 1 argument"

    ! test with non-empty separator and empty components
    s4 = sep1
    s5 = s4%join([s1, s99])
    n = len(s1) + len(s4)
    if (.not. (s5 == s1 // s4 .and. len(s5) == n)) &
        error stop "str::join(), 2 arguments, one not initialized"

    ! test with only zero-length arguments
    s4 = sep1
    s5 = s4%join([s99, s99])
    n = len(s4)
    if (.not. (s5 == s4 .and. len(s5) == n)) &
        error stop "str::join(), 2 arguments, both not initialized"

    ! test with all non-initialized sep. and arguments
    s5 = s99%join([s99, s99, s99])
    if (.not. (s5 == "" .and. len(s5) == 0)) &
        error stop "str::join(), zero-length separator and arguments"

    ! test with multi-character separator
    s4 = " - "
    s5 = s4%join([s1, s2, s3])
    if (.not. (s5 == (s1 // s4 // s2 // s4 // s3))) &
        error stop "str::join(), multi-character separator"
end subroutine

subroutine test_split ()
    !* Check splitting strings with explicit and default separators.

    type (str) :: s1, s2, s3, s4, sep
    type (status_t) :: status
    type (str), dimension(:), allocatable :: ilist, olist

    s1 = "foo"
    s2 = "bar"
    s3 = "baz"

    ! non-pathological string: verify that split() is the inverse
    ! operation of join()
    allocate (ilist(3), source=[s1, s2, s3])
    ! ilist = [s1, s2, s3]
    sep = ","
    s4 = sep%join (ilist)

    call s4%split (olist, sep, status=status)
    if (.not. (all(ilist == olist))) &
        error stop "str::split(), non-empty sep, non-empty str"
    deallocate (ilist)

    ! string that will generate empty substrings
    allocate (ilist(5), source=[str(''), s1, s2, s3, str('')])
    sep = ","
    s4 = sep%join (ilist)

    call s4%split (olist, sep, status=status)
    if (.not. (all(ilist == olist))) &
        error stop "str::split(), non-empty sep, non-empty str, empty substrings"
    deallocate (ilist)

    ! Pathological cases:
    ! Split empty string, non-empty separator
    s4 = ''
    call s4%split (olist, sep, status=status)
    if (.not. (size(olist) == 1 .and. olist(1) == '')) &
        error stop "str::split(), empty str"

    ! Split empty string, non-empty separator, drop empty substrings
    s4 = ''
    call s4%split (olist, sep, drop_empty=.true., status=status)
    if (.not. (size(olist) == 0)) &
        error stop "str::split(drop_empty=.true.), empty str"

    ! Empty separator
    deallocate (olist)
    allocate (ilist(3), source=[s1, s2, s3])
    s4 = sep%join (ilist)
    sep = ''
    call s4%split (olist, sep, status=status)
    if (.not. (status == FC_STATUS_VALUE_ERROR)) &
        error stop "str::split(), empty separator"
    deallocate (ilist)

    ! Omitted sep argument
    allocate (ilist(3), source=[s1, s2, s3])
    s4 = "   " // s1 // "    " // s2 // " " // s3 // "  "
    call s4%split (olist, status=status)
    if (.not. (all(ilist == olist))) &
        error stop "str::split(), omitted separator argument"
end subroutine

subroutine test_repeat ()
    !* Check repeating initialized, empty, and uninitialized strings.

    type (str) :: s1, s2, s99
    character (len=:), allocatable :: ch1
    integer :: n

    ! repeat not initialized str
    s2 = repeat(s99, 0)
    if (.not. (s2 == "")) error stop "str == repeat(str,0), rhs not initialized"

    ! repeat zero-length str 0 times
    s1 = ""
    s2 = repeat(s1, 0)
    if (.not. (s2 == "")) error stop "str == repeat(str,0), rhs zero-length"

    ! repeat str 0 times
    s1 = CHAR_VALUE1
    s2 = repeat(s1, 0)
    if (.not. (s2 == "")) error stop "str == repeat(str, 0)"

    ! repeat once
    s1 = CHAR_VALUE1
    s2 = repeat(s1, 1)
    if (.not. (s2 == s1)) error stop "str = repeat(str, 1)"

    ! repeat n times
    n = 7
    s1 = CHAR_VALUE1
    s2 = repeat(s1, n)
    ch1 = repeat(CHAR_VALUE1, n)
    if (.not. (s2 == ch1)) error stop "str = repeat(str, n)"

end subroutine

subroutine test_startswith ()
    !* Check prefix matching with string and character arguments.

    type (str) :: s1, s2, s99
    logical :: res

    ! str argument
    s1 = CHAR_VALUE1
    s2 = CHAR_VALUE1(:len(CHAR_VALUE1)-1)
    res = s1%startswith (s2)
    if (.not. res) &
        error stop "str::startswith(str): regular obj and arg, obj(1:n) == arg"

    res = s2%startswith (s1)
    if (res) &
        error stop "str::startswith(str): regular obj and arg, len(arg)>len(obj)"

    ! obj and arg are identical
    res = s1%startswith (s1)
    if (.not. res) error stop "str::startswith(str): obj == arg"

    ! test char argument
    res = s1%startswith (CHAR_VALUE1(:2))
    if (.not. res) error stop "str::startswith(character)"

    ! === Pathological string objects and arguments ===
    ! zero-length argument; treat this case as .TRUE. since
    ! for character char(1:0) == "" is also true
    s2 = ""
    res = s1%startswith (s2)
    if (.not. res) error stop "str::startswith(str): zero-length arg"

    ! Zero-length argument, zero-length object
    s1 = ""
    res = s1%startswith (s2)
    if (.not. res) error stop "str::startswith(str): zero-length obj and arg"

    ! Uninitialized object, zero-length argument
    res = s99%startswith (s1)
    if (.not. res) &
        error stop "str::startswith(str): uninit. obj, zero-length arg"

    ! Uninitialized object, regular argument
    s1 = CHAR_VALUE1
    res = s99%startswith (s1)
    if (res) error stop "str::startswith(str): uninit. obj, regular arg"

    ! Regular object, uninit. argument
    res = s1%startswith (s99)
    if (.not. res) error stop "str::startswith(str): regular obj, uninit. arg"

    ! Uninitialized object and argument
    res = s99%startswith (s99)
    if (.not. res) error stop "str::startswith(str): uninit. obj and arg"
end subroutine

subroutine test_endswith ()
    !* Check suffix matching with string and character arguments.

    type (str) :: s1, s2, s99
    logical :: res

    s1 = CHAR_VALUE1
    s2 = CHAR_VALUE1(2:len(CHAR_VALUE1))

    res = s1%endswith (s2)
    if (.not. res) &
        error stop "str::endswith(str): regular obj and arg, obj(n:-1) == arg"

    res = s2%endswith (s1)
    if (res) &
        error stop "str::endswith(str): regular obj and arg, len(arg)>len(obj)"

    ! obj and arg are identical
    res = s1%endswith (s1)
    if (.not. res) error stop "str::endswith(str): obj == arg"

    ! test char argument
    res = s1%endswith (CHAR_VALUE1(2:))
    if (.not. res) error stop "str::endswith(character)"

    ! === Pathological string objects and arguments ===
    ! zero-length argument; treat this case as .TRUE. since
    ! for character char(1:0) == "" is also true
    s2 = ""
    res = s1%endswith (s2)
    if (.not. res) error stop "str::endswith(str): zero-length arg"

    ! Zero-length argument, zero-length object
    s1 = ""
    res = s1%endswith (s2)
    if (.not. res) error stop "str::endswith(str): zero-length obj and arg"

    ! Uninitialized object, zero-length argument
    res = s99%endswith (s1)
    if (.not. res) error stop "str::endswith(str): uninit. obj, zero-length arg"

    ! Uninitialized object, regular argument
    s1 = CHAR_VALUE1
    res = s99%endswith (s1)
    if (res) error stop "str::endswith(str): uninit. obj, regular arg"

    ! Regular object, uninit. argument
    res = s1%endswith (s99)
    if (.not. res) error stop "str::endswith(str): regular obj, uninit. arg"

    ! Uninitialized object and argument
    res = s99%endswith (s99)
    if (.not. res) error stop "str::endswith(str): uninit. obj and arg"
end subroutine

subroutine test_parse ()
    !* Check parsing strings into numeric, character, string, and logical values.

    character (len=10) :: c1
    type (str) :: s1, s2, s3
    integer (int32) :: i1, i2
    integer (int64) :: i3, i4
    real (real32) :: r1, r2
    real (real64) :: r3, r4
    logical :: l1, l2

    type (status_t) :: status

    i1 = 123
    s1 = str(i1)
    call s1%parse (i2, status)
    if (.not. (i1 == i2 .and. status == FC_STATUS_OK)) &
        error stop "str::parse(int32)"

    i3 = 1234567890
    s1 = str(i3)
    call s1%parse (i4, status)
    if (.not. (i3 == i4 .and. status == FC_STATUS_OK)) &
        error stop "str::parse(int64)"

    r1 = 123.123
    s1 = str(r1)
    call s1%parse (r2, status)
    if (.not. (r1 == r2 .and. status == FC_STATUS_OK)) &
        error stop "str::parse(real32)"

    r3 = 123.123456789_real64
    s1 = str(r3)
    call s1%parse (r4, status)
    if (.not. (r3 == r4 .and. status == FC_STATUS_OK)) &
        error stop "str::parse(real64)"

    s1 = "123"
    call s1%parse (s2, status)
    if (.not. (s1 == s2 .and. status == FC_STATUS_OK)) &
        error stop "str::parse(str)"

    ! try with uninitialized string, should parse to empty string
    call s3%parse (s1, status)
    if (.not. (s1 == "" .and. status == FC_STATUS_OK)) &
        error stop "str::parse(str), uninitialized string"

    s1 = "123"
    call s1%parse (c1, status)
    if (.not. (s1 == trim(c1) .and. status == FC_STATUS_OK)) &
        error stop "str::parse(char)"

    ! try with uninitialized string, should parse to empty string
    call s3%parse (c1, status)
    if (.not. (c1 == "" .and. status == FC_STATUS_OK)) &
        error stop "str::parse(char), uninitialized string"

    ! logical conversion
    l1 = .true.
    s1 = str(l1)
    call s1%parse (l2, status)
    if (.not. (l1 .eqv. l2 .and. status == FC_STATUS_OK)) &
        error stop "str::parse(logical), true input"

    l1 = .false.
    s1 = str(l1)
    call s1%parse (l2, status)
    if (.not. (l1 .eqv. l2 .and. status == FC_STATUS_OK)) &
        error stop "str::parse(logical), false input"

end subroutine

end program
