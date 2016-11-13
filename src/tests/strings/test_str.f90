program test_strings

    use iso_fortran_env
    use corelib_strings
    use corelib_testing

    implicit none

    ! Some test data used to build strings
    character (len=*), parameter :: CHAR_VALUE1 = "test string"
    character (len=*), parameter :: CHAR_VALUE2 = "foo bar 123"
    character (len=*), parameter :: CHAR_VALUE3 = "The quick brown fox jumps over the lazy dog"

    real (real64), parameter :: REAL_VALUE1 = 123.456
    integer, parameter :: INT_VALUE1 = 123

    call test_all

contains

subroutine test_all()

    type (test_suite) :: tests

    call tests%set_label ("corelib_strings::str unit tests")

    ! run individual test cases
    call test_init (tests)
    call test_len (tests)
    call test_assign (tests)

    call test_equal (tests)

    call test_substring (tests)
    call test_concat (tests)

    call test_join (tests)
    call test_repeat (tests)

    call test_startswith (tests)
    call test_endswith (tests)

    call test_array (tests)

    call test_parse (tests)

    ! print test statistics
    call tests%print ()

end subroutine

subroutine test_init(tests)
    class (test_suite) :: tests
    class (test_case), pointer :: tc

    type (str) :: s1, s2

    tc => tests%add_test("String initialization")

    ! test initialization from character
    s1 = str(CHAR_VALUE1)
    call tc%assert_true ((s1 == CHAR_VALUE1) .and. len(s1) == len(CHAR_VALUE1), &
        "Init from character")

    ! initialization from other string
    s2 = s1
    call tc%assert_true (s1 == s2 .and. len(s1) == len(s2), "Init from str")

    ! initialization from real
    s1 = str (REAL_VALUE1, fmt="(f0.2)")
    call tc%assert_true (s1 == "123.46" .and. len(s1) == 6, &
        "Init from real (real64), F editing")

    ! initialization from integer
    s1 = str (INT_VALUE1)
    call tc%assert_true (s1 == "123" .and. len(s1) == 3, &
        "Init from integer, no editing")

    s1 = str (INT_VALUE1, fmt="i0.6")
    call tc%assert_true (s1 == "000123" .and. len(s1) == 6, &
        "Init from integer, fmt argument w/o parentheses")

end subroutine

! ******************************************************************************
! Tests for string representations of arrays

subroutine test_array (tests)
    class (test_suite) :: tests
    class (test_case), pointer :: tc

    integer (int32), parameter :: INT_VALUE1 = 1, INT_VALUE2 = int(1e6), &
        INT_VALUE3 = 1234567890

    real (real32), parameter :: REAL_VALUE1 = 0.123, REAL_VALUE2 = 1.23e10

    type (str) :: s1, s2

    tc => tests%add_test ("String representations of arrays")

    ! test with empty array
    s1 = str_array([integer (int32) ::])
    call tc%assert_true (s1 == "[]", "Empty int32 array")

    s1 = ""
    s1 = str_array([integer (int64) ::])
    call tc%assert_true (s1 == "[]", "Empty int64 array")

    s1 = ""
    s1 = str_array([real (real32) ::])
    call tc%assert_true (s1 == "[]", "Empty real32 array")

    s1 = ""
    s1 = str_array([real (real64) ::])
    call tc%assert_true (s1 == "[]", "Empty real64 array")

    ! test 1d-arrays with one element
    s1 = ""
    s1 = str_array([INT_VALUE1])
    call tc%assert_true (s1 == "[" // str(INT_VALUE1) // "]", &
        "str_array([" // str(INT_VALUE1) // "]), int32 array")

    s1 = ""
    s1 = str_array([integer (int64) :: INT_VALUE1])
    call tc%assert_true (s1 == "[" // str(INT_VALUE1) // "]", &
        "str_array([" // str(INT_VALUE1) // "]), int64 array")

    s1 = ""
    s1 = str_array([REAL_VALUE1])
    s2 = str(REAL_VALUE1)
    call tc%assert_true (s1 == "[" // s2 // "]", &
        "str_array([" // s2 // "_real32])")

    s1 = ""
    s1 = str_array([real (real64) :: REAL_VALUE1])
    s2 = str(real(REAL_VALUE1, real64))
    call tc%assert_true (s1 == "[" // s2 // "]", &
        "str_array([" // s2 // "_real64])")

end subroutine

subroutine test_len (tests)
    class (test_suite) :: tests
    class (test_case), pointer :: tc

    type (str) :: s1, s2

    tc => tests%add_test ("String length")

    s1 = CHAR_VALUE1
    call tc%assert_true (len(s1) == len(CHAR_VALUE1), "len(str) generic")

    s1 = ""
    call tc%assert_true (len(s1) == 0, "len(str) generic, zero-length string")

    call tc%assert_true (len(s2) == 0, "len(str) generic, uninitialized string")

end subroutine

subroutine test_assign (tests)
    class (test_suite) :: tests
    class (test_case), pointer :: tc

    type (str) :: s1, s2, s99
    type (str), allocatable :: s3
    character (len=:), allocatable :: ch1, ch2
    character (len=0) :: ch3

    tc => tests%add_test ("String assignment")

    s1 = s2
    ! str = str assignment with non-initialized str objects
    call tc%assert_true (s1 == s2, "str = str, not initialized")

    ! str = str assignment
    s1 = CHAR_VALUE1
    s2 = s1
    call tc%assert_true ((s1 == s2) .and. (s1%to_char() == s2%to_char()), &
        "str = str")

    ! str = character assignment
    s1 = CHAR_VALUE1
    call tc%assert_true (s1 == CHAR_VALUE1, "str = char")

    ! str = ""
    if (allocated(s3)) deallocate (s3)
    allocate (s3)
    s3 = ""
    call tc%assert_true (s3 == "" .and. len(s3) == 0, "str = ''")

    ! str = character (zero length)
    if (allocated(s3)) deallocate (s3)
    allocate (s3)
    s3 = ch3
    call tc%assert_true (s3 == "" .and. len(s3) == 0, &
        "str = char, rhs zero-length")

    ! character  = str assignment
    s1 = CHAR_VALUE1
    ! could use autoallocation from Fortran 2003
    if (allocated(ch1)) deallocate (ch1)
    allocate (character (len=len(CHAR_VALUE1)) :: ch1)
    ! erase LHS character
    ch1 = repeat(" ", len(ch1))
    ch1 = s1
    call tc%assert_true (ch1 == s1, "char = str")

    ! character = str, not initialized str
    ch1 = repeat("x", len(ch1))
    allocate (ch2, source=ch1)
    ch1 = s99
    ! Check that assignment did not alter string contents
    call tc%assert_true ((ch1 == ""), "char = str, rhs not initialized")

    ! character = str, zero-length str
    s1 = ""
    if (allocated(ch1)) deallocate (ch1)
    allocate (character (len=5) :: ch1)
    ch1 = s1
    call tc%assert_true (ch1 == repeat(" ", len(ch1)), &
        "char = str, zero-length rhs")

end subroutine

subroutine test_equal (tests)
    class (test_suite) :: tests
    class (test_case), pointer :: tc

    type (str) :: s1, s2, s99
    character (len=:), allocatable :: ch1

    tc => tests%add_test("String equality/inequality operators")

    ! not initialized strings: should be equal
    call tc%assert_true (s1 == s2, "str == str, both not initialized")
    call tc%assert_false (s1 /= s2, "str /= str, both not initialized")

    ! not initialized strings: one operand not initialized; should be not
    ! equal to non-zero string
    s1 = CHAR_VALUE1
    call tc%assert_false (s2 == s1, "str == str, lhs not initialized")
    call tc%assert_false (s1 == s2, "str == str, rhs not initialized")

    call tc%assert_true (s2 /= s1, "str /= str, lhs not initialized")
    call tc%assert_true (s1 /= s2, "str /= str, rhs not initialized")

    ! not initialized vs. zero-length: should be equal
    s1 = ""
    call tc%assert_true (s2 == s1, "str == str, lhs not initialized, rhs zero-length")
    call tc%assert_true (s1 == s2, "str == str, rhs not initialized, lhs zero-length")

    ! char vs. not initialized, zero-length char
    if (allocated(ch1)) deallocate (ch1)
    allocate (ch1, source="")
    call tc%assert_true (s99 == ch1, "str == char, lhs not initialized, rhs zero-length")
    call tc%assert_true (ch1 == s99, "str == str, rhs not initialized, lhs zero-length")

    call tc%assert_false (s99 /= ch1, "str /= char, lhs not initialized, rhs zero-length")
    call tc%assert_false (ch1 /= s99, "str /= str, rhs not initialized, lhs zero-length")

    ! both operands zero-length: should equal
    s1 = ""
    s2 = ""
    call tc%assert_true (s1 == s2, "str == str, both zero-length")
    call tc%assert_false (s1 /= s2, "str /= str, both zero-length")

    ! zero-length str vs. zero-length character
    s1 = ""
    if (allocated(ch1)) deallocate (ch1)
    allocate (ch1, source="")
    call tc%assert_true (s1 == ch1, "str == char, both zero-length")
    call tc%assert_false (s1 /= ch1, "str /= char, both zero-length")
    call tc%assert_true (ch1 == s1, "char == str, both zero-length")
    call tc%assert_false (ch1 /= s1, "char /= str, both zero-length")

    ! non-zero str vs. non-zero str of equal length
    s1 = CHAR_VALUE1
    s2 = CHAR_VALUE1
    call tc%assert_true (s1 == s2, "str == str, both equal, both initialized, both non-zero")
    call tc%assert_false (s1 /= s2, "str /= str, both equal, both initialized, both non-zero")

    ! non-zero vs. zero-length str, both initialized
    s1 = CHAR_VALUE1
    s2 = ""
    call tc%assert_false (s1 == s2, "str == str, both initialized, rhs zero-length")
    call tc%assert_false (s2 == s1, "str == str, both initialized, lhs zero-length")
    call tc%assert_true (s1 /= s2, "str /= str, both initialized, rhs zero-length")
    call tc%assert_true (s2 /= s1, "str /= str, both initialized, lhs zero-length")

    ! non-zero str vs. non-zero char, equal values
    s1 = CHAR_VALUE1
    call tc%assert_true (s1 == CHAR_VALUE1, "str == char, both equal, both non-zero")
    call tc%assert_true (CHAR_VALUE1 == s1, "char == str, both equal, both non-zero")
    call tc%assert_false (s1 /= CHAR_VALUE1, "str /= char, both equal, both non-zero")
    call tc%assert_false (CHAR_VALUE1 /= s1, "char /= str, both equal, both non-zero")


end subroutine

subroutine test_substring (tests)
    class (test_suite) :: tests
    class (test_case), pointer :: tc

    type (str) :: s1, s2, s3
    integer :: ito, ifrom

    tc => tests%add_test("String substring() method")

    ! not initialized string, not initialized substring
    s2 = s1%substring (1, 1)
    call tc%assert_true (len(s2) == 0, "str::substring(1, 1), not initialized str")

    ! zero-length substring
    s1 = ""
    s3 = s1%substring (1, 1)
    call tc%assert_true (s3 == "" .and. len(s3) == 0, "str::substring(1, 1), zero-length str")

    ! regular use of substring
    s1 = CHAR_VALUE1
    ifrom = 1
    ito = int(len(CHAR_VALUE1) / 2)
    s2 = CHAR_VALUE1(ifrom:ito)
    s3 = s1%substring (ifrom, ito)
    call tc%assert_true (s3 == s2, "str::substring(n, m)")

    ! substring with negative indices
    s1 = CHAR_VALUE1
    ifrom = 1
    ito = len(CHAR_VALUE1)
    s2 = CHAR_VALUE1(ifrom:ito)
    s3 = s1%substring (1, -1)
    call tc%assert_true (s3 == s2, "str::substring(n, -1)")

    s1 = CHAR_VALUE1
    s2 = CHAR_VALUE1(len(CHAR_VALUE1) - 1:len(CHAR_VALUE1))
    s3 = s1%substring (-2, -1)
    call tc%assert_true (s2 == s3, "str::substring(-2, -1)")

end subroutine

subroutine test_concat (tests)
    class (test_suite) :: tests
    class (test_case), pointer :: tc

    type (str) :: s1, s2, s3, s99
    character (len=:), allocatable :: ch1

    tc => tests%add_test("String concatenation")

    ! concatenate str and character
    s1 = CHAR_VALUE1
    s2 = s1 + CHAR_VALUE2
    ! manually compute result
    allocate (character (len=len(CHAR_VALUE1) + len(CHAR_VALUE2)) :: ch1)
    ch1 = CHAR_VALUE1 // CHAR_VALUE2
    call tc%assert_true (s2 == ch1, "str + character")

    ! concatenate using //
    s2 = s1 // CHAR_VALUE2
    call tc%assert_true (s2 == ch1, "str // character")

    ! concatenate str + str
    s2 = CHAR_VALUE2
    s3 = s1 + s2
    call tc%assert_true (s3 == ch1, "str + str")

    s3 = s1 // s2
    call tc%assert_true (s3 == ch1, "str // str")

    ! use CHARACTER as lhs operand
    s2 = CHAR_VALUE2 // s1
    ch1 = CHAR_VALUE2 // CHAR_VALUE1
    call tc%assert_true (s2 == ch1, "character // str")

    s2 = CHAR_VALUE2 + s1
    call tc%assert_true (s2 == ch1, "character + str")

    ! concatenate non-initialized string
    s1 = CHAR_VALUE1
    s2 = ""
    s2 = s1 // s99
    call tc%assert_true (s2 == s1, "str // str, rhs not initialized")

    s2 = ""
    s2 = s99 // s1
    call tc%assert_true (s2 == s1, "str // str, lhs not initialized")

    ! concatenate zero-length string
    s1 = CHAR_VALUE1
    s2 = ""
    s3 = ""
    s3 = s1 // s2
    call tc%assert_true (s3 == s1, "str // str, rhs zero-length")

    s3 = ""
    s3 = s2 // s1
    call tc%assert_true (s3 == s1, "str // str, lhs zero-length")

end subroutine

subroutine test_join (tests)
    class (test_suite) :: tests
    class (test_case), pointer :: tc

    type (str) :: s1, s2, s3, s4, s5, s99
    integer :: n
    character (len=:), allocatable :: ch1
    character (len=*), parameter :: sep1 = ",", sep2 = ""

    tc => tests%add_test ("String joining")

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
    call tc%assert_true (s5 == ch1 .and. len(ch1) == len(s5), &
        "str::join(), 3 arguments")

    ! test joining with a non-initialized separator string
    s5 = s99%join([s1, s2, s3])

    n = len(CHAR_VALUE1) + len(CHAR_VALUE2) + len(CHAR_VALUE3)
    if (allocated(ch1)) deallocate (ch1)
    allocate (character (len=n) :: ch1)
    ch1 = CHAR_VALUE1 // CHAR_VALUE2 // CHAR_VALUE3

    call tc%assert_true (s5 == ch1 .and. len(ch1) == len(s5), &
        "str::join(), 3 arguments (not initialized separator)")

    ! test with empty string separator
    s4 = sep2
    s5 = s4%join([s1, s2, s3])

    call tc%assert_true (s5 == ch1 .and. len(ch1) == len(s5), &
        "str::join(), 3 arguments (zero-length separator)")

    ! test join with not initialized string argument, empty string sep.
    s4 = sep2
    s5 = s4%join ([s1, s2, s3, s99])

    n = len(CHAR_VALUE1) + len(CHAR_VALUE2) + len(CHAR_VALUE3)
    if (allocated(ch1)) deallocate (ch1)
    allocate (character (len=n) :: ch1)
    ch1 = CHAR_VALUE1 // CHAR_VALUE2 // CHAR_VALUE3

    call tc%assert_true (s5 == ch1 .and. len(ch1) == len(s5), &
        "str::join(), 4 arguments (not initialized argument)")

    ! test with a single argument
    s4 = sep1
    s5 = s4%join ([s1])

    call tc%assert_true (s5 == s1 .and. len(s5) == len(s1), &
        "str::join(), 1 argument")

    ! test with non-empty separator and empty components
    s4 = sep1
    s5 = s4%join([s1, s99])
    n = len(s1) + len(s4)
    call tc%assert_true (s5 == s1 // s4 .and. len(s5) == n, &
        "str::join(), 2 arguments, one not initialized")

    ! test with only zero-length arguments
    s4 = sep1
    s5 = s4%join([s99, s99])
    n = len(s4)
    call tc%assert_true (s5 == s4 .and. len(s5) == n, &
        "str::join(), 2 arguments, both not initalized")

    ! test with all non-initialized sep. and arguments
    s5 = s99%join([s99, s99, s99])
    call tc%assert_true (s5 == "" .and. len(s5) == 0, &
        "str::join(), zero-length separator and arguments")

    ! test with multi-character separator
    s4 = " - "
    s5 = s4%join([s1, s2, s3])
    call tc%assert_true (s5 == (s1 // s4 // s2 // s4 // s3), &
        "str::join(), multi-character separator")
end subroutine

subroutine test_repeat (tests)
    class (test_suite) :: tests
    class (test_case), pointer :: tc

    type (str) :: s1, s2, s99
    character (len=:), allocatable :: ch1
    integer :: n

    tc => tests%add_test ("String repeat()")

    ! repeat not initialized str
    s2 = s99 * 0
    call tc%assert_true (s2 == "", "str == str * 0, rhs not initialized")

    ! repeat zero-length str 0 times
    s1 = ""
    s2 = s1 * 0
    call tc%assert_true (s2 == "", "str == str * 0, rhs zero-length")

    ! repeat str 0 times
    s1 = CHAR_VALUE1
    s2 = s1 * 0
    call tc%assert_true (s2 == "", "str == str * 0")

    ! repeat once
    s1 = CHAR_VALUE1
    s2 = s1 * 1
    call tc%assert_true (s2 == s1, "str = str * 1")

    ! repeat n times
    n = 7
    s1 = CHAR_VALUE1
    s2 = s1 * n
    ch1 = repeat(CHAR_VALUE1, n)
    call tc%assert_true (s2 == ch1, "str = str * n")

    ! repeat() generic for str
    s1 = CHAR_VALUE1
    n = 10
    s2 = repeat(s1, n)
    if (allocated(ch1)) deallocate (ch1)
    ch1 = repeat(CHAR_VALUE1, n)
    call tc%assert_true (s2 == ch1, "str = repeat(str, n)")

end subroutine

subroutine test_startswith (tests)
    class (test_suite) :: tests
    class (test_case), pointer :: tc

    type (str) :: s1, s2, s99

    tc => tests%add_test ("String startswith() method")

    ! str argument
    s1 = CHAR_VALUE1
    s2 = CHAR_VALUE1(:len(CHAR_VALUE1)-1)
    call tc%assert_true (s1%startswith (s2), &
        "str::startswith(str) evaluating to true")

    call tc%assert_false (s2%startswith (s1), &
        "str::startswith(str), longer str. argument")

    ! test char argument
    call tc%assert_true (s1%startswith (CHAR_VALUE1(:2)), &
        "str::startswith(character)")

    ! test zero-length string; treat this case as .TRUE. since
    ! for character char(1:0) == "" is also true
    s2 = ""
    call tc%assert_true (s1%startswith (s2), &
        "str::startswith(str), zero-length argument")

    ! test not initialized argument
    call tc%assert_false (s1%endswith (s99), &
        "str::startswith(str), not initialized argument")
end subroutine

subroutine test_endswith (tests)
    class (test_suite) :: tests
    class (test_case), pointer :: tc

    type (str) :: s1, s2, s99

    tc => tests%add_test ("String endswith() method")

    ! str argument
    s1 = CHAR_VALUE1
    s2 = CHAR_VALUE1(len(CHAR_VALUE1)-1:)
    call tc%assert_true (s1%endswith (s2), &
        "str::endswith(str) evaluating to true")

    call tc%assert_false (s2%endswith (s1), &
        "str::endswith(str), longer str. argument")

    ! test char argument
    call tc%assert_true (s1%endswith (CHAR_VALUE1(3:)), &
        "str::endswith(character)")

    ! test zero-length string; treat this case as .TRUE. since
    ! for character char(1:0) == "" is also true
    s2 = ""
    call tc%assert_true (s1%endswith (s2), &
        "str::endswith(str), zero-length argument")

    ! test not initialized argument
    call tc%assert_false (s1%endswith (s99), &
        "str::endswith(str), not initialized argument")
end subroutine

subroutine test_parse (tests)
    class (test_suite) :: tests
    class (test_case), pointer :: tc

    character (len=10) :: c1
    type (str) :: s1, s2, s3
    integer (int32) :: i1, i2
    integer (int64) :: i3, i4
    real (real32) :: r1, r2
    real (real64) :: r3, r4
    logical :: l1, l2

    integer :: status

    tc => tests%add_test ("String parse() method")

    i1 = 123
    s1 = str(i1)
    call s1%parse (i2, status)
    call tc%assert_true (i1 == i2 .and. status == STR_PARSE_SUCCESS, &
        "str::parse(int32)")

    i3 = 1234567890
    s1 = str(i3)
    call s1%parse (i4, status)
    call tc%assert_true (i3 == i4 .and. status == STR_PARSE_SUCCESS, &
        "str::parse(int64)")

    r1 = 123.123
    s1 = str(r1)
    call s1%parse (r2, status)
    call tc%assert_true (r1 == r2 .and. status == STR_PARSE_SUCCESS, &
        "str::parse(real32)")

    r3 = 123.123456789_real64
    s1 = str(r3)
    call s1%parse (r4, status)
    call tc%assert_true (r3 == r4 .and. status == STR_PARSE_SUCCESS, &
        "str::parse(real64)")

    s1 = "123"
    call s1%parse (s2, status)
    call tc%assert_true (s1 == s2 .and. status == STR_PARSE_SUCCESS, &
        "str::parse(str)")

    ! try with uninitialized string, should parse to empty string
    call s3%parse (s1, status)
    call tc%assert_true (s1 == "" .and. status == STR_PARSE_SUCCESS, &
        "str::parse(str), uninitialized string")

    s1 = "123"
    call s1%parse (c1, status)
    call tc%assert_true (s1 == trim(c1) .and. status == STR_PARSE_SUCCESS, &
        "str::parse(char)")

    ! try with uninitialized string, should parse to empty string
    call s3%parse (c1, status)
    call tc%assert_true (c1 == "" .and. status == STR_PARSE_SUCCESS, &
        "str::parse(char), uninitialized string")

    ! logical conversion
    l1 = .true.
    s1 = str(l1)
    call s1%parse (l2, status)
    call tc%assert_true (l1 .eqv. l2 .and. status == STR_PARSE_SUCCESS, &
        "str::parse(logical), true input")

    l1 = .false.
    s1 = str(l1)
    call s1%parse (l2, status)
    call tc%assert_true (l1 .eqv. l2 .and. status == STR_PARSE_SUCCESS, &
        "str::parse(logical), false input")


end subroutine


end program
