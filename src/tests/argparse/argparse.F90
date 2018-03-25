
program test_strings

    use iso_fortran_env
    use fcore_argparse
    use fcore_testing

    implicit none

    call test_all ()

contains

subroutine test_all()

    type (test_suite) :: tests

    call tests%set_label ("fcore_argparse unit tests")

    ! test reading integer arguments
    call test_integer (tests)

    ! Test STORE_TRUE action
    call test_store_true (tests)

    ! Test STORE_FALSE action
    call test_store_false (tests)

    ! Test TOGGLE action
    call test_toggle (tests)

#ifndef __FCORE_GFORTRAN_POLY_ARRAY_BUG
    ! run individual test cases
    call test_append (tests)
#endif

#ifndef __FCORE_GFORTRAN_POLY_ARRAY_BUG
    ! Test handling of cmd. arguments that are not mapped to any argument object
    call test_unmapped (tests)
#endif

    call test_validators (tests)

    call test_help (tests)

    call test_copy (tests)

    ! print test statistics
    call tests%print ()

end subroutine


subroutine test_integer (tests)
    class (test_suite) :: tests
    class (test_case), pointer :: tc

    type (argparser) :: parser
    type (status_t) :: status

    ! emulated command line arguments
    type (str), dimension(:), allocatable :: cmd
    logical :: is_present
    integer (int32), parameter :: input1_32 = 123, input2_32 = -123
    integer (int64), parameter :: input1_64 = 123, input2_64 = -123
    integer (int32) :: val32
    integer (int64) :: val64

    tc => tests%add_test("argparse integer tests")

    ! ===== Scalar 32bit integer =====
    call parser%init (description="argparse integer tests")
    call parser%add_argument ("name", default=input1_32, status=status)

    allocate (cmd(1))
    cmd(1) = "--name=" // str(input2_32)

    call parser%parse (cmd, status)
    is_present = parser%is_present ("name")
    call parser%get ("name", val32, status=status)
    call tc%assert_true (is_present .and. val32 == input2_32 .and. status == FC_STATUS_OK, &
        "Scalar 32bit integer argument, argument present")
    deallocate (cmd)

    ! test with no argument present
    allocate (cmd(0))

    call parser%parse (cmd, status)
    is_present = parser%is_present ("name")
    call parser%get ("name", val32, status=status)
    call tc%assert_true ((.not. is_present) .and. val32 == input1_32 .and. status == FC_STATUS_OK, &
        "Scalar 32bit integer argument, argument NOT present")

    deallocate (cmd)

    ! ===== Scalar 64bit integer =====
    call parser%init (description="argparse integer tests")
    call parser%add_argument ("name", default=input1_64, status=status)

    allocate (cmd(1))
    cmd(1) = "--name=" // str(input2_64)

    call parser%parse (cmd, status)
    is_present = parser%is_present ("name")
    call parser%get ("name", val64, status=status)
    call tc%assert_true (is_present .and. val64 == input2_64 .and. status == FC_STATUS_OK, &
        "Scalar 64bit integer argument, argument present")
    deallocate (cmd)

    ! test with no argument present
    allocate (cmd(0))

    call parser%parse (cmd, status)
    is_present = parser%is_present ("name")
    call parser%get ("name", val64, status=status)
    call tc%assert_true ((.not. is_present) .and. val64 == input1_64 .and. status == FC_STATUS_OK, &
        "Scalar 64bit integer argument, argument NOT present")
    deallocate (cmd)

end subroutine


#ifndef __FCORE_GFORTRAN_POLY_ARRAY_BUG
subroutine test_append (tests)
    class (test_suite) :: tests
    class (test_case), pointer :: tc

    type (argparser) :: parser
    type (status_t) :: status

    type (str) :: val
    ! emulated command line arguments
    type (str), dimension(:), allocatable :: cmd, val_list
    integer :: nvals


    tc => tests%add_test("argparse ACTION_APPEND")

    call parser%init (description="argparse append test")
    call parser%add_argument ("name", "n", action=ARGPARSE_ACTION_APPEND, status=status)

    ! --------------------------------------------------------------------------
    ! long syntax, single argument
    allocate (cmd(1))
    cmd(1) = "--name=foo"

    call parser%parse (cmd, status)

    call tc%assert_true (parser%is_present ("name"), &
        "ACTION_APPEND: argument present, nargs=1")

    call parser%get ("name", val)
    call tc%assert_true (val == "foo", &
        "ACTION_APPEND: correct argument value, nargs=1")

    nvals = parser%get_nvals ("name")
    call tc%assert_true (nvals == 1, &
        "ACTION_APPEND: corrent # of arguments reported, nargs=1")

    deallocate (cmd)

    ! --------------------------------------------------------------------------
    ! test with multiple arguments
    allocate(cmd(1))
    cmd(1) = "--name=foo,bar"

    call parser%parse (cmd, status)

    call tc%assert_true (parser%is_present ("name"), &
        "ACTION_APPEND: argument present, nargs=2")

    nvals = parser%get_nvals ("name")
    call tc%assert_true (nvals == 2, &
        "ACTION_APPEND: corrent # of arguments reported, nargs=2")

    allocate (val_list(nvals))
    call parser%get ("name", val_list)
    call tc%assert_true (val_list(1) == "foo" .and. val_list(2) == "bar", &
        "ACTION_APPEND: correct argument value, nargs=2")

    deallocate (cmd, val_list)

    ! --------------------------------------------------------------------------
    ! test short argument syntax -- single instance
    allocate (cmd(2))
    cmd(1) = "-n"; cmd(2) = "foo"

    call parser%parse (cmd, status)

    call tc%assert_true (parser%is_present ("name"), &
        "abbrev. syntax: argument present, nargs=1")

    nvals = parser%get_nvals ("name")
    call tc%assert_true (nvals == 1, &
        "abbrev. syntax: corrent # of arguments reported, nargs=1")

    allocate (val_list(nvals))
    call parser%get ("name", val_list)
    call tc%assert_true (val_list(1) == "foo", &
        "abbrev. syntax: correct argument value, nargs=1")

    deallocate (cmd, val_list)

    ! --------------------------------------------------------------------------
    ! test short syntax -- multiple instances
    allocate (cmd(4))
    cmd(1) = "-n"; cmd(2) = "foo"; cmd(3) = "-n"; cmd(4) = "bar"

    call parser%parse (cmd, status)

    call tc%assert_true (parser%is_present ("name"), &
        "abbrev. syntax: argument present, nargs=2")

    nvals = parser%get_nvals ("name")
    call tc%assert_true (nvals == 2, &
        "abbrev. syntax: corrent # of arguments reported, nargs=2")

    allocate (val_list(nvals))
    call parser%get ("name", val_list)
    call tc%assert_true (all(val_list == ["foo", "bar"]), &
        "abbrev. syntax: correct argument value, nargs=2")

    deallocate (cmd, val_list)

    ! --------------------------------------------------------------------------
    ! test short syntax -- single instances, no tokenization of value list
    allocate (cmd(2))
    cmd(1) = "-n"; cmd(2) = "foo,bar"

    call parser%parse (cmd, status)

    call tc%assert_true (parser%is_present ("name"), &
        "abbrev. syntax: argument present (value list, nargs=1)")

    nvals = parser%get_nvals ("name")
    call tc%assert_true (nvals == 1, &
        "abbrev. syntax: corrent # of arguments (value list, nargs=1)")

    allocate (val_list(nvals))
    call parser%get ("name", val_list)
    call tc%assert_true (all(val_list == ["foo,bar"]), &
        "abbrev. syntax: correct argument value (value list, nargs=1)")

    deallocate (cmd, val_list)

end subroutine
#endif


subroutine test_validators (tests)
    class (test_suite) :: tests
    class (test_case), pointer :: tc

    type (argparser) :: parser
    type (status_t) :: status

    integer :: ival
    ! emulated command line arguments
    type (str), dimension(:), allocatable :: cmd

    tc => tests%add_test("argparse validators")

    call parser%init (description="Validators test routine")

    allocate (cmd(1))

    call parser%add_argument ("pos-int", validator=validate_pos_int32)
    call parser%add_argument ("nonneg", validator=validate_nonneg_int32)
    call parser%add_argument ("nonempty-str", validator=validate_nonempty_str)

    ! === Test positive int validator ===
    ! Test with argument that can be converted to int but has invalid
    ! int value.
    cmd(1) ='--pos-int=0'
    call parser%parse (cmd, status)
    call tc%assert_true (status == FC_STATUS_VALUE_ERROR, &
        "validate_pos_int32() with invalid integer value")

    ! Test with argument that cannot be converged to int
    cmd(1) = '--pos-int=foobar'
    call parser%parse (cmd, status)
    call tc%assert_true (status == FC_STATUS_VALUE_ERROR, &
        "validate_pos_int32() with string value")

    cmd(1) = '--pos-int=1'
    call parser%parse (cmd, status)
    call parser%get ("pos-int", ival)
    call tc%assert_true (status == FC_STATUS_OK .and. ival == 1, &
        "validate_pos_int32() with valid value")

    ! === Test non-negative int validator ===
    cmd(1) ='--nonneg=-1'
    call parser%parse (cmd, status)
    call tc%assert_true (status == FC_STATUS_VALUE_ERROR, &
        "validate_nonneg_int32() with invalid integer value")

    ! Test with argument that cannot be converged to int
    cmd(1) = '--nonneg=foobar'
    call parser%parse (cmd, status)
    call tc%assert_true (status == FC_STATUS_VALUE_ERROR, &
        "validate_nonneg_int32() with string value")

    cmd(1) = '--nonneg=0'
    call parser%parse (cmd, status)
    call parser%get ("nonneg", ival)
    call tc%assert_true (status == FC_STATUS_OK .and. ival == 0, &
        "validate_nonneg_int32() with valid value")

    ! === Test non-empty string ===
    ! Note: for this to be triggered the user has to supply something like
    !   --nonempty-str ""
    ! If the command line is only
    !   --nonempty-str
    ! the parser will complain by itself, as it expects at least one value
    ! with ACTION_STORE.
    if (allocated(cmd)) deallocate (cmd)
    allocate (cmd(2))
    cmd(1) = '--nonempty-str'
    cmd(2) = ''
    call parser%parse (cmd, status)
    call tc%assert_true (status == FC_STATUS_VALUE_ERROR, &
        "validate_nonempty_str with empty string value")
end subroutine


subroutine test_store_true (tests)
    class (test_suite) :: tests
    class (test_case), pointer :: tc

    type (argparser) :: parser
    type (status_t) :: status
    logical :: val

    type (str), dimension(:), allocatable :: cmd_str

    tc => tests%add_test("argparse STORE_TRUE action")

    call parser%init ()
    call parser%add_argument ("foo", abbrev='f', action=ARGPARSE_ACTION_STORE_TRUE)

    allocate (cmd_str(1))

    ! Long form argument present
    cmd_str(1) = "--foo"
    call parser%parse (cmd_str, status)

    call parser%get ("foo", val)
    call tc%assert_true (status == FC_STATUS_OK .and. val, &
        "Argument present, value = .true. (long form)")

    ! Short form, argument present
    cmd_str(1) = "-f"
    call parser%parse (cmd_str, status)
    call parser%get ("foo", val)
    call tc%assert_true (status == FC_STATUS_OK .and. val, &
        "Argument present, value = .true. (short form)")

    ! Argument not present
    cmd_str(1) = ""
    call parser%parse (cmd_str, status)
    call parser%get ("foo", val)
    call tc%assert_true (status == FC_STATUS_OK .and. .not. val, &
        "Argument NOT present, value = .false. (long form)")

    ! Unexpected argument value
    cmd_str(1) = "--foo=bar"
    call parser%parse (cmd_str, status)
    call tc%assert_true (status == ARGPARSE_STATUS_PARSE_ERROR, &
        "Argument present, unexpected argument value (short form)")

end subroutine


subroutine test_store_false (tests)
    class (test_suite) :: tests
    class (test_case), pointer :: tc

    type (argparser) :: parser
    type (status_t) :: status
    logical :: val

    type (str), dimension(:), allocatable :: cmd_str

    tc => tests%add_test("argparse STORE_FALSE action")

    call parser%init ()
    call parser%add_argument ("foo", abbrev='f', action=ARGPARSE_ACTION_STORE_FALSE)

    allocate (cmd_str(1))

    ! Long form argument present
    cmd_str(1) = "--foo"
    call parser%parse (cmd_str, status)

    call parser%get ("foo", val)
    call tc%assert_true (status == FC_STATUS_OK .and. .not. val, &
        "Argument present, value = .false. (long form)")

    ! Short form, argument present
    cmd_str(1) = "-f"
    call parser%parse (cmd_str, status)
    call parser%get ("foo", val)
    call tc%assert_true (status == FC_STATUS_OK .and. .not. val, &
        "Argument present, value = .false. (short form)")

    ! Argument not present
    cmd_str(1) = ""
    call parser%parse (cmd_str, status)
    call parser%get ("foo", val)
    call tc%assert_true (status == FC_STATUS_OK .and. val, &
        "Argument NOT present, value = .true. (long form)")

    ! Unexpected argument value
    cmd_str(1) = "--foo=bar"
    call parser%parse (cmd_str, status)
    call tc%assert_true (status == ARGPARSE_STATUS_PARSE_ERROR, &
        "Argument present, unexpected argument value (short form)")

end subroutine


subroutine test_toggle (tests)
    class (test_suite) :: tests
    class (test_case), pointer :: tc

    type (argparser) :: parser
    type (status_t) :: status
    logical :: val

    type (str), dimension(:), allocatable :: cmd_str

    tc => tests%add_test ("argparse TOGGLE action")

    call parser%init ()

    ! Test with default settings for TOGGLE action, empty cmd. line
    call parser%add_argument ("foo", action=ARGPARSE_ACTION_TOGGLE)
    allocate (cmd_str(0))
    call parser%parse (cmd_str, status)
    call parser%get ("foo", val)

    call tc%assert_true (val, "Default settings, empty cmd. line")
    deallocate (cmd_str)

    ! Test with default settings, toggle ON
    allocate (cmd_str(1))
    cmd_str(1) = "--foo"
    call parser%parse (cmd_str, status)
    call parser%get ("foo", val)
    call tc%assert_true (val, "Default settings, toggle ON on cmd. line")

    ! Test with default settings, toggle OFF
    cmd_str(1) = "--no-foo"
    call parser%parse (cmd_str, status)
    call parser%get ("foo", val)
    call tc%assert_false (val, "Default settings, toggle OFF on cmd. line")
   
    ! Try to add argument with name --no-foo, this should not be permitted
    call parser%add_argument ("no-foo", status=status)
    call tc%assert_true (status /= FC_STATUS_OK, &
        "Attemping to add --no-foo when --foo defined")

end subroutine


 
#ifndef __FCORE_GFORTRAN_POLY_ARRAY_BUG
subroutine test_unmapped (tests)
    class (test_suite) :: tests
    class (test_case), pointer :: tc

    type (argparser) :: parser
    type (status_t) :: status
    integer :: num

    type (str), dimension(3) :: cmd_unmapped
    type (str), dimension(:), allocatable :: cmd_str, stored_str
    type (str) :: val1, val2

    tc => tests%add_test("argparse unmapped args")

    cmd_unmapped(1) = "foo"
    cmd_unmapped(2) = "bar"
    cmd_unmapped(3) = "lorem ipsum"

    call parser%init ()
    call parser%add_argument ("option1", required=.false.)
    call parser%add_argument ("option2", required=.false.)

    ! Test with no args present
    allocate (cmd_str(0))
    call parser%parse (cmd_str, status)
    num = parser%get_num_unmapped ()
    call tc%assert_true (status == FC_STATUS_OK .and. num == 0, &
        "No args present, assert (num_unmapped == 0)")

    ! Test with no unmapped args, but other args present
    if (allocated(cmd_str)) deallocate (cmd_str)
    allocate (cmd_str(2))
    cmd_str(1) = "--option1"
    cmd_str(2) = "value 1"

    call parser%parse (cmd_str, status)
    num = parser%get_num_unmapped ()
    call tc%assert_true (status == FC_STATUS_OK .and. num == 0, &
        "No unmapped but other args. present, assert (num_unmapped == 0)")

    ! Test with only unmapped args, one empty unmapped arg present
    if (allocated(cmd_str)) deallocate (cmd_str)
    allocate (cmd_str(1))
    cmd_str(1) = ""
    call parser%parse (cmd_str, status)
    num = parser%get_num_unmapped ()
    call parser%get_unmapped (val1)
    call tc%assert_true (status == FC_STATUS_OK .and. num == 1 &
        .and. val1 == "", &
        "One empty unmapped arg. present, assert (num_unmapped == 1)")

    ! Test mix of mappend and unmapped values at various positions
    if (allocated(cmd_str)) deallocate (cmd_str)
    allocate (cmd_str(6))
    cmd_str(1) = cmd_unmapped(1)
    cmd_str(2) = "--option1"
    cmd_str(3) = "baz"
    cmd_str(4) = cmd_unmapped(2)
    cmd_str(5) = "--option2=bar"
    cmd_str(6) = cmd_unmapped(3)

    call parser%parse (cmd_str, status)

    call parser%get ("option1", val1, status)
    call tc%assert_true (status == FC_STATUS_OK .and. val1 == "baz", &
        "Multiple interleaved mapped/unmapped args, mapped arg 1 OK")

    call parser%get ("option2", val2, status)
    call tc%assert_true (status == FC_STATUS_OK .and. val2 == "bar", &
        "Multiple interleaved mapped/unmapped args, mapped arg 2 OK")

    if (allocated(stored_str)) deallocate (stored_str)
    allocate (stored_str(size(cmd_unmapped)))
    num = parser%get_num_unmapped ()
    call parser%get_unmapped (stored_str, status)
    call tc%assert_true (status == FC_STATUS_OK .and. num == size(cmd_unmapped) &
        .and. all(stored_str == cmd_unmapped), &
        "Multiple interleaved mapped/unmapped args; unmapped args OK")

    ! Attempting to retrieve more or fewer unmapped values than present
    if (allocated(stored_str)) deallocate (stored_str)
    allocate (stored_str(size(cmd_unmapped) - 1))
    call parser%get_unmapped (stored_str, status)
    call tc%assert_true (status == FC_STATUS_VALUE_ERROR, &
        "Attempt to retrieve fewer unmapped values than present")

    ! Store into array that is larger than number of unmapped args:
    ! This is OK.
    if (allocated(stored_str)) deallocate (stored_str)
    allocate (stored_str(size(cmd_unmapped) + 1))
    call parser%get_unmapped (stored_str, status)
    num = parser%get_num_unmapped ()
    call tc%assert_true (status == FC_STATUS_OK .and. &
        all(cmd_unmapped == stored_str(1:num)), &
        "Attempt to retrieve more unmapped values than present")

    ! Test with no unmapped args. present, but trying to retrieve one as scalar
    if (allocated(cmd_str)) deallocate (cmd_str)
    allocate (cmd_str(0))
    call parser%parse (cmd_str, status)
    call parser%get_unmapped (val1, status)
    call tc%assert_true (status == FC_STATUS_INVALID_STATE, &
        "Attempt to retrieve scalar unmapped arg if none present")

end subroutine
#endif


subroutine test_help (tests)
    class (test_suite) :: tests
    class (test_case), pointer :: tc

    type (argparser) :: parser
    type (status_t) :: status
    type (str), dimension(:), allocatable :: cmd_args

    tc => tests%add_test("argparse HELP argument")

    call parser%init (description="argparse HELP test")

    ! Test if --help is the only argument
    allocate (cmd_args(1))
    cmd_args(1) = "--help"
    call parser%parse (cmd_args, status)
    call tc%assert_true (status == ARGPARSE_STATUS_HELP_PRESENT, &
        "Single argument --help")

    cmd_args(1) = "-h"
    call parser%parse (cmd_args, status)
    call tc%assert_true (status == ARGPARSE_STATUS_HELP_PRESENT, &
        "Single argument -h")
    deallocate (cmd_args)

    ! Test with  3 arguments with --help at various positions;
    ! Other arguments are not defined ex ante, as help should work regardless.
    allocate (cmd_args(3))
    cmd_args(1) = "--help"
    cmd_args(2) = "--foo"
    cmd_args(3) = "--bar"
    call parser%parse (cmd_args, status)
    call tc%assert_true (status == ARGPARSE_STATUS_HELP_PRESENT, &
        "Three arguments, --help in position 1")

    cmd_args(1) = "--foo"
    cmd_args(2) = "--help"
    call parser%parse (cmd_args, status)
    call tc%assert_true (status == ARGPARSE_STATUS_HELP_PRESENT, &
        "Three arguments, --help in position 2")

    cmd_args(2) = "--bar"
    cmd_args(3) = "--help"
    call parser%parse (cmd_args, status)
    call tc%assert_true (status == ARGPARSE_STATUS_HELP_PRESENT, &
        "Three arguments, --help in position 3")
    deallocate (cmd_args)

    ! Test with args that take user-provided values
    allocate (cmd_args(3))
    cmd_args(1) = "-f"
    cmd_args(2) = "bar"
    cmd_args(3) = "--help"
    call parser%parse (cmd_args, status)
    call tc%assert_true (status == ARGPARSE_STATUS_HELP_PRESENT, &
        "Three arguments incl. user-provided value")
    deallocate (cmd_args)

    ! Test with args. that are defined
    call parser%add_argument ("foo", abbrev="f", action=ARGPARSE_ACTION_STORE, &
        help="Help text for argument foo")
    allocate (cmd_args(3))
    cmd_args(1) = "-f"
    cmd_args(2) = "bar"
    cmd_args(3) = "--help"
    call parser%parse (cmd_args, status)
    call tc%assert_true (status == ARGPARSE_STATUS_HELP_PRESENT, &
        "Three arguments incl. user-provided value; args. defined ex ante")
    deallocate (cmd_args)

end subroutine


subroutine test_copy (tests)
    class (test_suite) :: tests

    class (test_case), pointer :: tc
    type (argparser), allocatable :: parser, parser2
    type (status_t) :: status
    type (str), dimension(:), allocatable :: cmd_args

    tc => tests%add_test ("argparse copy test")

    allocate (parser)

    call parser%init (description="argparse copy test")

    call parser%add_argument ("arg-value", &
        default=1, &
        help="Help text for argument with value", &
        status=status)

    call parser%add_argument ("arg-const", &
        action=ARGPARSE_ACTION_STORE_CONST, &
        default=1, &
        const=2, &
        help="Help text for storing constant")

    call parser%add_argument ("arg-toggle", &
        action=ARGPARSE_ACTION_TOGGLE, &
        default=.true., &
        help="Help text for toggle argument")

    allocate (parser2)
    parser2 = parser

    deallocate (parser)
    deallocate (parser2)

end subroutine

end program
