program test_strings

    use iso_fortran_env
    use corelib_argparse
    use corelib_testing
    use corelib_strings

    implicit none

    call test_all ()

contains

subroutine test_all()

    type (test_suite) :: tests

    call tests%set_label ("corelib_argparse unit tests")

    ! test reading integer arguments
    call test_integer (tests)

    ! run individual test cases
    call test_append (tests)

    ! print test statistics
    call tests%print ()

end subroutine


subroutine test_integer (tests)
    class (test_suite) :: tests
    class (test_case), pointer :: tc

    type (argparser) :: parser
    integer :: status

    ! emulated command line arguments
    type (str), dimension(:), allocatable :: cmd, val_list
    logical :: is_present
    integer (int32), parameter :: input1_32 = 123, input2_32 = -123, input3_32 = huge(1_int32)
    integer (int64), parameter :: input1_64 = 123, input2_64 = -123, input3_64 = huge(1_int64)
    integer (int32) :: val32
    integer (int64) :: val64
    integer :: nvals

    tc => tests%add_test("argparse integer tests")

    ! ===== Scalar 32bit integer =====
    call parser%init ("argparse integer tests")
    call parser%add_argument ("name", default=input1_32, status=status)

    allocate (cmd(1))
    cmd(1) = "--name=" // str(input2_32)

    call parser%parse (cmd, status)
    is_present = parser%is_present ("name")
    call parser%get ("name", val32, status=status)
    call tc%assert_true (is_present .and. val32 == input2_32 .and. status == STATUS_OK, &
        "Scalar 32bit integer argument, argument present")
    deallocate (cmd)

    ! test with no argument present
    allocate (cmd(0))

    call parser%parse (cmd, status)
    is_present = parser%is_present ("name")
    call parser%get ("name", val32, status=status)
    call tc%assert_true ((.not. is_present) .and. val32 == input1_32 .and. status == STATUS_OK, &
        "Scalar 32bit integer argument, argument NOT present")

    deallocate (cmd)

    ! ===== Scalar 64bit integer =====
    call parser%init ("argparse integer tests")
    call parser%add_argument ("name", default=input1_64, status=status)

    allocate (cmd(1))
    cmd(1) = "--name=" // str(input2_64)

    call parser%parse (cmd, status)
    is_present = parser%is_present ("name")
    call parser%get ("name", val64, status=status)
    call tc%assert_true (is_present .and. val64 == input2_64 .and. status == STATUS_OK, &
        "Scalar 64bit integer argument, argument present")
    deallocate (cmd)

    ! test with no argument present
    allocate (cmd(0))

    call parser%parse (cmd, status)
    is_present = parser%is_present ("name")
    call parser%get ("name", val64, status=status)
    call tc%assert_true ((.not. is_present) .and. val64 == input1_64 .and. status == STATUS_OK, &
        "Scalar 64bit integer argument, argument NOT present")
    deallocate (cmd)
    
end subroutine

subroutine test_append (tests)
    class (test_suite) :: tests
    class (test_case), pointer :: tc

    type (argparser) :: parser
    integer :: status

    type (str) :: name, val
    ! emulated command line arguments
    type (str), dimension(:), allocatable :: cmd, val_list
    integer :: nvals


    tc => tests%add_test("argparse ACTION_APPEND")

    call parser%init ("argparse append test")
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

end program
