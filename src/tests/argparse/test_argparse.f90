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

    ! run individual test cases
    call test_append (tests)

    ! print test statistics
    call tests%print ()

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
    logical :: is_present


    tc => tests%add_test("argparse ACTION_APPEND")

    call parser%init ("argparse append test")
    call parser%add_argument ("name", "n", action=ARGPARSE_ACTION_APPEND, status=status)

    allocate (cmd(1))
    cmd(1) = "--name=foo"

    call parser%parse (cmd, status)
    deallocate (cmd)

    call parser%is_present ("name", is_present)
    call tc%assert_true (is_present, &
        "ACTION_APPEND: argument present, nargs=1")

    call parser%get ("name", val)
    call tc%assert_true (val == "foo", &
        "ACTION_APPEND: correct argument value, nargs=1")

    nvals = parser%get_nvals ("name")
    call tc%assert_true (nvals == 1, &
        "ACTION_APPEND: corrent # of arguments reported, nargs=1")

    ! test with multiple arguments
    allocate(cmd(1))
    cmd(1) = "--name=foo,bar"

    call parser%parse (cmd, status)
    deallocate (cmd)

    call parser%is_present ("name", is_present)
    call tc%assert_true (is_present, &
        "ACTION_APPEND: argument present, nargs=2")

    nvals = parser%get_nvals ("name")
    call tc%assert_true (nvals == 2, &
        "ACTION_APPEND: corrent # of arguments reported, nargs=2")

    allocate (val_list(nvals))
    call parser%get ("name", val_list)
    call tc%assert_true (val_list(1) == "foo" .and. val_list(2) == "bar", &
        "ACTION_APPEND: correct argument value, nargs=2")


end subroutine

end program
