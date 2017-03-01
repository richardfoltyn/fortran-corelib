program argparse1

    use corelib_argparse

    implicit none

    type (argparser) :: parser
    type (status_t) :: status

    type (str) :: name, opt
    character (20) :: opt_char, opt_array(2)
    logical :: flag1, flag2
    integer :: const

    call parser%init ("argparse example 1")

    print *, "Adding argument name"
    call parser%add_argument ("name", "n", required=.true., status=status)

    print *, "Adding argument flag1"
    call parser%add_argument ("flag1", action=ARGPARSE_ACTION_STORE_TRUE, &
        status=status)

    print *, "Adding argument flag2"
    call parser%add_argument ("flag2", action=ARGPARSE_ACTION_STORE_FALSE, &
        status=status)

    print *, "Adding argument const"
    call parser%add_argument ("const", "c", action=ARGPARSE_ACTION_STORE_CONST, &
        const=123, default=0, status=status)

    print *, "Adding argument opt_array"
    call parser%add_argument ("opt_array", nargs=2, default=["foo", "bar"], status=status)

    print *, "Adding argument opt"
    call parser%add_argument ("opt", "o", default="Default option", status=status)

    ! test adding an argument with an existing name
    ! call parser%add_argument ("name", status=status)

    print *, "Parsing command line"
    call parser%parse ()

    call parser%get ("name", name, status=status)
    print *, "Argument 'name': ", name%to_char()

    call parser%get ("flag1", flag1)
    print *, "Argument 'flag1': ", flag1

    call parser%get ("flag2", flag2)
    print *, "Argument 'flag2': ", flag2

    opt_array = ""
    call parser%get ("opt_array", opt_array)
    print '(tr1, a, *(a,:,","))', "Argument 'opt_array': ",  opt_array

    call parser%get ("opt", opt)
    print *, "Argument 'opt': ", opt%to_char()

    call parser%get ("opt", opt_char)
    print *, "Argument 'opt', char type: ", opt_char

    call parser%get ("const", const)
    print *, "Argument 'const': ", const



end program
