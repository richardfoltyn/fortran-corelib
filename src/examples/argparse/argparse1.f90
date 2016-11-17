program argparse1

    use corelib_argparse
    use corelib_strings

    implicit none

    type (argparser) :: parser
    integer :: status

    type (str) :: name, opt
    logical :: flag1, flag2
    integer :: const

    call parser%init ("argparse example 1")
    call parser%add_argument ("name", "n", required=.true., status=status)
    call parser%add_argument ("flag1", action=ARGPARSE_ACTION_STORE_TRUE, &
        status=status)
    call parser%add_argument ("flag2", action=ARGPARSE_ACTION_STORE_FALSE, &
        status=status)
    call parser%add_argument ("opt", "o", default="Default opt", status=status)
    call parser%add_argument ("const", "c", action=ARGPARSE_ACTION_STORE_CONST, &
        const=123, default=0, status=status)

    call parser%parse ()

    call parser%get ("name", name, status=status)
    print *, "Argument 'name': ", name%to_char()

    call parser%get ("flag1", flag1)
    print *, "Argument 'flag1': ", flag1

    call parser%get ("flag2", flag2)
    print *, "Argument 'flag2': ", flag2

    call parser%get ("opt", opt)
    print *, "Argument 'opt': ", opt%to_char()

    call parser%get ("const", const)
    print *, "Argument 'const': ", const



end program
