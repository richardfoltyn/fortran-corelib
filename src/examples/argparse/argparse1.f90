program argparse1

    use corelib_argparse

    implicit none

    type (argparser) :: parser
    type (status_t) :: status

    type (str) :: name, opt
    character (20) :: opt_char
    logical :: flag1, flag2
    logical, dimension(2) :: flag3
    integer :: const

    call parser%init ("argparse example 1")
    call parser%add_argument ("name", "n", required=.true., status=status)
    call parser%add_argument ("flag1", action=ARGPARSE_ACTION_STORE_TRUE, &
        status=status)
    call parser%add_argument ("flag2", action=ARGPARSE_ACTION_STORE_FALSE, &
        status=status)
    call parser%add_argument ("flag3", action=ARGPARSE_ACTION_STORE_CONST, &
        const=[.true.,.false.], default=[.false., .false.], status=status)
    call parser%add_argument ("opt", "o", default="Default opt", status=status)
    call parser%add_argument ("const", "c", action=ARGPARSE_ACTION_STORE_CONST, &
        const=123, default=0, status=status)

    ! test adding an argument with an existing name
    call parser%add_argument ("name")

    call parser%parse ()

    call parser%get ("name", name, status=status)
    print *, "Argument 'name': ", name%to_char()

    call parser%get ("flag1", flag1)
    print *, "Argument 'flag1': ", flag1

    call parser%get ("flag2", flag2)
    print *, "Argument 'flag2': ", flag2

    call parser%get ("flag3", flag3)
    print '(tr1, a, *(l1,:,","))', "Argument 'flag3': ",  flag3

    call parser%get ("opt", opt)
    print *, "Argument 'opt': ", opt%to_char()

    call parser%get ("opt", opt_char)
    print *, "Argument 'opt', char type: ", opt_char

    call parser%get ("const", const)
    print *, "Argument 'const': ", const



end program
