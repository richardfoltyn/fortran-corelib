program argparse1

    use fcore_argparse

    implicit none

    type (argparser) :: parser
    type (status_t) :: status

    type (str) :: name, opt
    logical :: flag1, flag2
    integer :: const
    character (10) :: opt_char

    integer :: i, n
    type (str), dimension(:), allocatable :: swork
    character (:), dimension(:), allocatable :: cwork
    integer, dimension(:), allocatable :: iwork

    call parser%init ("argparse example 1")

    print *, "Adding argument name"
    call parser%add_argument ("name", "n", required=.true., status=status)
    call status%print ()

    print *, "Adding argument flag1"
    call parser%add_argument ("flag1", action=ARGPARSE_ACTION_STORE_TRUE, &
        status=status)
    call status%print ()

    print *, "Adding argument flag2"
    call parser%add_argument ("flag2", action=ARGPARSE_ACTION_STORE_FALSE, &
        status=status)
    call status%print ()

    print *, "Adding argument const"
    call parser%add_argument ("const", "c", action=ARGPARSE_ACTION_STORE_CONST, &
        const=123, default=0, status=status)
    call status%print ()

    print *, "Adding argument opt-char-arr"

    allocate (character (3) :: cwork(2))
    cwork(1) = "foo"
    cwork(2) = "baz"
    call parser%add_argument ("opt-char-arr", nargs=2, default=cwork, &
        status=status)
    call status%print ()
    deallocate (cwork)

    print *, "Adding argument opt-int-arr"
    allocate (iwork(2))
    iwork(1) = 123
    iwork(2) = 456
    call parser%add_argument ("opt-int-arr", nargs=2, default=iwork, &
        status=status)
    call status%print ()
    deallocate (iwork)

    print *, "Adding argument opt-str"
    call parser%add_argument ("opt-str", nargs=1, default=str("opt-str default"), &
        status=status)
    call status%print ()

    ! Note: using temporary str(:) array for default values crashes with gfortran
    ! and returns junk default values with ifort
    print *, "Adding argument opt-str-arr"
    allocate (swork(2))
    swork(1) = "foo"
    swork(2) = "bar baz"
    call parser%add_argument ("opt-str-arr", nargs=2, default=swork, status=status)
    call status%print ()
    deallocate (swork)

    print *, "Adding argument opt"
    call parser%add_argument ("opt", "o", default="Default opt", status=status)
    call status%print ()

    print *, "Adding argument opt2 (using str interface)"
    call parser%add_argument (str("opt2"), default="Default opt2", status=status)
    call status%print ()

    ! test adding an argument with an existing name
    ! call parser%add_argument ("name", status=status)

    print *, "Parsing command line"
    call parser%parse (status)
    call status%print ()

    call parser%get ("name", name, status=status)
    print *, "Argument 'name': ", name%to_char()
    call status%print ()

    call parser%get ("flag1", flag1, status=status)
    print *, "Argument 'flag1': ", flag1
    call status%print ()

    call parser%get ("flag2", flag2, status=status)
    print *, "Argument 'flag2': ", flag2
    call status%print ()

    allocate (character (3) :: cwork(2))
    call parser%get ("opt-char-arr", cwork, status)
    print '(tr1, a, *(a,:,", "))', "Argument 'opt-char-arr': ",  cwork
    call status%print ()
    deallocate (cwork)

    ! Test retrieving as str array. This makes more sense as str array
    ! does not impose requiredment that all elements are the same length.
    allocate (swork(2))
    call parser%get ("opt-char-arr", swork, status)
    call status%print ()
    if (status == FC_STATUS_OK) then
        print *, "Argument 'opt-char-arr', retrieved as type(str) array"
        do i = 1, size(swork)
            print '(t10, "(", i0, ") ", a)', i, swork(i)%to_char()
        end do
    end if
    deallocate (swork)

    ! Test retrieving integer array value
    print *, "== Argument 'opt-int-arr' =="
    n = parser%get_nvals ("opt-int-arr")
    allocate (iwork(n))
    call parser%get ("opt-int-arr", iwork, status)
    if (status == FC_STATUS_OK) then
        print '(tr1, a, *(i0, :, ", "))', "-- Values : ", iwork
    end if
    call status%print ()
    deallocate (iwork)

    ! Retrieve opt-str-arr
    allocate (swork(2))
    call parser%get ("opt-str-arr", swork, status)
    call print_result_header (parser, 'opt-str-arr', status)
    if (status == FC_STATUS_OK) then
        print *, "-- Values:"
        do i = 1, size(swork)
            print '(t5, "(", i0, ") ", a)', i, swork(i)%to_char()
        end do
    end if
    deallocate (swork)

    call parser%get ("opt", opt, status)
    print *, "Argument 'opt': ", opt%to_char()
    call status%print ()

    call parser%get ("opt", opt_char, status)
    print *, "Argument 'opt', char type: ", opt_char
    call status%print ()

    call parser%get ("const", const, status)
    print *, "Argument 'const': ", const
    call status%print ()

contains

subroutine print_result_header (parser, name, status)
    type (argparser), intent(in) :: parser
    character (*), intent(in) :: name
    type (status_t), intent(in) :: status

    print '(/,tr1, "== Argument ", a, " ==")', name
    call status%print ()
    print '(tr1, "-- Present: ", l1)', parser%is_present (name)

end subroutine


end program
