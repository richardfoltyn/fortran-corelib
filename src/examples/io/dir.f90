

program fcore_examples_io_dir

    use fcore_io
    use fcore_strings

    implicit none

    call example_is_dir ()

    contains

subroutine example_is_dir ()

    logical :: dir_exists
    type (str) :: sname
    character (*), parameter :: cname1 = '/tmp'
    character (*), parameter :: cname2 = '/tmp123'

    ! 1. Test directory that actually exists
    ! Test with string interface
    sname = cname1
    dir_exists = is_dir (sname)
    print 100, sname%to_char(), dir_exists

    ! Test with character interface
    dir_exists = is_dir (cname1)
    print 100, cname1, dir_exists

    ! 2. Test directory that actually exists
    ! Test with string interface
    sname = cname2
    dir_exists = is_dir (sname)
    print 100, sname%to_char(), dir_exists

    ! Test with character interface
    dir_exists = is_dir (cname2)
    print 100, cname2, dir_exists


100 format (tr2, "Directory ", (a), " exists: ", l1)
end subroutine

end program