
program test_io_ini

    use iso_fortran_env
    use fcore_common
    use fcore_io
    use fcore_testing

    implicit none

    character (*), parameter :: NL = new_line ('A')
    character (*), parameter :: SAMPLE1 = &
        '[section-1]' // nl // &
        'key1=value1' // nl // &
        'key2=1' // nl // &
        'key3=.true.' // nl // &
        '; Comment' // nl // &
        '  ' // nl // &
        '' // nl // &
        '[section-2]' // nl // &
        'key1=1.0' // nl // &
        'key2=1.0e-10' // nl // &
        'key3=F' // nl // &
        'key4=Yes' // nl// &
        'key5=no' // nl // &
        'key6=on' // nl // &
        'key7=off' // nl

    call test_all ()

contains

subroutine test_all()

    type (test_suite) :: tests

    call tests%set_label ("fcore_io_ini unit tests")

!    call test_from_file (tests)

    ! test reading integer arguments
!    call test_from_char (tests)

    call test_write (tests)

    ! print test statistics
    call tests%print ()

end subroutine



subroutine test_from_file (tests)
    class (test_suite) :: tests
    class (test_case), pointer :: tc

    character (*), parameter :: PATH = '/home/richard/tmp/test.ini'
    type (ini_file) :: file
    type (status_t) :: status


    tc => tests%add_test ('Parsing from internal character file')

    file = ini_file(path=PATH)

    call file%parse (status=status)





end subroutine



subroutine test_from_char (tests)
    class (test_suite) :: tests
    class (test_case), pointer :: tc

    type (ini_file) :: file
    type (status_t) :: status


    tc => tests%add_test ('Parsing from internal character file')

    call file%parse (SAMPLE1, status=status)

end subroutine



subroutine test_write (tests)
    class (test_suite) :: tests
    class (test_case), pointer :: tc

    type (ini_file) :: file
    type (status_t) :: status

    character (*), parameter :: PATH = '/tmp/test-write.ini'

    tc => tests%add_test ('Parsing from internal character file')

    file = ini_file (path=PATH)
    call file%parse (SAMPLE1, status=status)

    call file%write (status)

end subroutine


end program
