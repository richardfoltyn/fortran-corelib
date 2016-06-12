module foounit_mod

    use iso_fortran_env
    use linked_list_mod
    use test_case_mod
    use collection_mod

    implicit none
    private

    type :: test_suite
        private

        character (len=:), allocatable :: label
        type (linked_list), allocatable :: tests

    contains

        procedure, pass :: check_init
        procedure, public, pass :: set_label
        procedure, public, pass :: tally_results

        procedure, public, pass :: print
        procedure, public, pass :: add_test

        ! final :: finalize
    end type

    interface test_suite
        module procedure ctor_char
    end interface

    public :: test_suite, test_case


contains

! *****************************************************************************
! INITIALIZATION

function ctor_char (label) result(res)

    character (len=*), intent(in), optional :: label
    type (test_suite) :: res

    if (present(label)) allocate (res%label, source=label)

end function

subroutine check_init (self)
    class (test_suite), intent(in out) :: self

    if (.not. allocated(self%tests)) allocate (self%tests)
end subroutine

! ******************************************************************************
! ADD_TEST method

function add_test (self, label) result (res)
    class (test_suite), intent(in out) :: self
    character (len=*), intent(in), optional :: label
    type (test_case), pointer :: res
    ! local copy of test case
    type (test_case) :: tc

    ! check that tests list was allocated
    call self%check_init ()

    call tc%set_label(label)

    call self%tests%append (tc)

    ! retrieve pointer to created test_case object. We need to do this after
    ! the object was *copied* into the list to be sure that pointer has the
    ! correct target!
    res => test_case_cast (self%tests%item (self%tests%length()))

end function

! *****************************************************************************
! ATTRIBUTES
subroutine set_label (self, label)
    class (test_suite), intent(in out) :: self
    character (len=*), intent(in) :: label

    allocate (self%label, source=label)
end subroutine

! *****************************************************************************
! REPORTING

subroutine tally_results (self, passed, failed)
    class (test_suite), intent(in) :: self
    integer, intent(out) :: passed, failed

    class (test_case), pointer :: ptr_test
    ! deallocated automatically on subroutine exit
    class (iterator), allocatable :: iter

    integer :: passed_i, failed_i

    passed = 0
    failed = 0

    ! get list iterator
    call self%tests%get_iter (iter)

    do while (iter%has_next())
        ptr_test => test_case_cast (iter%item())

        ! get tally from individual test case
        call ptr_test%tally_results (passed_i, failed_i)

        passed = passed + passed_i
        failed = failed + failed_i
    end do

end subroutine

subroutine print (self, lun)
    class (test_suite), intent(in) :: self
    integer, intent(in), optional :: lun

    class (test_case), pointer :: ptr_test
    ! deallocated automatically on subroutine exit
    class (iterator), allocatable :: iter

    integer, parameter :: LINEWIDTH = 80
    character (len=*), parameter :: TITLE = "Test summary (all tests)"
    character (len=LINEWIDTH) :: separator
    integer, parameter :: LEFT_INDENT = 1
    character (len=:), allocatable :: str_indent
    integer :: passed, failed
    integer :: llun, i

    ! write to stdout if nothing else specified
    llun = OUTPUT_UNIT
    if (present(lun)) llun = lun

    allocate (character (len=LEFT_INDENT) :: str_indent)
    str_indent = " "

    ! initialize separator line
    forall (i = 1:LINEWIDTH) separator(i:i) = "="

    call self%tally_results (passed, failed)

    ! write empty line
    write (unit=llun, fmt="(a)") ""
    write (unit=llun, fmt="(a)") SEPARATOR


    if (allocated(self%label)) then
        if (len(self%label) > 0) then
            write (unit=llun, fmt="(a, 'Starting test run ''', a, '''...')") str_indent, self%label
            write (unit=llun, fmt="(a)") SEPARATOR
        end if
    end if

    ! report results for individual test cases
    if ((passed + failed) > 0) then
        ! get list iterator
        call self%tests%get_iter (iter)

        do while (iter%has_next())
            ptr_test => test_case_cast (iter%item())

            call ptr_test%print (llun)
        end do
    end if

    write (unit=llun, fmt="(a)") ""
    write (unit=llun, fmt="(a)") SEPARATOR
    write (unit=llun, fmt="(a, a)") str_indent, TITLE
    write (unit=llun, fmt="(a, 'Passed: ', i0, tr 2, 'Failed: ', i0)") &
                str_indent, passed, failed
    write (unit=llun, fmt="(a)") SEPARATOR

end subroutine

! *****************************************************************************
! FINALIZERS
!subroutine finalize (self)
!    type (test_suite), intent(in out) :: self
!
!    if (allocated(self%label)) deallocate (self%label)
!    if (allocated(self%tests)) deallocate (self%tests)
!end subroutine


end module
