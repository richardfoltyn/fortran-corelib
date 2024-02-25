module fcore_testing_test_suite_mod

    use iso_fortran_env
    use fcore_testing_test_case_mod
    use fcore_collections
    use fcore_common

    implicit none
    private

    character (*), parameter :: DEFAULT_LABEL = "[unnamed test suite]"

    type :: test_suite
        private

        type (str) :: label
        type (linked_list) :: tests

    contains

        procedure, pass :: set_label_str
        procedure, pass :: set_label_char
        generic, public :: set_label => set_label_str, set_label_char

        procedure, public, pass :: tally_results

        procedure, public, pass :: print

        procedure, pass :: add_test_str
        procedure, pass :: add_test_char
        generic, public :: add_test => add_test_str, add_test_char

    end type

    interface test_suite
        module procedure ctor_char, ctor_str
    end interface

    public :: test_suite

contains

! *****************************************************************************
! INITIALIZATION

pure function ctor_str (label) result(res)
    class (str), intent(in), optional :: label
    type (test_suite) :: res

    call ctor_impl (res, label)
end function

pure function ctor_char (label) result(res)
    character (len=*), intent(in) :: label
    type (test_suite) :: res

    call ctor_impl (res, str(label))
end function

pure subroutine ctor_impl (self, label)
    type (test_suite), intent(in out) :: self
    class (str), intent(in), optional :: label

    if (present(label)) then
        self%label = label
    else
        self%label = DEFAULT_LABEL
    end if
end subroutine


! ******************************************************************************
! ADD_TEST method

function add_test_str (self, label) result (res)
    class (test_suite), intent(in out) :: self
    class (str), intent(in), optional :: label
    type (test_case), pointer :: res
    ! local copy of test case
    type (test_case) :: tc
    class (*), pointer :: ptr_item

    if (present(label)) then
        call tc%set_label (label)
    end if

    call self%tests%append (tc)

    ! retrieve pointer to created test_case object. We need to do this after
    ! the object was *copied* into the list to be sure that pointer has the
    ! correct target!
    ptr_item => self%tests%item (self%tests%length())
    call dynamic_cast (ptr_item, res)

end function

function add_test_char (self, label) result(res)
    class (test_suite), intent(in out) :: self
    character (len=*), intent(in) :: label
    type (test_case), pointer :: res

    res => self%add_test (str(label))
end function

! *****************************************************************************
! ATTRIBUTES
subroutine set_label_str (self, label)
    class (test_suite), intent(in out) :: self
    class (str), intent(in) :: label

    self%label = label
end subroutine

subroutine set_label_char (self, label)
    class (test_suite), intent(in out) :: self
    character (len=*), intent(in) :: label

    call self%set_label (str(label))
end subroutine

! *****************************************************************************
! REPORTING

subroutine tally_results (self, passed, failed)
    class (test_suite), intent(in) :: self
    integer, intent(out) :: passed, failed

    type (test_case), pointer :: ptr_test
    ! deallocated automatically on subroutine exit
    class (iterator), allocatable :: iter
    class (*), pointer :: ptr_item

    integer :: passed_i, failed_i

    passed = 0
    failed = 0

    ! get list iterator
    call self%tests%get_iter (iter)

    do while (iter%has_next())
        ptr_item => iter%item()
        call dynamic_cast (ptr_item, ptr_test)

        ! get tally from individual test case
        call ptr_test%tally_results (passed_i, failed_i)

        passed = passed + passed_i
        failed = failed + failed_i
    end do

end subroutine

subroutine print (self, lun)
    class (test_suite), intent(in) :: self
    integer, intent(in), optional :: lun

    type (test_case), pointer :: ptr_test
    class (*), pointer :: ptr_item
    ! deallocated automatically on subroutine exit
    class (iterator), allocatable :: iter

    integer, parameter :: LINEWIDTH = 80
    character (len=*), parameter :: TITLE_SUMMARY = "Test summary (all tests)"
    character (len=LINEWIDTH) :: separator
    integer, parameter :: LEFT_INDENT = 1
    character (len=:), allocatable :: str_indent, llabel
    integer :: passed, failed
    integer :: llun, n

    ! write to stdout if nothing else specified
    llun = OUTPUT_UNIT
    if (present(lun)) llun = lun

    allocate (character (len=LEFT_INDENT) :: str_indent)
    str_indent = " "

    ! initialize separator line
    separator = repeat ("=", LINEWIDTH)

    call self%tally_results (passed, failed)

    ! write empty line
    write (unit=llun, fmt="(a)") ""
    write (unit=llun, fmt="(a)") SEPARATOR

    n = len(self%label)
    if (n > 0) then
        allocate (character (len=n) :: llabel)
        llabel = self%label
    else
        allocate (llabel, source=DEFAULT_LABEL)
    end if

    write (unit=llun, fmt="(a, 'Starting test run ''', a, '''...')") str_indent, llabel
    write (unit=llun, fmt="(a)") SEPARATOR

    ! report results for individual test cases
    if ((passed + failed) > 0) then
        ! get list iterator
        call self%tests%get_iter (iter)

        do while (iter%has_next())
            ptr_item => iter%item()
            call dynamic_cast (ptr_item, ptr_test)

            call ptr_test%print (llun)
        end do
    end if

    write (unit=llun, fmt="(a)") ""
    write (unit=llun, fmt="(a)") SEPARATOR
    write (unit=llun, fmt="(a, a)") str_indent, TITLE_SUMMARY
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
