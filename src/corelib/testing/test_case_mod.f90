module test_case_mod

    use iso_fortran_env
    use assertion_mod
    use linked_list_mod
    use collection_mod
    use corelib_string

    implicit none
    private

    character (*), parameter :: DEFAULT_LABEL = "[unspecified test label]"

    type :: test_case
        private

        type (str) :: label
        type (linked_list), allocatable :: tests

    contains

        procedure, pass :: check_init

        procedure, pass :: set_label_str
        procedure, pass :: set_label_char
        generic, public :: set_label => set_label_str, set_label_char

        procedure, public, pass :: tally_results

        procedure, pass :: assert_true_str
        procedure, pass :: assert_true_char
        procedure, pass :: assert_false_str
        procedure, pass :: assert_false_char

        generic, public :: assert_true => assert_true_str, assert_true_char
        generic, public :: assert_false => assert_false_str, assert_false_char

        procedure, public, pass :: item
        procedure, public, pass :: print

        !final :: finalize
    end type

    interface test_case
        module procedure ctor_char, ctor_str, ctor_default
    end interface

    interface dynamic_cast
        module procedure cast_any_to_test_case
    end interface

    public :: test_case, dynamic_cast

contains

! *****************************************************************************
! INITIALIZATION

function ctor_default() result(res)
    type (test_case) :: res
    call ctor_impl (res)
end function

function ctor_char (label) result(res)
    character (len=*), intent(in) :: label
    type (test_case) :: res

    call ctor_impl (res, str(label))
end function

function ctor_str (label) result(res)
    class (str), intent(in) :: label
    type (test_case) :: res

    call ctor_impl (res, label)
end function

subroutine ctor_impl (self, label)
    type (test_case), intent(in out) :: self
    type (str), intent(in), optional :: label

    if (present(label)) then
        self%label = label
    else
        self%label = DEFAULT_LABEL
    end if
end subroutine

subroutine check_init (self)
    class (test_case), intent(in out) :: self

    if (.not. allocated(self%tests)) allocate (self%tests)
end subroutine

! *****************************************************************************
! ATTRIBUTES

subroutine set_label_char (self, label)
    class (test_case), intent(in out) :: self
    character (len=*), intent(in) :: label

    self%label = label
end subroutine

subroutine set_label_str (self, label)
    class (test_case), intent(in out) :: self
    class (str), intent(in) :: label

    self%label = label
end subroutine


! *****************************************************************************
! ITEM GETTER

function item (self, i) result(res)
    class (test_case), intent(in) :: self
    integer, intent(in) :: i
    class (assertion), pointer :: res

    class (*), pointer :: ptr_base

    ptr_base => self%tests%item (i)
    res => null()

    select type (obj => ptr_base)
        class is (assertion)
            res => obj
        class default
            stop "Unsupported type"
    end select

end function

! *****************************************************************************
! ASSERTIONS

subroutine assert_true_str (self, condition, label)
    class (test_case), intent(in out) :: self
    class (str), intent(in), optional :: label
    logical, intent(in) :: condition

    ! safe to allocate as local variable, will be copied into linked list
    type (assertion) :: assert_obj
    integer :: status

    call self%check_init

    status = STATUS_FAILED
    if (condition) status = STATUS_PASSED

    call assert_obj%set_status (status)

    if (present(label)) then
        call assert_obj%set_label (label)
    end if

    ! append assertion to list
    call self%tests%append (assert_obj)
end subroutine

subroutine assert_true_char (self, condition, label)
    class (test_case), intent(in out) :: self
    character (len=*), intent(in) :: label
    logical, intent(in) :: condition

    call self%assert_true (condition, str(label))
end subroutine

subroutine assert_false_str (self, condition, label)
    class (test_case), intent(in out) :: self
    class (str), intent(in), optional :: label
    logical, intent(in) :: condition

    call self%assert_true (.not. condition, label)

end subroutine

subroutine assert_false_char (self, condition, label)
    class (test_case), intent(in out) :: self
    character (len=*), intent(in) :: label
    logical, intent(in) :: condition

    call self%assert_true (.not. condition, str(label))

end subroutine

! *****************************************************************************
! REPORTING

subroutine tally_results (self, passed, failed)
    class (test_case), intent(in) :: self
    integer, intent(out) :: passed, failed

    class (assertion), pointer :: ptr_assert
    ! deallocated automatically on subroutine exit
    class (iterator), allocatable :: iter

    passed = 0
    failed = 0

    call self%tests%get_iter (iter)

    do while (iter%has_next())
        ptr_assert => assertion_cast (iter%item())
        if (ptr_assert%get_status() == STATUS_PASSED) then
            passed = passed + 1
        else
            failed = failed + 1
        end if
    end do

end subroutine

subroutine print (self, lun)
    class (test_case), intent(in) :: self
    integer, intent(in), optional :: lun

    class (assertion), pointer :: ptr_assert
    ! deallocated automatically on subroutine exit
    class (iterator), allocatable :: iter

    character (len=*), parameter :: PREFIX = ">>>"
    integer, parameter :: LEFT_INDENT = 1, ASSERT_INDENT = 2
    character (len=100) :: fmt_str
    character (len=:), allocatable :: str_indent, str_label
    integer :: passed, failed
    integer :: llun, n

    allocate (character (len=LEFT_INDENT) :: str_indent)
    str_indent = " "

    ! write to stdout if nothing else specified
    llun = OUTPUT_UNIT
    if (present(lun)) llun = lun

    passed = 0
    failed = 0

    call self%tally_results (passed, failed)

    ! write empty line
    write (unit=llun, fmt="(a)") ""

    fmt_str = ""
    write (unit=llun, fmt="(a, tr 1)", advance="no") PREFIX

    n = len(self%label)
    if (n > 0) then
        ! cannot use component as source in gfortran 4.9
        allocate (character (len=n) :: str_label)
        str_label = self%label
    else
        allocate (str_label, source=DEFAULT_LABEL)
    end if

    write (unit=llun, fmt="(a, tr 1, '''', a, '''')") "Running", str_label

    if ((passed + failed) > 0) then
        ! get list iterator
        call self%tests%get_iter (iter)

        do while (iter%has_next())
            ptr_assert => assertion_cast (iter%item())
            call ptr_assert%print (llun, ASSERT_INDENT)
        end do
    end if

    write (unit=llun, fmt="(a)") ""
    write (unit=llun, fmt="(a, tr 1)", advance="no") PREFIX
    write (unit=llun, fmt="(a, tr 1)", advance="no") "Test statistics for"
    write (unit=llun, fmt="('''', a, '''')") trim(str_label)
    write (unit=llun, fmt="(a)", advance="no") str_indent
    write (unit=llun, fmt="(a, tr 1, i0, tr 2, a, tr 1, i0)") &
            "Passed:", passed, "Failed:", failed


end subroutine

! *****************************************************************************
! FINALIZERS
!subroutine finalize (self)
!    type (test_case), intent(in out) :: self
!
!    if (allocated(self%label)) deallocate(self%label)
!    if (allocated(self%tests)) deallocate(self%tests)
!end subroutine

!subroutine assertion_node_finalize (self)
!    type (assertion_node), intent(in out) :: self
!
!    if (allocated(self%value)) deallocate (self%value)
!
!end subroutine

! *****************************************************************************
! CASTS

subroutine cast_any_to_test_case (base, res)
    class (*), intent(in), pointer :: base
    type (test_case), intent(out), pointer :: res

    select type (obj => base)
    class is (test_case)
        res => obj
    class default
        error stop "Unsupported cast"
    end select
end subroutine

end module
