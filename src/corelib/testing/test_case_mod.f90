module test_case_mod

    use iso_fortran_env
    use assertion_mod
    use linked_list_mod
    use collection_mod
    
    implicit none
    private
    
    type :: test_case
        private
        
        character (len=:), allocatable :: label
        type (linked_list), allocatable :: tests
        
    contains
    
        procedure, pass :: check_init
        procedure, public, pass :: set_label
        procedure, public, pass :: tally_results
        
        procedure, public, pass :: assert_true
        procedure, public, pass :: assert_false
        
        procedure, public, pass :: item
        procedure, public, pass :: print
    
        !final :: finalize
    end type

    interface test_case
        module procedure ctor_char
    end interface
    
    public :: test_case, test_case_cast

contains

! *****************************************************************************
! INITIALIZATION
function ctor_char (label) result(res)

    character (len=*), intent(in), optional :: label
    type (test_case) :: res
    
    if (present(label)) allocate (res%label, source=label)

end function

subroutine check_init (self)
    class (test_case), intent(in out) :: self
    
    if (.not. allocated(self%tests)) allocate (self%tests)
end subroutine

! *****************************************************************************
! ATTRIBUTES

subroutine set_label (self, label)
    class (test_case), intent(in out) :: self
    character (len=*), intent(in) :: label
    
    allocate (self%label, source=label)
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

subroutine assert_true (self, condition, label)
    class (test_case), intent(in out) :: self
    character (len=*), intent(in), optional :: label
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

subroutine assert_false (self, condition, label)
    class (test_case), intent(in out) :: self
    character (len=*), intent(in), optional :: label
    logical, intent(in) :: condition
    
    call self%assert_true (.not. condition, label)

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
    
    integer, parameter :: LINEWIDTH = 80
    character (len=*), parameter :: PREFIX = ">>>"
    integer, parameter :: LEFT_INDENT = 1, ASSERT_INDENT = 2
    character (len=100) :: fmt_str
    character (len=:), allocatable :: str_indent, str_label
    integer :: passed, failed
    integer :: llun

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
    
    if (allocated(self%label)) then
        allocate (str_label, source=self%label)
    else
        allocate (str_label, source="[unspecified test label]")
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

function test_case_cast (base) result(res)
    class (*), intent(in), pointer :: base
    class (test_case), pointer :: res
    
    select type (obj => base)
    class is (test_case)
        res => obj
    class default
        stop "Unsupported cast"        
    end select

end function

end module