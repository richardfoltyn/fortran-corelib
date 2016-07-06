module assertion_mod

    use iso_fortran_env
    use linked_list_mod

    implicit none
    private

    integer, parameter :: STATUS_PASSED = 0, STATUS_FAILED = 1, &
            STATUS_UNDEFINED = -1

    type :: assertion
        private

        character (len=:), allocatable :: label
        integer :: status = STATUS_UNDEFINED
    contains

        procedure, public, pass(self) :: get_status
        procedure, public, pass(self) :: set_status

        procedure, public, pass(self) :: get_label
        procedure, public, pass(self) :: set_label

        procedure, public, pass(self) :: print
    end type

    interface assertion
        module procedure ctor_default
    end interface

    public :: STATUS_PASSED, STATUS_FAILED, assertion, assertion_cast

    contains

! *****************************************************************************
! CONSTRUCTORS

function ctor_default (label, status) result(self)

    character (len=*), intent(in), optional :: label
    integer, intent(in), optional :: status

    type (assertion) :: self

    if (present(status)) then
        if ((status /= STATUS_PASSED) .and. (status /= STATUS_FAILED)) then
            stop "Invalid status code"
        end if

        self%status = status
    end if

    if (present(label)) self%label = label

end function

! *****************************************************************************
! ATTRIBUTES

function get_label (self) result(res)
    class (assertion), intent(in) :: self
    character (len=:), allocatable :: res

    if (allocated(self%label)) then
        allocate (res, source=self%label)
    else
        allocate (res, source="")
    end if
end function

subroutine set_label (self, label)
    class (assertion), intent(in out) :: self
    character (len=*), intent(in) :: label

    if (allocated(self%label)) deallocate (self%label)
    allocate (self%label, source=label)

end subroutine

pure function get_status (self) result(res)
    class (assertion), intent(in) :: self
    integer :: res

    res = self%status
end function

subroutine set_status (self, status)
    class (assertion), intent(in out) :: self
    integer :: status

    self%status = status

end subroutine

! *****************************************************************************
! PRINTING


subroutine print (self, lun, indent)

    class (assertion), intent(in) :: self
    integer, intent(in), optional :: lun, indent
    integer :: llun, lindent, len_label

    integer, parameter :: LINEWIDTH = 80
    character (len=*), parameter :: STR_PASSED = "PASSED", STR_FAILED = "FAILED"

    character (len=:), allocatable :: str_indent
    character (len=:), allocatable :: llabel
    character (len=10) :: str_status
    character (len=100) :: fmt_str

    llun = OUTPUT_UNIT
    lindent = 2

    allocate (character (len=lindent) :: str_indent)
    str_indent = ""

    if (present(lun)) llun = lun
    if (present(indent)) lindent = indent
    if (allocated(self%label)) then
        allocate (character (len(self%label)) :: llabel)
        llabel = self%label
    else
        allocate (llabel, source="[unlabeled assertion]")
    end if

    if (self%get_status() == STATUS_PASSED) then
        str_status = STR_PASSED
    else
        str_status = STR_FAILED
    end if


    ! deduct space for status message
    len_label = LINEWIDTH - lindent - max(len(STR_PASSED), len(STR_FAILED)) - 2
    fmt_str = ""
    write (unit=fmt_str, fmt="('(tr ', i0, ', a, tr ', i0, ', a, tr 2, a)')") &
            lindent, max(len_label - len(llabel), 0)

    write (unit=llun, fmt=fmt_str) llabel, str_status

end subroutine

! *****************************************************************************
! CASTS

function assertion_cast (base) result(res)
    class (*), intent(in), pointer :: base
    class (assertion), pointer :: res

    select type (obj => base)
    class is (assertion)
        res => obj
    class default
        stop "Unsupported cast"
    end select

end function

end module
