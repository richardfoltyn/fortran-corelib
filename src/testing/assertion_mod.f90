module fcore_testing_assertion_mod

    use iso_fortran_env
    use fcore_collections
    use fcore_common

    implicit none
    private

    integer, parameter :: STATUS_PASSED = 0, STATUS_FAILED = 1, &
            STATUS_UNDEFINED = -1

    type :: assertion
        private

        type (str) :: label
        integer :: status = STATUS_UNDEFINED
    contains

        procedure, public, pass :: get_status
        procedure, public, pass :: set_status

        procedure, public, pass :: get_label
        procedure, pass :: set_label_str
        procedure, pass :: set_label_char
        generic, public :: set_label => set_label_str, set_label_char

        procedure, public, pass :: print
    end type

    interface assertion
        module procedure ctor_char, ctor_str
    end interface

    public :: STATUS_PASSED, STATUS_FAILED, assertion, assertion_cast

    contains

! *****************************************************************************
! CONSTRUCTORS

function ctor_default(status) result(self)
    integer, intent(in), optional :: status
    type (assertion) :: self

    call ctor_impl (self, status=status)
end function

function ctor_str (label, status) result(self)
    type (str), intent(in) :: label
    integer, intent(in), optional :: status
    type (assertion) :: self

    call ctor_impl (self, label, status)

end function

function ctor_char (label, status) result(self)
    character (len=*), intent(in) :: label
    integer, intent(in), optional :: status
    type (assertion) :: self

    call ctor_impl (self, str(label), status)
end function

subroutine ctor_impl (self, label, status)
    type (assertion), intent(in out) :: self
    type (str), intent(in) :: label
    integer, intent(in) :: status

    optional :: label, status

    integer :: lstatus

    lstatus = STATUS_UNDEFINED
    if (present(status)) lstatus = status

    if ((lstatus /= STATUS_PASSED) .and. (lstatus /= STATUS_FAILED) .and. &
        (lstatus /= STATUS_UNDEFINED)) then
        error stop "Invalid status code"
    end if

    self%status = lstatus

    if (present(label)) then
        self%label = label
    else
        self%label = "[unlabeled assertion]"
    end if
end subroutine

! *****************************************************************************
! ATTRIBUTES

function get_label (self) result(res)
    class (assertion), intent(in) :: self
    type (str) :: res

    res = self%label
end function

subroutine set_label_str (self, label)
    class (assertion), intent(in out) :: self
    type (str), intent(in) :: label

    self%label = label

end subroutine

subroutine set_label_char (self, label)
    class (assertion), intent(in out) :: self
    character (len=*), intent(in) :: label

    call self%set_label (str(label))
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
    integer, intent(in), optional :: lun
    integer, intent(in), optional :: indent

    integer :: llun, lindent, max_len_label, n
    integer, parameter :: LINEWIDTH = 80
    character (len=*), parameter :: STR_PASSED = "PASSED", STR_FAILED = "FAILED"

    integer :: nfill
    character (len=10) :: str_status
    character (len=100) :: fmt_str
    type (str) :: label

    llun = OUTPUT_UNIT
    lindent = 2

    if (present(lun)) llun = lun
    if (present(indent)) lindent = indent
    n = len(self%label)
    if (n > 0) then
        label = self%label
    else
        label = "[unlabeled assertion]"
    end if

    if (self%get_status() == STATUS_PASSED) then
        str_status = STR_PASSED
    else
        str_status = STR_FAILED
    end if

    ! deduct space for status message, separator between label and status
    ! message, and indentation
    max_len_label = LINEWIDTH - lindent - max(len(STR_PASSED), len(STR_FAILED)) - 2
    fmt_str = ""
    if (max_len_label > len(label)) then
        ! Need to left-adjust label
        nfill = max_len_label - len(label)
        label = label // repeat(' ', nfill)
    end if

    write (unit=fmt_str, fmt="('(tr ', i0, ', a', i0, ', tr2, a)')") &
            lindent, max_len_label

    write (unit=llun, fmt=fmt_str) label%to_char(), str_status

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
