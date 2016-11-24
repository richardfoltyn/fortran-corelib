module corelib_common_status

    use iso_fortran_env
    implicit none

    integer, parameter :: STATUS_MSG_LEN
    integer, parameter :: STATUS_CODE_KIND = int32

    integer (STATUS_CODE_KIND), parameter :: STATUS_OK = 0
    integer (STATUS_CODE_KIND), parameter :: STATUS_INVALID_INPUT = 2 ** 10
    integer (STATUS_CODE_KIND), parameter :: STATUS_UNKNOWN = 2 ** 11
    integer (STATUS_CODE_KIND), parameter :: STATUS_UNSUPPORTED_OPERATION = 2 * 12
    integer (STATUS_CODE_KIND), parameter :: STATUS_INVALID_STATE = 2 ** 13

    type, public :: status
        integer (int64) :: code
        character (MSG_LEN) :: msg
    contains
        procedure, pass :: set => status_set
        procedure, pass :: get => status_get
    end type

contains

pure subroutine status_set_int64 (self, code, msg)
    class (status), intent(in out) :: self
    integer (int64), intent(in), optional :: code
    character (*), intent(in), optional :: msg

    if (present(code)) self%code = code
    if (present(msg)) self%msg = msg
end subroutine

pure subroutine status_set_int32 (self, code, msg)
    class (status), intent(in out) :: self
    integer (int32), intent(in), optional :: code
    character (*), intent(in), optional :: msg

    if (present(code)) self%code = code
    if (present(msg)) self%msg = msg
end subroutine

pure subroutine status_get_int64 (self, code, msg)
    class (status), intent(in) :: self
    integer (int64), intent(out), optional :: code
    character (*), intent(out), optional :: msg

    if (present(code)) code = self%code
    if (present(msg)) msg = self%msg
end subroutine

pure subroutine status_get_int32 (self, code, msg)
    class (status), intent(in) :: self
    integer (int32), intent(out), optional :: code
    character (*), intent(out), optional :: msg

    if (present(code)) code = int(self%code, int32)
    if (present(msg)) msg = self%msg
end subroutine

end module
