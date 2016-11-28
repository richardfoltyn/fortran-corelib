module corelib_io_file

    use corelib_strings
    implicit none

    private

    !>  Returns true if specified path is a file.
    interface is_file
        module procedure is_file_str, is_file_char
    end interface

    public :: file_delete, is_file

contains

subroutine file_delete (path, status, msg)
    class (str), intent(in) :: path
    integer, intent(out), optional :: status
    class (str), intent(in out), optional :: msg

    integer :: unit, lstatus
    integer, parameter :: BUFSIZE = 100
    logical :: file_exists
    character (len=:), allocatable :: lmsg

    inquire (file=path%to_char(), exist=file_exists)

    if (present(msg)) allocate (character (BUFSIZE) :: lmsg)

    if (file_exists) then
        if (present(msg)) then
            open(newunit=unit, file=path%to_char(), status='old', &
                iostat=lstatus, iomsg=lmsg)
            if (lstatus == 0) close(unit, status='delete', &
                iostat=lstatus, iomsg=lmsg)
        else
            open(newunit=unit, file=path%to_char(), status='old', &
                iostat=lstatus)
            if (lstatus == 0) close(unit, status='delete', iostat=lstatus)
        end if
    else
        lmsg = "File does not exist"
        lstatus = 0
    end if

    if (present(msg)) msg = trim(lmsg)
    if (present(status)) status = lstatus

end subroutine

! ------------------------------------------------------------------------------
! IS_FILE function

function is_file_str (path) result(res)
    class (str), intent(in) :: path
        !!  File path
    logical :: res
        !!  Return value

    res = .false.
    inquire (file=path%to_char(), exist=res)
end function

function is_file_char (path) result(res)
    character (*), intent(in) :: path
    logical :: res

    res = is_file (str(path))
end function

end module
