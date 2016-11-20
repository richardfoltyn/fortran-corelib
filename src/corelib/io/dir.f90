module corelib_io_dir

    use corelib_io_path
    use corelib_strings

    implicit none
    private

    interface dir_exists
        module procedure dir_exists_str, dir_exists_char
    end interface

    public :: dir_exists

contains

! ------------------------------------------------------------------------------
! DIR_EXISTS functions
function dir_exists_str (path) result(res)
    class (str), intent(in) :: path
    logical :: res

    type (str) :: filename, ftmp
    real (real64) :: rnd
    integer :: f_unit, iostat, i
    integer, parameter :: RND_LEN = 10, MAX_TRIES = 10
    character (len=100) :: iomsg
    logical :: file_exists

    res = .true.

    ! find a random file name that does not actually exist in target
    ! directory
    do i = 1, MAX_TRIES
        call random_number (rnd)
        ! create random 10-character sequence
        filename = "__" // str(int(rnd * (10.d0 ** RND_LEN))) // ".tmp"
        call join_path (path, filename, ftmp)

        inquire (file=ftmp%to_char(), exist=file_exists)

        ! if file exists by pure coincidence then of course the directory must
        ! exist as well :)
        if (file_exists) then
            return
        else
            exit
        end if
    end do

    ! at this point filepath contains a path to a file that does not yet exist
    open (newunit=f_unit, file=ftmp%to_char(), status="new", action="write", &
        iostat=iostat, iomsg=iomsg)

    if (iostat /= 0) res = .false.

    ! delete temporary file
    close (unit=f_unit, status="delete")

end function

function dir_exists_char (path) result(res)
    character (*), intent(in) :: path
    logical :: res

    type (str) :: str_path

    str_path = path
    res = dir_exists (str_path)
end function

end module
