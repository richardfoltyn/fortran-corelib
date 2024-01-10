
module fcore_char
    !*  Module implements common library routines that operate on charater
    !   variables.

    implicit none
    private

    public :: upper

    contains

pure function upper (char) result(uchar)
    !*  UPPER converts the given character value to upper case (only supports
    !   ASCII character values).
    character (*), intent(in) :: char
    character (len=len(char)) :: uchar

    integer :: i, j, lb, ub, offset

    lb = iachar ("a")
    ub = iachar ("z")
    offset = iachar ("A")

    uchar = char

    do i = 1, len(char)
        j = iachar (char(i:i))
        if (j >= lb .and. j <= ub) then
            uchar(i:i) = achar (offset + j - lb)
        end if
    end do

end function

end module
