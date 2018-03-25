
#include <fcore.h>

module fcore_io_path

    use fcore_common

    implicit none
    private

    interface join_path
        module procedure join_path_array_str, &
            join_path2_str, join_path3_str, join_path4_str, &
            join_path2_char_str, join_path3_char_str, join_path4_char_str
    end interface

    public :: join_path

contains

! ------------------------------------------------------------------------------
! JOIN_PATH functions

pure subroutine join_path_impl (str_list, res)
    __FCORE_POLY_ARRAY (str), dimension(:), intent(in) :: str_list
    type (str), intent(in out) :: res

    class (str), dimension(:), allocatable :: components
    type (str) :: path_sep

    integer :: n, i
    n = size(str_list)

    if (n > 1) then
        ! create local copy of path components
        allocate (components(n), source=str_list)
        ! strip any trailing path separators from all but last component
        do i = 1, n-1
            if (components(i)%endswith("/") .or. components(i)%endswith("\")) then
                components(i) = components(i)%substring (1, -2)
            end if
        end do

        ! strip any leading path separators from all but first component
        do i = 2, n
            if (components(i)%startswith("/") .or. components(i)%startswith("\")) then
                components(i) = components(i)%substring (2, -1)
            end if
        end do

        path_sep = "/"
        res = path_sep%join (components)
    else
        res = str_list(1)
    end if
end subroutine

pure function join_path_array_str (str_list) result(res)
    __FCORE_POLY_ARRAY (str), dimension(:), intent(in) :: str_list
    type (str) :: res

    call join_path_impl (str_list, res)
end function

pure function join_path2_str (p1, p2) result(res)
    class (str), intent(in) :: p1, p2
    type (str) :: res
    type (str), dimension(2) :: str_list

    str_list(1) = p1
    str_list(2) = p2

    call join_path_impl (str_list, res)
end function

pure function join_path3_str (p1, p2, p3) result(res)
    class (str), intent(in) :: p1, p2, p3
    type (str) :: res
    type (str), dimension(3) :: str_list

    str_list(1) = p1
    str_list(2) = p2
    str_list(3) = p3

    call join_path_impl (str_list, res)
end function

pure function join_path4_str (p1, p2, p3, p4) result(res)
    class (str), intent(in) :: p1, p2, p3, p4
    type (str) :: res
    type (str), dimension(4) :: str_list

    str_list(1) = p1
    str_list(2) = p2
    str_list(3) = p3
    str_list(4) = p4

    call join_path_impl (str_list, res)
end function

pure function join_path2_char_str (p1, p2) result(res)
    character (*), intent(in) :: p1, p2
    type (str) :: res

    type (str), dimension(2) :: str_list

    str_list(1) = p1
    str_list(2) = p2

    call join_path_impl (str_list, res)
end function

pure function join_path3_char_str (p1, p2, p3) result(res)
    character (*), intent(in) :: p1, p2, p3
    type (str) :: res

    type (str), dimension(3) :: str_list

    str_list(1) = p1
    str_list(2) = p2
    str_list(3) = p3

    call join_path_impl (str_list, res)
end function

pure function join_path4_char_str (p1, p2, p3, p4) result(res)
    character (*), intent(in) :: p1, p2, p3, p4
    type (str) :: res

    type (str), dimension(4) :: str_list

    str_list(1) = p1
    str_list(2) = p2
    str_list(3) = p3
    str_list(4) = p4

    call join_path_impl (str_list, res)
end function

end module
