! gfortran up to v5.x incorrectly processes procedure calls if the actual argument
! is a temporary array of user-derived type, and the dummy argument is
! polymorphic; see https://gcc.gnu.org/bugzilla/show_bug.cgi?id=60322
#if __GFORTRAN__ && (__GNUC__  < 6)
#define _POLYMORPHIC_ARRAY(t) type (t)
#else
#define _POLYMORPHIC_ARRAY(t) class (t)
#endif

module corelib_io_path

    use corelib_strings

    implicit none
    private

    interface join_path
        module procedure join_path_array, &
            join_path2_str, join_path3_str, join_path4_str, &
            join_path2_char, join_path3_char, join_path4_char, &
            join_path2_char_str, join_path3_char_str, join_path4_char_str
    end interface

    public :: join_path

contains

! ------------------------------------------------------------------------------
! JOIN_PATH functions

pure subroutine join_path_array (str_list, res)
    _POLYMORPHIC_ARRAY (str), dimension(:), intent(in) :: str_list
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

pure subroutine join_path2_str (p1, p2, res)
    class (str), intent(in) :: p1, p2
    class (str), intent(in out) :: res
    type (str), dimension(2) :: str_list

    str_list(1) = p1
    str_list(2) = p2

    call join_path_array (str_list, res)
end subroutine

pure subroutine join_path3_str (p1, p2, p3, res)
    class (str), intent(in) :: p1, p2, p3
    class (str), intent(in out) :: res
    type (str), dimension(3) :: str_list

    str_list(1) = p1
    str_list(2) = p2
    str_list(3) = p3

    call join_path_array (str_list, res)
end subroutine

pure subroutine join_path4_str (p1, p2, p3, p4, res)
    class (str), intent(in) :: p1, p2, p3, p4
    class (str), intent(in out) :: res
    type (str), dimension(4) :: str_list

    str_list(1) = p1
    str_list(2) = p2
    str_list(3) = p3
    str_list(4) = p4

    call join_path_array (str_list, res)
end subroutine

pure subroutine join_path2_char (p1, p2, res)
    character (*), intent(in) :: p1, p2
    character (*), intent(out) :: res

    type (str), dimension(2) :: str_list
    type (str) :: str_res

    str_list(1) = p1
    str_list(2) = p2

    call join_path_array (str_list, str_res)
    ! convert back to char
    res = str_res
end subroutine

pure subroutine join_path3_char (p1, p2, p3, res)
    character (*), intent(in) :: p1, p2, p3
    character (*), intent(out) :: res

    type (str), dimension(3) :: str_list
    type (str) :: str_res

    str_list(1) = p1
    str_list(2) = p2
    str_list(3) = p3

    call join_path_array (str_list, str_res)
    ! convert back to char
    res = str_res
end subroutine

pure subroutine join_path4_char (p1, p2, p3, p4, res)
    character (*), intent(in) :: p1, p2, p3, p4
    character (*), intent(out) :: res

    type (str), dimension(4) :: str_list
    type (str) :: str_res

    str_list(1) = p1
    str_list(2) = p2
    str_list(3) = p3
    str_list(4) = p4

    call join_path_array (str_list, str_res)
    ! convert back to char
    res = str_res
end subroutine

pure subroutine join_path2_char_str (p1, p2, res)
    character (*), intent(in) :: p1, p2
    class (str), intent(in out) :: res

    type (str), dimension(2) :: str_list

    str_list(1) = p1
    str_list(2) = p2

    call join_path_array (str_list, res)
end subroutine

pure subroutine join_path3_char_str (p1, p2, p3, res)
    character (*), intent(in) :: p1, p2, p3
    class (str), intent(in out) :: res

    type (str), dimension(3) :: str_list

    str_list(1) = p1
    str_list(2) = p2
    str_list(3) = p3

    call join_path_array (str_list, res)
end subroutine

pure subroutine join_path4_char_str (p1, p2, p3, p4, res)
    character (*), intent(in) :: p1, p2, p3, p4
    class (str), intent(in out) :: res

    type (str), dimension(4) :: str_list

    str_list(1) = p1
    str_list(2) = p2
    str_list(3) = p3
    str_list(4) = p4

    call join_path_array (str_list, res)
end subroutine

end module
