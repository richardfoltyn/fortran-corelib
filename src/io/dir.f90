module fcore_io_dir

    use, intrinsic :: iso_c_binding, only: C_CHAR, C_NULL_CHAR, C_INT

    use fcore_io_path
    use fcore_common


    implicit none
    private

    interface is_dir
        module procedure is_dir_str, is_dir_char
    end interface

    public :: is_dir

    interface
        function c_is_dir (name) result(res) bind(C, name='fcore_c_is_dir')
            import
            character (C_CHAR), intent(in) :: name(*)
            integer (C_INT) :: res
        end function
    end interface

contains

! ------------------------------------------------------------------------------
! DIR_EXISTS functions

function is_dir_char (path) result(res)
    character (*), intent(in) :: path
    logical :: res

    integer :: n
    integer (C_INT) :: c_res
    character (kind=C_CHAR,len=:), dimension(:), allocatable :: c_name
    integer :: i

    n = len(path)

    allocate ( character (1) :: c_name(n+1))
    do i = 1, n
        c_name(i) = path(i:i)
    end do
    c_name(n+1) = C_NULL_CHAR

    c_res = c_is_dir (c_name)
    res = (c_res /= 0)

    deallocate (c_name)

end function

function is_dir_str (path) result(res)
    class (str), intent(in) :: path
    logical :: res

    character (:), allocatable :: cpath
    integer :: n

    n = len(path)
    allocate (character (n) :: cpath)

    cpath = path%to_char()
    res = is_dir (cpath)

    deallocate (cpath)
end function

end module
