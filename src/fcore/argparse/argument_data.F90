

module fcore_argparse_argument_data
    !*  Module implements type to store user-provided default or constant
    !   data associated with a command-line argument.
    !
    !   The underlying idea is to avoid having to pass arguments of type
    !       class (*), dimension(:), optional ...
    !   between routines, as this (1) makes creating unambiguous routine
    !   signitures with a lot of arguments difficult; and (2), it helps
    !   addressing the bugs related to polymorphic array arguments in
    !   gfortran 5 and lower.

    use fcore_common

    implicit none
    private

    public :: argument_data
    public :: argument_data_alloc
    public :: argument_data_alloc_char
    public :: argument_data_get_nvals
    public :: argument_data_reset
    public :: assignment (=)

    type :: argument_data
        class (*), dimension(:), allocatable :: data_array
        class (*), allocatable :: data_scalar
    end type

    interface argument_data_alloc
        procedure argument_data_alloc_array, argument_data_alloc_scalar
    end interface

    interface assignment (=)
        procedure argument_data_assign
    end interface

    contains



pure subroutine argument_data_alloc_array (self, x)
    type (argument_data), intent(inout) :: self
    class (*), intent(in), dimension(:) :: x

    integer :: n, i, k
    type (str), dimension(:), allocatable :: data_str

    if (allocated(self%data_array)) deallocate (self%data_array)

    n = size(x)
    select type (x)
    type is (character (*))
        k = len(x)
        allocate (data_str(n))
        do i = 1, n
            data_str(i) = x(i)
        end do
        call move_alloc (data_str, self%data_array)
    class default
        allocate (self%data_array(n), source=x)
    end select

end subroutine


pure subroutine argument_data_alloc_scalar (self, x)
    type (argument_data), intent(inout) :: self
    class (*), intent(in), optional :: x

    type (str) :: data_str

    if (allocated(self%data_scalar)) deallocate (self%data_scalar)

    if (present(x)) then
        select type (x)
        type is (character (*))
            data_str = x
            allocate (self%data_scalar, source=data_str)
        class default
            allocate (self%data_scalar, source=x)
        end select
    end if
end subroutine


subroutine argument_data_alloc_char (self, val)
    type (argument_data), intent(inout) :: self
    character (*), intent(in), dimension(:) :: val

    type (str), dimension(:), allocatable :: swork

    integer :: n, i

    if (allocated(swork)) deallocate (swork)

    n = size(val)
    allocate (swork(n))

    do i = 1, n
        swork(i) = val(i)
    end do

    call move_alloc (swork, self%data_array)

end subroutine


pure function argument_data_get_nvals (self) result(res)
    type (argument_data), intent(in) :: self
    integer :: res

    res = 0

    if (allocated(self%data_array)) then
        res = size(self%data_array)
    else if (allocated(self%data_scalar)) then
        res = 1
    end if
end function


pure subroutine argument_data_reset (self)
    type (argument_data), intent(inout) :: self

    if (allocated(self%data_array)) deallocate (self%data_array)
    if (allocated(self%data_scalar)) deallocate (self%data_scalar)
end subroutine


elemental subroutine argument_data_assign (lhs, rhs)
    type (argument_data), intent(inout) :: lhs
    type (argument_data), intent(in) :: rhs

    integer :: n

    if (allocated(lhs%data_array)) deallocate (lhs%data_array)
    if (allocated(lhs%data_scalar)) deallocate (lhs%data_scalar)

    if (allocated(rhs%data_array)) then
        n = size(rhs%data_array)
        allocate (lhs%data_array(n), source=rhs%data_array)
    end if

    if (allocated(rhs%data_scalar)) then
        allocate (lhs%data_scalar, source=rhs%data_scalar)
    end if

end subroutine


end module
