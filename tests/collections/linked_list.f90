
program test_linked_list
    !* Test linked-list sizes, copying, and iteration.

    use fcore_common
    use fcore_collections

    implicit none

    call test_size ()
    call test_copy ()
    call test_iter ()

contains

subroutine test_size ()
    !* Check list sizes after appending, removing, and clearing elements.

    type (linked_list) :: list

    integer :: i, n

    n = size(list)
    if (n /= 0) error stop "A new list must be empty"

    do i = 1, 5
        call list%append (i)
        n = size(list)
        if (n /= i) &
            error stop "Appending an element must increase the list size by one"
    end do

    do i = 4, 0, -1
        call list%remove (1)
        n = size(list)
        if (n /= i) &
            error stop "Removing an element must decrease the list size by one"
    end do

    call list%extend ([1,2,3,4,5])
    call list%clear ()
    n = size (list)
    if (n /= 0) error stop "Clearing a list must remove all elements"
end subroutine

subroutine test_copy ()
    !* Check that assignment copies list values into independent objects.

    type (linked_list) :: src, dst
    type (str) :: item1, item2, item3
    class (*), pointer :: ptr_item_src, ptr_item_dst
    class (str), pointer :: ptr_str_src, ptr_str_dst
    logical, dimension(:), allocatable :: equal
    integer :: i

    item1 = "foo"
    item2 = "bar"
    item3 = "baz"

    ! Test with empty list
    dst = src
    if (size(dst) /= 0) &
        error stop "Copying an empty list must produce an empty list"
    call dst%clear ()

    ! Test with list containing three elements
    call src%append (item1)
    call src%append (item2)
    call src%append (item3)

    dst = src
    if (size(dst) /= size(src)) &
        error stop "A copied list must have the same size as its source"

    ! Check that all elements are equal
    allocate (equal(size(dst)), source=.false.)
    do i = 1, min(size(dst), size(src))
        ptr_item_src => src%item(i)
        ptr_item_dst => dst%item(i)

        call dynamic_cast (ptr_item_src, ptr_str_src)
        call dynamic_cast (ptr_item_dst, ptr_str_dst)

        equal(i) = (ptr_str_src == ptr_str_dst)
    end do

    if (.not. all(equal)) &
        error stop "A copied list must contain the same values as its source"

    ! Test that items have been copied by modifying source
    do i = 1, size(src)
        ptr_item_src => src%item(i)
        call dynamic_cast (ptr_item_src, ptr_str_src)
        ptr_str_src = ptr_str_src // "_src"
    end do

    equal = .true.
    do i = 1, min(size(dst), size(src))
        ptr_item_src => src%item(i)
        ptr_item_dst => dst%item(i)

        call dynamic_cast (ptr_item_src, ptr_str_src)
        call dynamic_cast (ptr_item_dst, ptr_str_dst)
        equal(i) = (ptr_str_src == ptr_str_dst)
    end do

    if (any(equal)) &
        error stop "Changing source values must leave the copied list unchanged"

end subroutine


subroutine test_iter ()
    !* Check iterator values and positions for empty and populated lists.

    integer, dimension(:), allocatable :: dat, counter
    logical, dimension(:), allocatable :: equal
    class (iterator), allocatable :: iter
    class (*), pointer :: ptr_item
    integer, pointer :: ptr_int
    integer :: i, n

    type (linked_list) :: list

    ! Test empty list
    call list%get_iter (iter)
    n = 0
    do while (iter%has_next())
        n = n + 1
    end do

    if (n /= 0) error stop "An empty list must yield no iterator items"

    ! Test on some non-empty collection
    allocate (dat(5))
    dat = [(i, i = 1, size(dat))]

    call list%clear ()
    call list%extend (dat)

    ! Check that iterator returns the correct items and
    ! the correct running index
    allocate (counter(size(dat)), source=0)
    allocate (equal(size(dat)), source=.false.)

    ! Manually keep track of running index
    i = 0
    call list%get_iter(iter)
    do while (iter%has_next())
        i = i + 1
        counter(i) = iter%counter ()
        ptr_item => iter%item()
        call dynamic_cast (ptr_item, ptr_int)
        equal(i) = (ptr_int == dat(i))
    end do

    if (.not. all(equal)) &
        error stop "An iterator must return each list value in order"

    if (.not. all(counter == dat)) &
        error stop "An iterator must report consecutive item positions"

end subroutine
end
