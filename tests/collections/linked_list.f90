
program test_linked_list
    !*  Unit tests for linked_list implementation from
    !   FCorelib collections.

    use, intrinsic :: iso_fortran_env
    use fcore_testing
    use fcore_common
    use fcore_collections

    implicit none

    call test_all ()

contains

subroutine test_all ()

    type (test_suite) :: tests

    call tests%set_label ("linked_list unit tests")

    call test_size (tests)

    call test_copy (tests)

    call test_iter (tests)

    call tests%print ()
end subroutine


subroutine test_size (tests)
    !*  TEST_SIZE tests whether size() function works
    !   for linked_list.

    class (test_suite) :: tests
    class (test_case), pointer :: tc

    type (linked_list) :: list
    
    integer :: i, n

    tc => tests%add_test ("linked_list size tests")

    n = size(list)
    call tc%assert_true (n == 0, "Empty list, no elements added")

    do i = 1, 5
        call list%append (i)
        n = size(list)
        call tc%assert_true (n == i, "List with " // str(i) // " element(s)")
    end do

    do i = 4, 0, -1
        call list%remove (1)
        n = size(list)
        call tc%assert_true (n == i, "List with " // str(i) // " element(s)")
    end do

    call list%extend ([1,2,3,4,5])
    call list%clear ()
    n = size (list)
    call tc%assert_true (n == 0, "After calling CLEAR method")
end subroutine



subroutine test_copy (tests)
    !*  TEST_COPY tests copying of linked_list objects
    !   using the assignment operator.

    class (test_suite) :: tests
    class (test_case), pointer :: tc

    type (linked_list) :: src, dst
    type (str) :: item1, item2, item3
    class (*), pointer :: ptr_item_src, ptr_item_dst
    class (str), pointer :: ptr_str_src, ptr_str_dst
    logical, dimension(:), allocatable :: equal
    integer :: i

    tc => tests%add_test ("linked_list copy tests")

    item1 = "foo"
    item2 = "bar"
    item3 = "baz"

    ! Test with empty list
    dst = src
    call tc%assert_true (size(dst) == 0, "Copy empty src")
    call dst%clear ()

    ! Test with list containing three elements
    call src%append (item1)
    call src%append (item2)
    call src%append (item3)

    dst = src
    call tc%assert_true (size(dst) == size(src), &
        "size(src) = 3: check size(src) == size(dst)")

    ! Check that all elements are equal
    allocate (equal(size(dst)), source=.false.)
    do i = 1, min(size(dst), size(src))
        ptr_item_src => src%item(i)
        ptr_item_dst => dst%item(i)

        call dynamic_cast (ptr_item_src, ptr_str_src)
        call dynamic_cast (ptr_item_dst, ptr_str_dst)

        equal(i) = (ptr_str_src == ptr_str_dst)
    end do

    call tc%assert_true (all(equal), "size(src) = 3: check all items equal")

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

    call tc%assert_true (.not. any(equal), &
        "size(src) = 3: check src and dst items are diff. obj.")

end subroutine


subroutine test_iter (tests)
    !*  TEST_ITER tests functions of iterator protocol applied
    !   to linked_list.

    class (test_suite) :: tests
    class (test_case), pointer :: tc

    integer, dimension(:), allocatable :: dat, counter
    logical, dimension(:), allocatable :: equal
    class (iterator), allocatable :: iter
    class (*), pointer :: ptr_item
    integer, pointer :: ptr_int
    integer :: i, n

    type (linked_list) :: list

    tc => tests%add_test ("linked_list iterator tests")

    ! Test empty list
    call list%get_iter (iter)
    n = 0
    do while (iter%has_next())
        n = n + 1
    end do

    call tc%assert_true (n == 0, "iter on empty list")

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

    call tc%assert_true (all(equal), &
        "iter on size(list) = " // str(size(list)) // ": items test")

    call tc%assert_true (all(counter == dat), &
        "iter on size(list) = " // str(size(list)) // ": index test")

end subroutine
end
