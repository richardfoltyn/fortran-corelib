module fcore_collections_linked_list_mod

    use fcore_collections_abc_mod
    use fcore_common_base
    use fcore_common_status_helpers

    implicit none
    private

    public :: linked_list

    type, extends(iterator) :: linked_list_iterator
        private

        integer :: progress = 0
        class (linked_list), pointer :: ptr_list => null()
        class (list_node),  pointer :: ptr_current => null()
    contains
        procedure, public, pass :: has_next => iterator_next
        procedure, public, pass :: item => iterator_item
        procedure, public, pass :: counter => iterator_counter
        procedure, pass :: node => iterator_node

        procedure, public, pass :: initialize => iterator_initialize
    end type

    type :: list_node
        private

        class (*), allocatable :: item_obj
        type (list_node), pointer :: ptr_next => null()
        type (list_node), pointer :: ptr_prev => null()

    contains
        procedure, public, pass :: item => node_item
        ! procedure, public, pass :: next => node_next
        ! procedure, public, pass :: prev => node_prev

        procedure, pass :: node_assign
        generic, public :: assignment (=) => node_assign

        final :: node_finalize
    end type

    type, extends(collection) :: linked_list
        private

        type (list_node), pointer :: ptr_first => null(), ptr_last => null()
        integer :: n = 0

    contains
        procedure, public, pass :: length => list_length

        procedure, public, pass :: append => list_append
        
        procedure, pass :: extend_array => list_extend_array
        generic, public :: extend => extend_array

        procedure, public, pass :: remove => list_remove
        procedure, public, pass :: insert => list_insert
        procedure, public, pass :: clear => list_clear

        procedure, public, pass :: item => list_get_item
        procedure, pass :: node => list_get_node

        procedure, pass :: list_assign
        generic, public :: assignment (=) => list_assign

        ! implement deferred procedure from collection::iter()
        procedure, public, pass :: get_iter => list_iter

        final :: list_finalize

    end type

    interface dynamic_cast
        module procedure cast_any_to_list_node
    end interface

    interface dynamic_cast
        module procedure cast_iterator_to_linked_list_iterator
    end interface

contains


! *****************************************************************************
! LIST_NODE assignment
subroutine node_assign (self, rhs)
    ! self will be finalized automatically since it's intent(out)
    class (list_node), intent(out) :: self
    class (list_node), intent(in) :: rhs

    self%ptr_next => rhs%ptr_next
    self%ptr_prev => rhs%ptr_prev

    allocate (self%item_obj, source=rhs%item_obj)

end subroutine

! *****************************************************************************
! LIST_NODE attributes

!function node_next (self) result(res)
!    class (list_node), intent(in) :: self
!    class (list_node), pointer :: res
!
!    res => self%ptr_next
!end function
!
!function node_prev (self) result(res)
!    class (list_node), intent(in) :: self
!    class (list_node), pointer :: res
!
!    res => self%ptr_prev
!end function

function node_item (self) result(res)
    class (list_node), intent(in), target :: self
    class (*), pointer :: res

    nullify (res)
    if (allocated(self%item_obj)) res => self%item_obj

end function


! *****************************************************************************
! LIST_NODE finalizer

recursive subroutine node_finalize (self)
   type (list_node), intent(in out) :: self

   if (allocated(self%item_obj)) deallocate (self%item_obj)
end subroutine


! ******************************************************************************
! LIST_NODE casts

subroutine cast_any_to_list_node (obj, ptr, status)
    class (*), intent(in), target :: obj
    class (list_node), pointer, intent(out) :: ptr
    type (status_t), intent(out), optional :: status

    call status_set_ok (status)
    nullify (ptr)

    select type (obj)
    class is (list_node)
        ptr => obj
    class default
        call status_set_cast_error (status, 'list_node')
    end select
end subroutine

! *****************************************************************************
! LIST ASSIGNMENT

subroutine list_assign (self, rhs)
    class (linked_list), intent(in out) :: self
    class (collection), intent(in) :: rhs

    class (iterator), allocatable :: rhs_iter
    class (*), pointer :: ptr_item


    ! get rid of whatever is contained in list at the moment
    call self%clear ()

    ! obtain iterator for rhs list
    call rhs%get_iter (rhs_iter)

    do while (rhs_iter%has_next())
        ptr_item => rhs_iter%item ()
    
        call self%append (ptr_item)
    end do

end subroutine

! *****************************************************************************
! ATTRIBUTES
pure function list_length (self) result(res)
    class (linked_list), intent(in) :: self
    integer :: res

    res = self%n
end function


! *****************************************************************************
! ACCESSORT METHODS

function list_get_item (self, i) result(res)
    class (linked_list), intent(in) :: self
    class (*), pointer :: res
    class (list_node), pointer :: node
    integer, intent(in) :: i

    res => null()

    node => self%node (i)
    if (associated(node)) res => node%item()

end function

function list_get_node (self, i) result (res)
    class (linked_list), intent(in) :: self
    class (list_node), pointer :: res
    integer, intent(in) :: i

     class (iterator), allocatable :: iter
     class (linked_list_iterator), pointer :: ptr_iter

     res => null()

    ! nothing to do for empty list
    if (i < 1 .or. i > self%n) return

    ! obtain item iterator
    call self%get_iter (iter)
    ! cast to linked_list_iterator subtype to access attributes
    call dynamic_cast (iter, ptr_iter)

    do while (iter%has_next())
        if (iter%counter() == i) then
            res => ptr_iter%node()
            return
        end if
    end do

end function

! *****************************************************************************
! INSERT methods

subroutine list_insert (self, item, i)
    class (linked_list), intent(in out) :: self
    class (*), intent(in) :: item
    integer, intent(in) :: i

    class (list_node), pointer :: node
    ! ptr_old points to old node element and position i
    type (list_node), pointer :: ptr_old

    ! allocate new node to be inserted and instantiate copy of contained data
    allocate (node)
    allocate (node%item_obj, source=item)

    nullify (ptr_old)

    if (self%n > 0) then
        if (i <= self%n) then
            ! locate current node at index i
            ptr_old = self%node(i)

            ! Establish links to node preceding ptr_old:
            ! If we insert at position on, this pointer will be null()
            node%ptr_prev => ptr_old%ptr_prev
            ! If i > 1, adjust target of ptr_next in previous element
            if (associated(node%ptr_prev)) node%ptr_prev%ptr_next => node

            ! establish links to old node
            node%ptr_next => ptr_old
            ptr_old%ptr_prev => node

            ! set pointer to first list item
            if (i == 1) self%ptr_first => node
            ! set pointer to last list item
            if (i == self%n) self%ptr_last => ptr_old
        else
            ! insert at the end
            self%ptr_last%ptr_next => node
            node%ptr_prev => self%ptr_last
            self%ptr_last => node
        end if
    else
        ! list is empty, insert as first element
        self%ptr_first => node
        self%ptr_last => node
        ! ptr_prev and ptr_next remain nullified in this case
    end if

    self%n = self%n + 1
end subroutine

! *****************************************************************************
! APPEND methods

subroutine list_append(self, item)
    class (linked_list), intent(in out) :: self
    class (*), intent(in) :: item

    call self%insert (item, self%n + 1)
end subroutine

! *****************************************************************************
! EXTEND methods

subroutine list_extend_array (self, items)
    class (linked_list), intent(in out) :: self
    class (*), intent(in), dimension(:) :: items

    integer :: n, i

    n = size(items)

    do i = 1, n
        call self%insert (items(i), self%n + 1)
    end do

end subroutine


! *****************************************************************************
! Remove methods
recursive subroutine list_remove (self, i)
    class (linked_list), intent(in out) :: self
    integer, intent(in) :: i

    ! pointer to node that is to be removed
    type (list_node), pointer :: ptr_remove

    ! nothing to do, quietly ignore
    if (i > self%n .or. i < 1) return

    ptr_remove => self%node (i)

    if (associated(ptr_remove%ptr_prev) .and. associated(ptr_remove%ptr_next)) then
        ptr_remove%ptr_prev%ptr_next => ptr_remove%ptr_next
        ptr_remove%ptr_next%ptr_prev => ptr_remove%ptr_prev
    else
        ! removed element is the last list element (ptr_next is not associated)
        if (associated(ptr_remove%ptr_prev)) then
            ptr_remove%ptr_prev%ptr_next => null()
            self%ptr_last => ptr_remove%ptr_prev
        end if

        ! removed element is the first element in the linked list
        ! (ptr_prev is not associated)
        if (associated(ptr_remove%ptr_next)) then
            ptr_remove%ptr_next%ptr_prev => null()
            self%ptr_first => ptr_remove%ptr_next
        end if
    end if

    self%n = self%n - 1

    ! deallocate object and clean up (will deallocate item component automatically)
    deallocate (ptr_remove)

end subroutine

!-------------------------------------------------------------------------------
! CLEAR method

pure recursive subroutine list_clear (self)
    !*  LIST_CLEAR removes all elements contained in the list, deallocating
    !   any memory used in the process. The list length is reset to zero.

    class (linked_list), intent(in out) :: self

    type (list_node), pointer :: ptr_node, ptr_next

    nullify (ptr_node, ptr_next)

    ! traverse linked list, deallocate each element in turn
    ptr_node => self%ptr_first
    do while (associated(ptr_node))
        ptr_next => ptr_node%ptr_next
        deallocate (ptr_node)
        ptr_node => ptr_next
    end do

    nullify (self%ptr_first)
    nullify (self%ptr_last)

    self%n = 0
end subroutine


! *****************************************************************************
! LINKED_LIST iter

subroutine list_iter (self, iter)
    class (linked_list), intent(in) :: self
    class (iterator), intent(out), allocatable :: iter

    allocate ( linked_list_iterator :: iter)

    select type (obj => iter)
    class is (linked_list_iterator)
        call obj%initialize (self)
    end select

end subroutine

! *****************************************************************************
! LINKED_LIST finalizer

recursive subroutine list_finalize (self)
    type (linked_list), intent(in out) :: self

    call self%clear ()
end subroutine

! *****************************************************************************
! ITERATOR methods

subroutine iterator_initialize (self, lst)
    class (linked_list_iterator), intent(in out) :: self
    class (linked_list), intent(in), target :: lst

    self%ptr_list => lst
end subroutine

function iterator_next (self) result(res)
    class (linked_list_iterator), intent(in out) :: self
    logical :: res

    res = .false.

    ! at the beginning of the list, or list is empty
    if (.not. associated(self%ptr_current)) then
        self%ptr_current => self%ptr_list%ptr_first
        if (associated(self%ptr_current)) then
            self%progress = 1
            res = .true.
        end if
    else if (associated(self%ptr_current%ptr_next)) then
        self%ptr_current => self%ptr_current%ptr_next
        res = .true.
        self%progress = self%progress + 1
    end if

end function

function iterator_counter (self) result(res)
    class (linked_list_iterator), intent(in) :: self
    integer :: res

    res = self%progress
end function

function iterator_item (self) result(res)
    class (linked_list_iterator), intent(in) :: self
    class (*), pointer :: res

    res => null()
    if (associated(self%ptr_current)) then
        res => self%ptr_current%item()
    end if
end function

function iterator_node (self) result(res)
    class (linked_list_iterator), intent(in) :: self
    class (list_node), pointer :: res

    res => self%ptr_current
end function

subroutine cast_iterator_to_linked_list_iterator (obj, ptr, status)
    class (iterator), intent(in), target :: obj
    class (linked_list_iterator), pointer :: ptr
    type (status_t), intent(out), optional :: status

    call status_set_ok (status)
    nullify (ptr)

    select type (obj)
    class is (linked_list_iterator)
        ptr => obj
    class default
        call status_set_cast_error (status, 'linked_list_iterator')
    end select
end subroutine

end module
