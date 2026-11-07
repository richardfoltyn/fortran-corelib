module finalization_test_support
    !* Track finalization of payloads owned by linked-list nodes.

    use iso_fortran_env, only: int32

    implicit none
    private

    public :: tracked_item, finalizations

    integer(int32) :: finalizations(3) = 0

    type :: tracked_item
        integer(int32) :: id = 0
        integer(int32), allocatable :: data(:)
    contains
        final :: finalize_item
    end type

contains

subroutine finalize_item (self)
    !* Record finalization before the payload's automatic deallocation.

    type(tracked_item), intent(inout) :: self
        !* Payload being finalized.

    if (allocated(self%data)) then
        finalizations(self%id) = finalizations(self%id) + 1
    end if
end subroutine

end module

program test_list_finalization

    use finalization_test_support, only: tracked_item, finalizations
    use fcore_collections, only: linked_list, size

    implicit none

    call test_clear ()
    call test_assignment ()
    call test_list_deallocation ()

contains

subroutine test_clear ()
    !* Clearing releases every contained object exactly once and permits reuse.

    type(linked_list) :: list
    type(tracked_item) :: item

    finalizations = 0
    item%id = 1
    item%data = [1, 2]

    call list%clear ()
    if (size(list) /= 0 .or. any(finalizations /= 0)) error stop "Clear empty list"

    call list%append (item)
    call list%append (item)
    call list%clear ()
    if (size(list) /= 0) error stop "Clear populated list length"
    if (finalizations(1) /= 2) error stop "Clear must finalize both payloads"
    if (.not. allocated(item%data)) error stop "Clear must preserve source object"

    call list%clear ()
    if (finalizations(1) /= 2) error stop "Repeated clear must not finalize again"

    call list%append (item)
    call list%clear ()
    if (size(list) /= 0 .or. finalizations(1) /= 3) error stop "Reuse cleared list"
end subroutine

subroutine test_assignment ()
    !* Assignment clears the destination while preserving independent copies.

    type(linked_list) :: source, destination
    type(tracked_item) :: item

    finalizations = 0
    item%data = [1]
    item%id = 1
    call source%append (item)
    item%id = 2
    call destination%append (item)

    destination = source
    if (finalizations(2) /= 1) error stop "Assignment must finalize old payload"
    if (finalizations(1) /= 0) error stop "Assignment must preserve source payload"

    call destination%clear ()
    if (finalizations(1) /= 1) error stop "Clear assigned copy"
    if (size(source) /= 1) error stop "Source must survive clearing assigned copy"
    call source%clear ()
    if (finalizations(1) /= 2) error stop "Clear assignment source"
end subroutine

subroutine test_list_deallocation ()
    !* Finalizing the list releases contained objects through the clear method.

    type(linked_list), allocatable :: list
    type(tracked_item) :: item

    finalizations = 0
    item%id = 3
    item%data = [1]
    allocate (list)
    call list%append (item)
    call list%append (item)
    deallocate (list)
    if (finalizations(3) /= 2) error stop "List finalizer must release both payloads"
end subroutine

end program
