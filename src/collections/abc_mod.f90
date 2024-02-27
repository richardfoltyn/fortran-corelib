! Module contains abstract base classes and interfaces shared by
! collection classes.

module fcore_collections_abc_mod

    implicit none
    private

    public :: collection, iterator
    public :: len, size

    type, abstract :: collection
    contains
        procedure (iface_iter), deferred, public, pass :: get_iter
        procedure (iface_length), deferred, public, pass :: length
    end type

    type, abstract :: iterator
    contains
        procedure (iface_next), public, deferred, pass :: has_next
        procedure (iface_item), public, deferred, pass :: item
        procedure (iface_counter), public, deferred, pass :: counter
    end type

    abstract interface
        function iface_next (self) result(res)
            import iterator
            class (iterator), intent(inout) :: self
            logical :: res
        end function

        function iface_item (self) result(res)
            import iterator
            class (iterator), intent(in) :: self
            class (*), pointer :: res
        end function

        function iface_counter (self) result(res)
            import iterator
            class (iterator), intent(in) :: self
            integer :: res
        end function
    end interface

    abstract interface
        subroutine iface_iter (self, iter)
            import collection
            import iterator
            class (collection), intent(in) :: self
            class (iterator), intent(out), allocatable :: iter
        end subroutine

        pure function iface_length (self) result(res)
            import collection
            class (collection), intent(in) :: self
            integer :: res
        end function
    end interface

    interface len
        module procedure len_collection
    end interface

    interface size
        module procedure len_collection
    end interface

contains

pure function len_collection (obj) result(res)
    class (collection), intent(in) :: obj
    integer :: res

    res = obj%length ()
end function

end module
