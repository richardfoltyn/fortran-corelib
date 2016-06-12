module collection_mod

    implicit none
    private

    type, abstract :: collection
    contains
        procedure (iface_iter), deferred, public, pass :: get_iter
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
            class (iterator), intent(in out) :: self
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
    end interface

    public :: collection, iterator


end module
