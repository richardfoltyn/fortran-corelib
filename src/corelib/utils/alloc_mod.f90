module corelib_utils_alloc

    implicit none
    private

    interface assert_alloc
        module procedure assert_alloc_char
    end interface

    public :: assert_alloc
contains

pure subroutine assert_alloc_char (obj, nmin, nbuffer, copy)
    character (len=:), intent(in out), allocatable :: obj
    integer, intent(in) :: nmin
    integer, intent(in), optional :: nbuffer
    logical, intent(in), optional :: copy

    character (len=:), allocatable :: work
    logical :: lcopy
    integer :: n, lnbuffer, nalloc

    ! default values for optional arguments
    lcopy = .true.
    ! create additional buffer of 0 elements if size lower than nmin
    lnbuffer = 0
    if (present(copy)) lcopy = copy
    if (present(nbuffer)) lnbuffer = nbuffer

    ! silently ignore negative values
    lnbuffer = max(0, lnbuffer)

    ! object length when reallocation required
    nalloc = nmin + lnbuffer

    if (allocated(obj)) then
        n = len(obj)
        if (n < nmin) then
            ! copy contents to work array, reallocate to final object
            if (lcopy) then
                allocate (character (nalloc) :: work)
                work(1:n) = obj
                call move_alloc (work, obj)
            else
                deallocate (obj)
                allocate (character (nalloc) :: obj)
            end if
        end if
    else
        allocate (character (nalloc) :: obj)
    end if
end subroutine

end module
