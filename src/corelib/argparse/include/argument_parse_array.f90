! Implementation for PARSE method (array return types)

class (argument), intent(in), target :: self
integer, intent(out) :: status
character (*) , intent(out), optional :: msg
class (*), dimension(:), pointer :: ptr_stored

integer :: i


if (self%is_present) then
    if (self%action == ARGPARSE_ACTION_STORE) then
        do i = 1, self%nargs
            call self%passed_values(i)%parse (val(i), status)
            if (status /= STATUS_OK) then
                if (present(msg)) &
                    msg = "Could not convert command line argument to requested type"
                goto 100
            end if
        end do

        return

    else if (self%action == ARGPARSE_ACTION_STORE_CONST) then
        ptr_stored => self%const
    end if
else if (allocated (self%default)) then
    ptr_stored => self%default
end if

call dynamic_cast (self%default, ptr, status)
if (status == STATUS_OK) then
    val = ptr
else
    if (present(msg)) msg = "Argument type incompatible with stored value"
end if

100 continue
