! Implementation for PARSE method (array return types)

class (argument), intent(in), target :: self
integer, intent(out) :: status
character (*) , intent(out), optional :: msg
class (*), dimension(:), pointer :: ptr_stored

integer :: i

nullify (ptr, ptr_stored)

if (self%is_present) then
    select case (self%action)
    case (ARGPARSE_ACTION_STORE_CONST)
        ptr_stored => self%const
    case default
        do i = 1, self%get_nvals()
            call self%passed_values(i)%parse (val(i), status)
            if (status /= STATUS_OK) then
                if (present(msg)) &
                    msg = "Could not convert command line argument to requested type"
                goto 100
            end if
        end do
        return
    end select
else if (allocated (self%default)) then
    ptr_stored => self%default
end if

if (associated (ptr_stored)) then
    call dynamic_cast (self%default, ptr, status)
    if (status == STATUS_OK) then
        val = ptr
    else
        if (present(msg)) msg = "Argument type incompatible with stored value"
    end if
else
    status = STATUS_INVALID_STATE
    if (present(msg)) msg = "Argument not present and no default value provided"
end if

100 continue
