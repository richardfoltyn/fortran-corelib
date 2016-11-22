! Method implementation for PARSE method (return type independent part)

class (argument), intent(in), target :: self
integer, intent(out) :: status
character (*), intent(out), optional :: msg
class (*), pointer :: ptr_stored

nullify (ptr, ptr_stored)

if (self%is_present) then
    select case (self%action)
    case (ARGPARSE_ACTION_STORE_CONST)
        ptr_stored => self%const(1)
    case default
        call self%passed_values(1)%parse (val, status)

        if (status /= STATUS_OK) then
            if (present(msg)) &
                msg = "Could not convert command line argument to requested type"
            goto 100
        end if

        return
    end select
else if (allocated (self%default)) then
    ptr_stored => self%default(1)
end if

! at this point we need to retrieve and convert the value stored in either
! const or default
if (associated (ptr_stored)) then
    call dynamic_cast (ptr_stored, ptr, status)
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
