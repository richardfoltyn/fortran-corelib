! Method implementation for PARSE method (return type independent part)

class (argument), intent(in), target :: self
integer, intent(out) :: status
class (str), intent(in out) :: msg
class (*), pointer :: ptr_stored

nullify (ptr, ptr_stored)
status = STATUS_OK

if (self%is_present) then
    select case (self%action)
    case (ARGPARSE_ACTION_STORE_CONST)
        ptr_stored => self%const(1)
    case default
        call self%passed_values(1)%parse (val, status)

        if (status /= STATUS_OK) then
            status = STATUS_INVALID_STATE
            msg = "Could not convert command line argument to requested type"
            return
        end if

        return
    end select
else if (allocated (self%default)) then
    ptr_stored => self%default(1)
end if

if (.not. associated(ptr_stored)) then
    status = STATUS_INVALID_STATE
    msg = "Argument not present and no default value provided"
    return
end if

! at this point we need to retrieve and convert the value stored in either
! const or default
call dynamic_cast (ptr_stored, ptr, status)
if (status == STATUS_OK) then
    val = ptr
else
    msg = "Argument type incompatible with stored value"
end if
