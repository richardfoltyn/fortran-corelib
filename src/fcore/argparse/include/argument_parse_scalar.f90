! Method implementation for PARSE method (return type independent part)

class (argument), intent(in), target :: self
type (status_t), intent(out) :: status
class (*), dimension(:), pointer :: ptr_stored

nullify (ptr, ptr_stored)
call status%clear ()
status = FC_STATUS_OK

if (self%is_present) then
    select case (self%action)
    case (ARGPARSE_ACTION_STORE_CONST)
        ptr_stored => self%const
    case (ARGPARSE_ACTION_TOGGLE)
        ptr_stored => self%const
    case default
        call self%passed_values(1)%parse (val, status)

        if (status /= FC_STATUS_OK) then
            status = FC_STATUS_INVALID_STATE
            status%msg = "Could not convert command line argument to requested type"
            return
        end if

        return
    end select
else if (allocated (self%default)) then
    ptr_stored => self%default
end if

if (.not. associated(ptr_stored)) then
    status = FC_STATUS_INVALID_STATE
    status%msg = "Argument not present and no default value provided"
    return
end if

! at this point we need to retrieve and convert the value stored in either
! const or default
call dynamic_cast (ptr_stored, ptr, status)
if (status == FC_STATUS_OK) then
    val = ptr(1)
else
    status = FC_STATUS_UNSUPPORTED_OP
    status%msg = "Argument type incompatible with stored value"
end if
