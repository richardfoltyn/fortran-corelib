! Implementation for PARSE method (array return types)

class (argument), intent(in), target :: self
type (status_t), intent(out) :: status
class (*), dimension(:), pointer :: ptr_stored

integer :: i

nullify (ptr, ptr_stored)
call status%clear ()
status = CL_STATUS_OK

if (self%is_present) then
    select case (self%action)
    case (ARGPARSE_ACTION_STORE_CONST)
        ptr_stored => self%const
    case default
        do i = 1, self%get_nvals()
            call self%passed_values(i)%parse (val(i), status)
            if (status /= CL_STATUS_OK) then
                status = CL_STATUS_INVALID_STATE
                status%msg = "Could not convert command line argument to requested type"
                return
            end if
        end do
        return
    end select
else if (allocated (self%default)) then
    ptr_stored => self%default
end if

if (.not. associated(ptr_stored)) then
    status = CL_STATUS_INVALID_STATE
    status%msg = "Argument not present and no default value provided"
    return
end if

call dynamic_cast (self%default, ptr, status)
if (status == CL_STATUS_OK) then
    val = ptr
else
    status%msg = "Argument type incompatible with stored value"
end if
