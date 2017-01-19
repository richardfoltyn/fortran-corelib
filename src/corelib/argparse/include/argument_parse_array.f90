! Implementation for PARSE method (array return types)

class (argument), intent(in), target :: self
integer (ENUM_KIND), intent(out) :: status
class (str), intent(in out) :: msg
class (*), dimension(:), pointer :: ptr_stored

integer :: i

nullify (ptr, ptr_stored)
status = STATUS_OK

if (self%is_present) then
    select case (self%action)
    case (ARGPARSE_ACTION_STORE_CONST)
        ptr_stored => self%const
    case default
        do i = 1, self%get_nvals()
            call self%passed_values(i)%parse (val(i), status)
            if (status /= STATUS_OK) then
                status = STATUS_INVALID_STATE
                msg = "Could not convert command line argument to requested type"
                return
            end if
        end do
        return
    end select
else if (allocated (self%default)) then
    ptr_stored => self%default
end if

if (.not. associated(ptr_stored)) then
    status = STATUS_INVALID_STATE
    msg = "Argument not present and no default value provided"
    return
end if

call dynamic_cast (self%default, ptr, status)
if (status == STATUS_OK) then
    val = ptr
else
    msg = "Argument type incompatible with stored value"
end if
