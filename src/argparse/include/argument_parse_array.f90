! Implementation for PARSE method (array return types)

class (argument), intent(in), target :: self
type (status_t), intent(out) :: status
class (*), dimension(:), pointer :: ptr_stored

integer :: i

nullify (ptr, ptr_stored)
call status%clear ()
status = FC_STATUS_OK

if (self%is_present) then
    select case (self%action)
    case (ARGPARSE_ACTION_STORE_CONST)
        ptr_stored => self%const%data_array
    case default

        if (size(val) < self%get_nvals()) then
            status = FC_STATUS_VALUE_ERROR
            status%msg = "Array size insufficient to store value(s)"
            return
        end if

        do i = 1, self%get_nvals()
            call self%passed_values(i)%parse (val(i), status)
            if (status /= FC_STATUS_OK) then
                status = FC_STATUS_INVALID_STATE
                status%msg = "Could not convert command line argument to requested type"
                return
            end if
        end do
        return
    end select
else if (allocated (self%default%data_array)) then
    ptr_stored => self%default%data_array
end if

if (.not. associated(ptr_stored)) then
    status = FC_STATUS_INVALID_STATE
    status%msg = "Argument not present and no default value provided"
    return
end if

call dynamic_cast (ptr_stored, ptr, status)
if (status == FC_STATUS_OK) then
    val = ptr
else
    status%msg = "Argument type incompatible with stored value"
end if
