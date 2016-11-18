! Method implementation for PARSE method (return type independent part)

class (argument), intent(in), target :: self
integer, intent(out) :: status
character (*), intent(out), optional :: msg
class (*), pointer :: ptr_stored

if (self%is_present) then
    if (self%action == ARGPARSE_ACTION_STORE) then
        call self%passed_values(1)%parse (val, status)

        if (status /= STATUS_OK) then
            if (present(msg)) &
                msg = "Could not convert command line argument to requested type"
            goto 100
        end if

        return

    else if (self%action == ARGPARSE_ACTION_STORE_CONST) then
        ptr_stored => self%const(1)
    end if
else if (allocated (self%default)) then
    ptr_stored => self%default(1)
end if

! at this point we need to retrieve and convert the value stored in either
! const or default
call dynamic_cast (ptr_stored, ptr, status)
if (status == STATUS_OK) then
    val = ptr
else
    if (present(msg)) msg = "Argument type incompatible with stored value"
end if

100 continue
