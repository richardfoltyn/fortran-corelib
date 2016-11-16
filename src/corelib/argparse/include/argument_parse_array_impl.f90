    class (argument), intent(in) :: self
    integer, intent(out) :: status
    character (100) :: msg

    integer :: i

    if (self%is_present) then
        if (self%action == ARGPARSE_ACTION_STORE) then
            do i = 1, self%nargs
                call self%passed_values(i)%parse (val(i), status)
                if (status /= STATUS_OK) then
                    msg = "Could not convert command line argument to requested type"
                    goto 100
                end if
            end do
        else if (self%action == ARGPARSE_ACTION_STORE_CONST) then
            ! need to interpret stored const in terms of output data type
            call dynamic_cast (self%const, ptr, status)
            if (status /= STATUS_OK) then
                msg = "Argument type incompatible with stored constant"
                goto 100
            end if
            val = ptr
        end if
    else if (allocated (self%default)) then
        call dynamic_cast (self%default, ptr, status)
        if (status /= STATUS_OK) then
            msg = "Argument type incompatible with stored default value"
            goto 100
        end if
        val = ptr
    end if

100 continue
    if (len(msg) > 0) write (ERROR_UNIT, fmt=*) msg
