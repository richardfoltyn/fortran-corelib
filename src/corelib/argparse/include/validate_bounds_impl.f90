
logical, intent(in), optional :: strict_lb, strict_ub
logical :: lstrict_lb, lstrict_ub, valid

lstrict_lb = .false.
lstrict_ub = .false.
if (present(strict_lb)) lstrict_lb = strict_lb
if (present(strict_ub)) lstrict_ub = strict_ub

call status%init (CL_STATUS_OK)

call val%parse (ival, status)
if (status /= CL_STATUS_OK) then
    status = CL_STATUS_VALUE_ERROR
    status%msg = "Invalid value; could not convert to specified data type"
    return
end if

if (present(lb)) then
    valid = (lstrict_lb .and. ival > lb) .or. (.not. lstrict_lb .and. ival >= lb)
    if (.not. valid) then
        status = CL_STATUS_VALUE_ERROR
        status%msg = "Value outside of permitted range"
        return
    end if
end if

if (present(ub)) then
    valid = (lstrict_ub .and. ival < ub) .or. (.not. lstrict_ub .and. ival <= ub)
    if (.not. valid) then
        status = CL_STATUS_VALUE_ERROR
        status%msg = "Value outside of permitted range"
        return
    end if
end if
