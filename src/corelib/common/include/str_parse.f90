class (str), intent(in) :: self
type (status_t), optional :: status

intent(out) :: val, status

integer :: lstatus

if (present(status)) then
    call status%clear ()
    status = CL_STATUS_VALUE_ERROR
end if

lstatus = 0

if (allocated(self%value)) then
    if (len(self) > 0) then
        read (unit=self%value, fmt=*, iostat=lstatus) val
        if (present(status) .and. lstatus == 0) status = CL_STATUS_OK
    end if
end if
