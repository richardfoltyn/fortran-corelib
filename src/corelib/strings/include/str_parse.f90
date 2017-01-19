class (str), intent(in) :: self
integer (ENUM_KIND), optional :: status

intent(out) :: val, status

integer (ENUM_KIND) :: lstatus
lstatus = STATUS_INVALID_INPUT

if (allocated(self%value)) then
    if (len(self) > 0) then
        read (unit=self%value, fmt=*, iostat=lstatus) val
        if (lstatus == 0) lstatus = STATUS_OK
    end if
end if

if (present(status)) status = lstatus
