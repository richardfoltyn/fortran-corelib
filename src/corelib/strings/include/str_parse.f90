class (str), intent(in) :: self
integer, optional :: status

intent(out) :: val, status

integer :: lstatus
lstatus = STR_PARSE_INVALID

if (allocated(self%value)) then
    if (len(self) > 0) then
        read (unit=self%value, fmt=*, iostat=lstatus) val
        if (lstatus == 0) lstatus = STR_PARSE_SUCCESS
    end if
end if

if (present(status)) status = lstatus
