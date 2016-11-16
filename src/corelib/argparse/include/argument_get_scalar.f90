class (argument), intent(in) :: self
integer, intent(out), optional :: status

call self%get (work, status)
if (status == STATUS_OK) val = work(1)
