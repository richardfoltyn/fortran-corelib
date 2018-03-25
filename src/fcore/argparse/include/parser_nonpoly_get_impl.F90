
subroutine __APPEND(argparser_get_array_str,__TYPE_SUFFIX) (self, name, val, status)
    !*  ARGPARSER_GET implements the non-polymophic GET method for
    !   array-valued arguments to support buggy compilers.
    class (argparser), intent(inout) :: self
    type (str), intent(in) :: name
    __TYPE, intent(inout), dimension(:) :: val
    type (status_t), intent(out), optional :: status

    class (argument), pointer :: ptr_arg
    type (status_t) :: lstatus

    call lstatus%init (FC_STATUS_OK)

    call validate_identifier (name, lstatus)
    if (lstatus /= FC_STATUS_OK) goto 100

    call argparser_get_check_state (self, lstatus)
    if (lstatus /= FC_STATUS_OK) goto 100

    call self%find_arg (name, ptr_arg, lstatus)
    if (lstatus /= FC_STATUS_OK) goto 100

    ! at this point ptr_arg points to the argument identified by name.
    ! Retrieve stored argument value
    call ptr_arg%parse (val, lstatus)

    call process_argument_status (name, lstatus)

    100 continue
    if (present(status)) status = lstatus
end subroutine


subroutine __APPEND(argparser_get_array_char,__TYPE_SUFFIX) (self, name, val, status)
    class (argparser), intent(inout) :: self
    character (*), intent(in) :: name
    __TYPE, intent(out), dimension(:) :: val
    type (status_t), intent(out), optional :: status

    call self%get (str(name), val, status)
end subroutine