
module fcore_common_status_helpers
    !*  Module with helper functions for handling status codes
    !   and harmonized error messages.
    !
    !   Helper functions accept OPTIONAL status arguments such
    !   that they can be directly used in all routines which
    !   themselves have only optional status arguments
    !   without the need to check for presence, etc.

    use, intrinsic :: iso_fortran_env
    use fcore_common_constants
    use fcore_common_kinds
    use fcore_common_base

    implicit none
    private

    public :: status_set_not_associated
    public :: status_set_ok
    public :: status_set_cast_error

    interface status_set_cast_error
        module procedure status_set_cast_error_str, status_set_cast_error_char
    end interface

contains

pure subroutine status_set_ok (status)
    !*  STATUS_SET_OK sets the status code to FC_STATUS_OK
    !   and removes any status message.

    type (status_t), intent(out), optional :: status

    if (present(status)) then
        call status%init (FC_STATUS_OK)
    end if
end subroutine


pure subroutine status_set_not_associated (status)
    !*  STATUS_SET_NOT_ASSOCIATED sets the status code
    !   and error message to be used when unassociated
    !   pointers are encountered.

    type (status_t), intent(out), optional :: status
    
    if (present(status)) then
        call status%init (FC_STATUS_NOT_ASSOCIATED)
        status%msg = "Pointer not associated"
    end if
end subroutine


pure subroutine status_set_cast_error_char (status, type_name)
    !*  STATUS_SET_CAST_ERROR_CHAR sets the status code and 
    !   error message when a dynamic cast cannot be performed.

    type (status_t), intent(out), optional :: status
    character (*), intent(in) :: type_name

    call status_set_cast_error (status, str(type_name))
end subroutine


pure subroutine status_set_cast_error_str (status, type_name)
    !*  STATUS_SET_CAST_ERROR_STR sets the status code and 
    !   error message when a dynamic cast cannot be performed.

    type (status_t), intent(out), optional :: status
    type (str), intent(in), optional :: type_name

    if (present(status)) then
        call status%init (FC_STATUS_TYPE_ERROR)

        if (present(type_name)) then
            status%msg = "Unsupported cast to '" // type_name // "'"
        else
            status%msg = "Unsupported cast"
        end if
    end if
end subroutine




end module
