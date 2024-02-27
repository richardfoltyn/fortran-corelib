
module fcore_common_input_validation

    use, intrinsic :: iso_fortran_env
    use fcore_common_base
    use fcore_common_kinds
    use fcore_common_constants
    
    implicit none
    private

    public :: input_assert_present, input_assert_not_present

    interface input_assert_present
        module procedure assert_present_char, assert_present_str
    end interface

    interface input_assert_not_present
        module procedure assert_not_present_char, assert_not_present_str
    end interface

contains

subroutine assert_present_str (arg, name, status, code)
    class (*), intent(in), optional :: arg
    type (str), intent(in) :: name
    type (status_t), intent(in out) :: status
    integer (FC_ENUM_KIND), intent(in), optional :: code

    integer (FC_ENUM_KIND) :: lcode

    lcode = FC_STATUS_VALUE_ERROR
    if (present(code)) lcode = code

    status = FC_STATUS_OK

    if (.not. present(arg)) then
        status = lcode
        status%msg = "Missing required argument '" // name // "'"
    end if

end subroutine

subroutine assert_present_char (arg, name, status, code)
    class (*), intent(in), optional :: arg
    character (*), intent(in) :: name
    type (status_t), intent(in out) :: status
    integer (FC_ENUM_KIND), intent(in), optional :: code

    call assert_present_str (arg, str(name), status, code)
end subroutine

subroutine input_assert_present_1d (arg, name, status, code)
    class (*), intent(in), dimension(:), optional :: arg
    type (str), intent(in) :: name
    type (status_t), intent(in out) :: status
    integer (FC_ENUM_KIND), intent(in), optional :: code

    integer (FC_ENUM_KIND) :: lcode

    lcode = FC_STATUS_VALUE_ERROR
    if (present(code)) lcode = code

    status = FC_STATUS_OK

    if (.not. present(arg)) then
        status = lcode
        status%msg = "Missing required argument '" // name // "'"
    end if

end subroutine


subroutine assert_not_present_str (arg, name, status, code)
    class (*), intent(in), optional :: arg
    type (str), intent(in) :: name
    type (status_t), intent(in out) :: status
    integer (FC_ENUM_KIND), intent(in), optional :: code

    integer (FC_ENUM_KIND) :: lcode

    lcode = FC_STATUS_VALUE_ERROR
    if (present(code)) lcode = code

    status = FC_STATUS_OK

    if (present(arg)) then
        status = lcode
        status%msg = "Unsupported argument '" // name // "'"
    end if

end subroutine

subroutine assert_not_present_char (arg, name, status, code)
    class (*), intent(in), optional :: arg
    character (*), intent(in) :: name
    type (status_t), intent(in out) :: status
    integer (FC_ENUM_KIND), intent(in), optional :: code

    call assert_not_present_str (arg, str(name), status, code)
end subroutine



end module
