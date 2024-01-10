

module fcore_diagnostics
    !*  Module implements routines that write diagnostic messages to
    !   STDERR when code is compiled with the __DIAGNOSTICS symbol.

    use, intrinsic :: iso_fortran_env, stderr => error_unit

    use fcore_char

    implicit none
    private

    public :: diag_entry
    public :: diag_exit
    public :: diag_msg

    contains


subroutine diag_entry (name)
    character (*), intent(in) :: name

#ifdef __DIAGNOSTICS
    write (stderr, '(">>> Entering ", a)') upper (name)
#endif
end subroutine



subroutine diag_exit (name)
    character (*), intent(in) :: name
#ifdef __DIAGNOSTICS
    write (stderr, '("<<< Exiting ", a)') upper (name)
#endif
end subroutine



subroutine diag_msg (msg)
    character (*), intent(in) :: msg

#ifdef __DIAGNOSTICS
    write (stderr, '(a)') msg
#endif
end subroutine

end module
