module fcore_logging

    use iso_fortran_env
    use fcore_datetime
    use fcore_common_kinds
    use fcore_common

    implicit none
    private
    
    public :: logger, get_logger
    
    integer (FC_ENUM_KIND), public, parameter :: FC_LOG_DEBUG = 0
    integer (FC_ENUM_KIND), public, parameter :: FC_LOG_INFO = 1
    integer (FC_ENUM_KIND), public, parameter :: FC_LOG_WARNING = ishft(1, 1)
    integer (FC_ENUM_KIND), public, parameter :: FC_LOG_ERROR = ishft(1, 2)
    integer (FC_ENUM_KIND), public, parameter :: FC_LOG_CRITICAL = ishft(1, 3)

    type (str) :: LEVEL_NAMES(5)

    type logger
        private
        type (system_time) :: t_created
        integer :: line_length = 80
        integer :: default_log_level = FC_LOG_INFO
    contains
        procedure, pass :: logger_debug_str
        procedure, pass :: logger_info_str
        procedure, pass :: logger_warning_str
        procedure, pass :: logger_error_str
        procedure, pass :: logger_critical_str
        procedure, pass :: logger_log_str

        procedure, pass :: logger_debug_char
        procedure, pass :: logger_info_char
        procedure, pass :: logger_warning_char
        procedure, pass :: logger_error_char
        procedure, pass :: logger_critical_char
        procedure, pass :: logger_log_char

        generic, public :: debug => logger_debug_str, logger_debug_char
        generic, public :: info => logger_info_str, logger_info_char
        generic, public :: warning => logger_warning_str, logger_warning_char
        generic, public :: error => logger_error_str, logger_error_char
        generic, public :: critical => logger_critical_str, logger_critical_char

        generic, public :: log => logger_log_str, logger_log_char

        procedure, pass :: logger_heading_char
        procedure, pass :: logger_heading_str
        
        generic, public :: heading => logger_heading_char, logger_heading_str
    end type


    contains

pure function level_name (level) result (res)
    integer (FC_ENUM_KIND), intent(in) :: level
    type (str) :: res

    select case (level)
    case (FC_LOG_DEBUG)
        res = "DEBUG"
    case (FC_LOG_INFO)
        res = "INFO"
    case (FC_LOG_WARNING)
        res = "WARNING"
    case (FC_LOG_ERROR)
        res = "ERROR"
    case (FC_LOG_CRITICAL)
        res = "CRITICAL"
    end select
end function

! ******************************************************************************
! LOGGER creation

function get_logger () result(res)
    class (logger), pointer :: res
    type (logger), allocatable, target, save :: log

    if (.not. allocated(log)) then
        allocate (log)
        log%t_created = perf_counter()
    end if

    res => log
end function

!-------------------------------------------------------------------------------
! HEADING routines

subroutine logger_heading_char (self, text, level)
    class (logger), intent(in) :: self
    character (*), intent(in) :: text
    integer (FC_ENUM_KIND), intent(in), optional :: level
    
    call self%heading (str(text), level)
end subroutine


subroutine logger_heading_str (self, text, level)
    class (logger), intent(in) :: self
    type (str), intent(in) :: text
    integer (FC_ENUM_KIND), intent(in), optional :: level
    
    type (timedelta) :: td
    type (str) :: td_str, msg, sep
    integer :: n
    
    td = perf_counter () - self%t_created
    td_str = td%strftime ("[%dd %h:%m:%s] ")
    
    n = self%line_length - len(td_str)
    sep = td_str // repeat("=", n)
    msg = td_str // " " // text
    
    write (unit=OUTPUT_UNIT, fmt='(a)') sep%to_char ()
    write (unit=OUTPUT_UNIT, fmt='(a)') msg%to_char ()
    write (unit=OUTPUT_UNIT, fmt='(a)') sep%to_char ()
end subroutine

! ******************************************************************************
! LOGGING procedures
subroutine logger_log_str (self, level, msg)
    class (logger), intent(in) :: self
    integer (FC_ENUM_KIND), intent(in) :: level
    class (str), intent(in) :: msg

    type (timedelta) :: td
    type (str) :: td_str, log_str

    td = perf_counter() - self%t_created
    td_str = td%strftime("[%dd %h:%m:%s] ")

    log_str = td_str // level_name (level) // ": " // msg

    write (unit=OUTPUT_UNIT, fmt="(a)") log_str%to_char()

end subroutine

subroutine logger_log_char (self, level, msg)
    class (logger), intent(in) :: self
    integer (FC_ENUM_KIND), intent(in) :: level
    character (len=*), intent(in) :: msg

    call self%log (level, str(msg))
end subroutine

! ******************************************************************************
subroutine logger_debug_str (self, msg)
    class (logger), intent(in) :: self
    class (str), intent(in) :: msg

    call self%log (FC_LOG_DEBUG, msg)
end subroutine

subroutine logger_debug_char (self, msg)
    class (logger), intent(in) :: self
    character (len=*), intent(in) :: msg

    call self%log (FC_LOG_DEBUG, str(msg))
end subroutine

! ******************************************************************************
subroutine logger_info_str (self, msg)
    class (logger), intent(in) :: self
    class (str), intent(in) :: msg

    call self%log (FC_LOG_INFO, msg)
end subroutine

subroutine logger_info_char (self, msg)
    class (logger), intent(in) :: self
    character (len=*), intent(in) :: msg

    call self%log (FC_LOG_INFO, str(msg))
end subroutine

! ******************************************************************************
subroutine logger_warning_str (self, msg)
    class (logger), intent(in) :: self
    class (str), intent(in) :: msg

    call self%log (FC_LOG_WARNING, msg)
end subroutine

subroutine logger_warning_char (self, msg)
    class (logger), intent(in) :: self
    character (len=*), intent(in) :: msg

    call self%log (FC_LOG_WARNING, str(msg))
end subroutine

! ******************************************************************************
subroutine logger_error_str (self, msg)
    class (logger), intent(in) :: self
    class (str), intent(in) :: msg

    call self%log (FC_LOG_ERROR, msg)
end subroutine

subroutine logger_error_char (self, msg)
    class (logger), intent(in) :: self
    character (len=*), intent(in) :: msg

    call self%log (FC_LOG_ERROR, str(msg))
end subroutine

! ******************************************************************************
subroutine logger_critical_str (self, msg)
    class (logger), intent(in) :: self
    class (str), intent(in) :: msg

    call self%log (FC_LOG_CRITICAL, msg)
end subroutine

subroutine logger_critical_char (self, msg)
    class (logger), intent(in) :: self
    character (len=*), intent(in) :: msg

    call self%log (FC_LOG_CRITICAL, str(msg))
end subroutine


end module
