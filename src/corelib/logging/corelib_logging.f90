module corelib_logging

    use iso_fortran_env
    use corelib_datetime
    use corelib_string

    implicit none
    private

    enum, bind(c)
        enumerator :: LEVEL_DEBUG = 1, LEVEL_INFO = 2, LEVEL_WARNING = 3, &
            LEVEL_ERROR = 4, LEVEL_CRITICAL = 5
    end enum

    type (str) :: LEVEL_NAMES(5)

    type logger
        private
        type (system_time) :: t_created
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

    end type

    public :: logger, get_logger
    public :: LEVEL_DEBUG, LEVEL_INFO, LEVEL_WARNING, LEVEL_ERROR, LEVEL_CRITICAL

    contains

pure function level_name (lvl) result (res)
    integer, intent(in) :: lvl
    type (str) :: res

    select case (lvl)
    case (LEVEL_DEBUG)
        res = "DEBUG"
    case (LEVEL_INFO)
        res = "INFO"
    case (LEVEL_WARNING)
        res = "WARNING"
    case (LEVEL_ERROR)
        res = "ERROR"
    case (LEVEL_CRITICAL)
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

! ******************************************************************************
! LOGGING procedures
subroutine logger_log_str (self, lvl, msg)
    class (logger), intent(in) :: self
    integer, intent(in) :: lvl
    class (str), intent(in) :: msg

    type (timedelta) :: td
    type (str) :: td_str, log_str

    td = perf_counter() - self%t_created
    td_str = td%strftime("[%dd %h:%m:%s] ")

    log_str = td_str // level_name (lvl) // ": " // msg

    write (unit=OUTPUT_UNIT, fmt="(a)") log_str%to_char()

end subroutine

subroutine logger_log_char (self, lvl, msg)
    class (logger), intent(in) :: self
    integer, intent(in) :: lvl
    character (len=*), intent(in) :: msg

    call self%log (lvl, str(msg))
end subroutine

! ******************************************************************************
subroutine logger_debug_str (self, msg)
    class (logger), intent(in) :: self
    class (str), intent(in) :: msg

    call self%log (LEVEL_DEBUG, msg)
end subroutine

subroutine logger_debug_char (self, msg)
    class (logger), intent(in) :: self
    character (len=*), intent(in) :: msg

    call self%log (LEVEL_DEBUG, str(msg))
end subroutine

! ******************************************************************************
subroutine logger_info_str (self, msg)
    class (logger), intent(in) :: self
    class (str), intent(in) :: msg

    call self%log (LEVEL_INFO, msg)
end subroutine

subroutine logger_info_char (self, msg)
    class (logger), intent(in) :: self
    character (len=*), intent(in) :: msg

    call self%log (LEVEL_INFO, str(msg))
end subroutine

! ******************************************************************************
subroutine logger_warning_str (self, msg)
    class (logger), intent(in) :: self
    class (str), intent(in) :: msg

    call self%log (LEVEL_WARNING, msg)
end subroutine

subroutine logger_warning_char (self, msg)
    class (logger), intent(in) :: self
    character (len=*), intent(in) :: msg

    call self%log (LEVEL_WARNING, str(msg))
end subroutine

! ******************************************************************************
subroutine logger_error_str (self, msg)
    class (logger), intent(in) :: self
    class (str), intent(in) :: msg

    call self%log (LEVEL_ERROR, msg)
end subroutine

subroutine logger_error_char (self, msg)
    class (logger), intent(in) :: self
    character (len=*), intent(in) :: msg

    call self%log (LEVEL_ERROR, str(msg))
end subroutine

! ******************************************************************************
subroutine logger_critical_str (self, msg)
    class (logger), intent(in) :: self
    class (str), intent(in) :: msg

    call self%log (LEVEL_CRITICAL, msg)
end subroutine

subroutine logger_critical_char (self, msg)
    class (logger), intent(in) :: self
    character (len=*), intent(in) :: msg

    call self%log (LEVEL_CRITICAL, str(msg))
end subroutine


end module
