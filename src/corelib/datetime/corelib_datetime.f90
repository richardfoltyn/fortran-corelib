module corelib_datetime

    use iso_fortran_env, only: int64
    use corelib_string
    use corelib_utils, only: assert_alloc
    implicit none
    private

    integer (int64), parameter :: TIMEDELTA_RESOLUTION = 10 ** 6
    integer (int64), parameter :: SECONDS_PER_DAY = 3600 * 24

    type timedelta
        ! Convention: days takes on any presentable positive/negative
        ! integer value.
        integer (int64) :: days = 0
        ! seconds and microseconds are non-negative.
        integer (int64) :: seconds = 0
        integer (int64) :: microseconds = 0
    contains
        procedure, public, pass :: strftime => timedelta_strftime
    end type

    type system_time
        private
        integer (int64) :: count, count_rate, count_max
    end type

    interface operator (-)
        module procedure system_time_minus
    end interface

    public :: timedelta, system_time, perf_counter
    public :: operator(-)

contains

function perf_counter() result(res)

    type (system_time) :: res

    call system_clock (res%count, res%count_rate, res%count_max)
end function

pure function system_time_minus(lhs, rhs) result(res)
    class (system_time), intent(in) :: lhs, rhs
    type (timedelta) :: res

    integer (int64) :: diff, rate

    ! rate contains ticks per second
    rate = lhs%count_rate

    diff = lhs%count - rhs%count

    if (lhs%count >= rhs%count) then
        ! days >= 0
        res%days = diff / (SECONDS_PER_DAY * rate)
    else
        ! days < 0 such that seconds > 0, microseconds > 0
        res%days = diff / (SECONDS_PER_DAY * rate) - 1
    end if

    diff = diff - (res%days * SECONDS_PER_DAY * rate)

    res%seconds = diff / lhs%count_rate
    diff = diff - (res%seconds * rate)

    res%microseconds = diff * TIMEDELTA_RESOLUTION / rate
end function

! ******************************************************************************
! TIMEDELTA formatting

! TIMEDELTA_STRFTIME creates a string representation of timedelta object
! according to the supplied format.
! Supported formatting directives:
! %d    number of days
! %h    zero-padded hour: 00, 01, ... 23
! %m    zero-padded minutes: 00, 01, ... 59
! %s    zero-padded seconds: 00, 01, ... 59
! %f    zero-padded microseconds: 000000, 000001, ... 999999
! %%    literal %
pure function timedelta_strftime (self, fmt) result(res)
    class (timedelta), intent(in) :: self
    character (len=*), intent(in) :: fmt
    type (str) :: res

    integer, parameter :: BUFFER_INIT_SIZE = 20
    integer, parameter :: BUFFER_INCR = 10
    character (len=:), allocatable :: buf

    integer :: i_at, n, j_at, k, fwidth, hh, mm, ss
    character (len=1) :: c

    allocate (character (len=BUFFER_INIT_SIZE) :: buf)

    n = len(fmt)

    i_at = 1
    j_at = 1

    hh = self%seconds / 3600
    mm = (self%seconds - hh * 3600) / 60
    ss = mod(self%seconds, 60_int64)

    do while (i_at <= n)
        if (fmt(i_at:i_at) == '%') then
            ! % should not occur in last position, that cannot be interpreted
            if (i_at == n) exit
            c = fmt(i_at + 1:i_at+1)
            select case (c)
            case ('%')
                call assert_alloc (buf, j_at, BUFFER_INCR)
                buf(j_at:j_at) = '%'
                j_at = j_at + 1
            case ('d')
                fwidth = 1
                if (self%days > 0) fwidth = floor(log10(real(self%days)))
                call assert_alloc (buf, j_at + fwidth - 1, BUFFER_INCR)
                buf(j_at:j_at + fwidth - 1) = str(self%days, fmt='i0')
                j_at = j_at + fwidth
            case ('h')
                fwidth = 2
                call assert_alloc (buf, j_at + fwidth - 1, BUFFER_INCR)
                buf(j_at:j_at + fwidth - 1) = str(hh, fmt='i0.2')
                j_at = j_at + fwidth
            case ('m')
                fwidth = 2
                call assert_alloc (buf, j_at + fwidth - 1, BUFFER_INCR)
                buf(j_at:j_at + fwidth - 1) = str(mm, fmt='i0.2')
                j_at = j_at + fwidth
            case ('s')
                fwidth = 2
                call assert_alloc (buf, j_at + fwidth - 1, BUFFER_INCR)
                buf(j_at:j_at + fwidth - 1) = str(ss, fmt='i0.2')
                j_at = j_at + fwidth
            case ('f')
                fwidth = 6
                call assert_alloc (buf, j_at + fwidth - 1, BUFFER_INCR)
                buf(j_at:j_at + fwidth - 1) = str(self%microseconds, fmt='i0.6')
                j_at = j_at + fwidth
            end select

            ! increment cursor in format string by 2 (since all formats are %x)
            i_at = i_at + 2
        else
            ! find first instance of %
            do k = 1, n - i_at
                if (fmt(i_at+k:i_at+k) == '%') exit
            end do
            ! went one position too far in both cases (if % encountered, or end of string)
            k = k - 1

            call assert_alloc (buf, j_at + k, BUFFER_INCR)
            buf(j_at:j_at+k) = fmt(i_at:i_at+k)
            j_at = j_at + k + 1
            i_at = i_at + k + 1
        end if
    end do

    res = buf(1:j_at-1)
    deallocate (buf)
end function

end module
