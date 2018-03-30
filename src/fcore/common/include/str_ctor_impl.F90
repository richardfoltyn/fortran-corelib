
character (*), intent(in), optional :: fmt
    !*  Optional format to use for parsing VAL. Must conform to standard
    !   Fortran format specifications for WRITE, even though the surrounding
    !   parentheses may be omitted.
type (str) :: res
    !*  String object created from VAL. Contains error message if conversion
    !   was not successful.

integer, parameter :: LEN_BUFFER = 100
character (LEN_BUFFER) :: buf
character (:), allocatable :: lfmt
integer :: iostat
integer :: n

if (present(fmt)) then
    n = len_trim (fmt)
    if (.not. (fmt(1:1) == '(' .and. fmt(n:n) == ')')) then
        allocate (character (n+2) :: lfmt)

        lfmt(2:n+1) = fmt(1:n)
        lfmt(1:1) = '('
        lfmt(n+2:n+2) = ')'

        write (unit=buf, fmt=lfmt, iostat=iostat) val

        deallocate (lfmt)
    else
        write (unit=buf, fmt=fmt, iostat=iostat) val
    end if
else
    if (IS_INTEGER) then
        ! Get rid of leading spaces that result from default formatting of
        ! integers.
        write (unit=buf, fmt='(i0)', iostat=iostat) val
    else
        write (unit=buf, fmt=*, iostat=iostat) val
    end if
end if

if (iostat == 0) then
    n = len_trim (buf)
    res = buf(1:n)
else
    res = "Error converting value to string"
end if

