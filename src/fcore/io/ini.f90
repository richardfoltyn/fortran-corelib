

module fcore_io_ini
    !*  Module to implement classes and routines to read and write files in INI
    !   format.

    use, intrinsic :: iso_fortran_env
    use fcore_common

    implicit none
    private

    integer, parameter :: BUFFER_SIZE = 2**10
    character (*), parameter :: COMMENT_CHARS = ';#'

    type, public :: ini_key_value
        private

        type (str) :: name
        type (str) :: value

    contains
        procedure, pass :: ini_key_value_parse_char
        procedure, pass :: ini_key_value_parse_str
        generic :: parse => ini_key_value_parse_char, ini_key_value_parse_str

        procedure, pass :: ini_key_value_get
        generic, public :: get => ini_key_value_get

        procedure, pass :: ini_key_value_set_char
        generic, public :: set => ini_key_value_set_char

        procedure, pass :: ini_key_value_get_logical
        generic :: get_logical => ini_key_value_get_logical
        
        procedure, pass :: ini_key_value_to_char
        generic :: to_char => ini_key_value_to_char

    end type



    type, public :: ini_section
        private

        type (str) :: name
        type (ini_key_value), dimension(:), allocatable :: values

    contains
!        procedure, pass :: ini_section_parse_char
        procedure, pass :: ini_section_parse_str
!        generic, public :: parse => ini_section_parse_char, ini_section_parse_str
        generic, public :: parse => ini_section_parse_str

        procedure, pass :: ini_section_has_key_char
        procedure, pass :: ini_section_has_key_str
        generic, public :: has_key => ini_section_has_key_char, &
            ini_section_has_key_str

        procedure, pass :: ini_section_get_char
        procedure, pass :: ini_section_get_str
        generic, public :: get => ini_section_get_char, &
            ini_section_get_str

        procedure, pass :: ini_section_set_char
        procedure, pass :: ini_section_set_str
        generic, public :: set => ini_section_set_char, ini_section_set_str

        procedure, pass :: ini_section_value_count
        generic, public :: value_count => ini_section_value_count

        procedure, pass :: ini_section_to_char
        generic :: to_char => ini_section_to_char

    end type



    type, public :: ini_file
        private

        type (str), public :: path
        type (ini_section), dimension(:), allocatable :: sections

    contains
        procedure, pass :: ini_file_parse_file
        procedure, pass :: ini_file_parse_char
        procedure, pass :: ini_file_parse_str_array
        generic, public :: parse => ini_file_parse_file, ini_file_parse_char, &
            ini_file_parse_str_array

        procedure, pass :: ini_file_add_section_section
!        procedure, pass :: ini_file_add_section_char_array
        procedure, pass :: ini_file_add_section_str_array
        generic, public :: add_section => ini_file_add_section_section, &
            ini_file_add_section_str_array

        procedure, pass :: ini_file_has_section_char
        procedure, pass :: ini_file_has_section_str
        generic, public :: has_section => ini_file_has_section_char, &
            ini_file_has_section_str

        procedure, pass :: ini_file_has_key_char
        procedure, pass :: ini_file_has_key_str
        generic, public :: has_key => ini_file_has_key_char, &
            ini_file_has_key_str

        procedure, pass :: ini_file_get_char
        procedure, pass :: ini_file_get_str
        generic, public :: get => ini_file_get_char, ini_file_get_str

        procedure, pass :: ini_file_section_count
        generic, public :: section_count => ini_file_section_count

        procedure, pass :: ini_file_line_count
        generic, public :: line_count => ini_file_line_count

        procedure, pass :: ini_file_to_char
        generic, public :: to_char => ini_file_to_char

        procedure, pass :: ini_file_write
        generic, public :: write => ini_file_write

    end type


    interface ini_file
        procedure ini_file_char
    end interface

    contains


pure function ini_file_char (path) result(res)
    character (*), intent(in) :: path
    type (ini_file) :: res

    res%path = path
end function




subroutine ini_file_parse_file (self, status)
    class (ini_file), intent(inout) :: self
    type (status_t), intent(out), optional :: status

    integer :: iostat, uid, nlines, i

    type (status_t) :: lstatus
    character (1024) :: lmsg
    character (BUFFER_SIZE), allocatable, dimension(:) :: data
    type (str), allocatable, dimension(:) :: sdata

    open (newunit=uid, file=self%path%to_char(), status='old', action='read', &
        access='sequential', iostat=iostat, iomsg=lmsg, err=100)

    nlines = 0

    do while (.true.)
        ! Check how many records are in the file
        read (unit=uid, fmt=*, iomsg=lmsg, iostat=iostat, err=100, end=10)
        nlines = nlines + 1
    end do

10  continue

    rewind (unit=uid, iostat=iostat, iomsg=lmsg, err=100)

    ! --- Read data ---

    allocate (data(nlines))

    read (unit=uid, fmt='(a)', iomsg=lmsg, iostat=iostat, err=100, end=20) data

20  continue

    close (uid)

    ! --- Parse input data ---

    allocate (sdata(nlines))

    do i = 1, nlines
        sdata(i) = data(i)
    end do

    call self%parse (sdata, status)

    return

100 continue

    lstatus = FC_STATUS_IO_ERROR

    if (present(status)) then
        status = lstatus
        status%msg = lmsg
    end if
end subroutine



subroutine ini_file_parse_char (self, contents, status)
    class (ini_file), intent(inout) :: self
    character (*), intent(in) :: contents
    type (status_t), intent(out), optional :: status

    type (str) :: sep, s
    type (str), allocatable, dimension(:) :: tokens
    type (status_t) :: lstatus
    character (1024) :: lmsg

    lstatus = FC_STATUS_UNDEFINED
    lmsg = ''

    sep = new_line ('A')
    s = str (contents)
    call s%split (tokens, sep=sep, drop_empty=.true., status=lstatus)

!    read (contents, fmt='(*(a,/))', iostat=iostat, iomsg=lmsg, err=100) data

    ! End of string reached, pass on to internal parsing routine
    call self%parse (tokens, lstatus)

    if (present(status)) status = lstatus

end subroutine



subroutine ini_file_parse_str_array (self, contents, status)
    class (ini_file), intent(inout) :: self
    type (str), intent(in), dimension(:) :: contents
    type (status_t), intent(out), optional :: status

    integer :: i, j, nlines, k
    type (str) :: line
    type (str), dimension(:), allocatable :: lines
    type (status_t) :: lstatus

    lstatus = FC_STATUS_OK

    ! --- Determine number of non-empty lines ---

    nlines = 0
    do i = 1, size (contents)
        if (len_trim (contents(i)) == 0) cycle
        if (scan (contents(i), COMMENT_CHARS) > 0) cycle

        nlines = nlines + 1
    end do

    if (nlines == 0) then
        lstatus%msg = "Empty input data provided"
        goto 100
    end if

    ! --- Retain only non-empty lines ---

    allocate (lines(nlines))
    nlines = 0
    do i = 1, size (contents)
        if (len_trim (contents(i)) == 0) cycle

        ! Remove any leading whitespace
        line = adjustl (contents(i))

        ! Skip comment lines
        if (scan (line, COMMENT_CHARS) == 1) cycle

        nlines = nlines + 1
        lines(nlines) = line
        print *, line%to_char()
    end do

    ! --- Parse actual contents ---

    i = 1

    do while (i <= size (lines))
        if ( (index(lines(i), '[') == 0) .or. (index(lines(i), ']') == 0)) then
            lstatus = FC_STATUS_PARSE_ERROR
            goto 100
        end if

        ! Find end of section (either end of file or beginning of next section)
        do j = i+1, size(lines) - 1
            if (index (lines(j), '[') == 1) exit
        end do

        do k = i, j - 1
            print *, lines(k)%to_char()
        end do

        call ini_file_add_section_str_array (self, lines(i:j-1))

        ! Shift index for next section
        i = j
    end do

100 continue

    if (present(status)) status = lstatus

end subroutine




subroutine ini_file_add_section_str_array (self, contents, status)
    class (ini_file), intent(inout) :: self
    type (str), intent(in), dimension(:) :: contents
    type (status_t), intent(inout), optional :: status

    type (status_t) :: lstatus
    type (ini_section) :: section

    lstatus = FC_STATUS_OK

    call section%parse (contents, lstatus)
    if (lstatus /= FC_STATUS_OK) goto 100

    call self%add_section (section)

100 continue

    if (present(status)) status = lstatus

end subroutine



!subroutine ini_file_add_section_char_array (self, contents, status)
!    class (ini_file), intent(inout) :: self
!    character (BUFFER_SIZE), intent(in), dimension(:) :: contents
!    type (status_t), intent(inout), optional :: status
!
!    type (status_t) :: lstatus
!    type (ini_section) :: section
!
!    lstatus = FC_STATUS_OK
!
!    call section%parse (contents, lstatus)
!    if (lstatus /= FC_STATUS_OK) goto 100
!
!    call self%add_section (section)
!
!100 continue
!
!    if (present(status)) status = lstatus
!
!end subroutine




subroutine ini_file_add_section_section (self, section, status)
    class (ini_file), intent(inout) :: self
    class (ini_section), intent(in) :: section
    type (status_t), intent(out), optional :: status

    type (status_t) :: lstatus

    integer :: n
    type (ini_section), dimension(:), allocatable :: sections

    lstatus = FC_STATUS_OK

    if (allocated (self%sections)) then
        if (any (self%sections%name == section%name)) then
            lstatus = FC_STATUS_VALUE_ERROR
            lstatus%msg = "Duplicate section name " // section%name
        end if
    end if

    if (allocated (self%sections)) then
        n = size (self%sections)
        allocate (sections(n+1))
        sections(1:n) = self%sections
        sections(n+1) = section
        call move_alloc (sections, self%sections)
    else
        allocate (self%sections(1), source=section)
    end if

    if (present(status)) status = lstatus

end subroutine



function ini_file_has_section_str (self, name) result(flag)
    class (ini_file), intent(in) :: self
    type (str), intent(in) :: name
    logical :: flag

    flag = .false.

    if (allocated (self%sections)) then
        flag = any (self%sections%name == name)
    end if
end function



function ini_file_has_section_char (self, name) result(flag)
    class (ini_file), intent(in) :: self
    character (*), intent(in) :: name
    logical :: flag

    flag = self%has_section (str(name))
end function



function ini_file_has_key_str (self, section, key) result(flag)
    class (ini_file), intent(in) :: self
    type (str), intent(in) :: section
    type (str), intent(in) :: key
    logical :: flag

    integer :: i
    flag = .false.

    if (allocated (self%sections)) then
        if (self%has_section (section)) then
            do i = 1, size (self%sections)
                if (self%sections(i)%name == section) then
                    flag = self%sections(i)%has_key (key)
                    exit
                end if
            end do
        end if
    end if
end function



function ini_file_has_key_char (self, section, key) result(flag)
    class (ini_file), intent(in) :: self
    character (*), intent(in) :: section
    character (*), intent(in) :: key
    logical :: flag

    flag = self%has_key (str(section), str(key))
end function



subroutine ini_file_get_str (self, section, key, val, status)
    class (ini_file), intent(in) :: self
    type (str), intent(in) :: section
    type (str), intent(in) :: key
    class (*), intent(out) :: val
    type (status_t), intent(out) , optional :: status

    integer :: i
    type (status_t) :: lstatus

    lstatus = FC_STATUS_OK

    if (.not. self%has_section (section)) then
        lstatus = FC_STATUS_VALUE_ERROR
        lstatus%msg = 'Section ' // section // ' not present'
        goto 100
    end if

    do i = 1, size (self%sections)
        if (self%sections(i)%name == section) then
            call self%sections(i)%get (key, val, lstatus)
            exit
        end if
    end do

100 continue

    if (present(status)) status = lstatus
end subroutine



subroutine ini_file_get_char (self, section, key, val, status)
    class (ini_file), intent(in) :: self
    character (*), intent(in) :: section
    character (*), intent(in) :: key
    class (*), intent(out) :: val
    type (status_t), intent(out) , optional :: status

    call self%get (str(section), str(key), val, status)
end subroutine



pure function ini_file_section_count (self) result(res)
    class (ini_file), intent(in) :: self
    integer :: res

    res = 0

    if (allocated (self%sections)) then
        res = size (self%sections)
    end if
end function



pure function ini_file_line_count (self) result(res)
    class (ini_file), intent(in) :: self
    integer :: res

    integer :: i
    res = 0

    if (allocated (self%sections)) then
        do i = 1, size (self%sections)
            res = res + self%sections(i)%value_count() + 1
        end do
    end if
end function



pure subroutine ini_file_to_char (self, char, status)
    class (ini_file), intent(in) :: self
    character (BUFFER_SIZE), intent(out), dimension(:) :: char
    type (status_t), intent(out), optional :: status

    type (status_t) :: lstatus
    integer :: i, n, ioffset

    lstatus = FC_STATUS_OK

    if (size (char) < self%line_count()) then
        lstatus = FC_STATUS_VALUE_ERROR
        lstatus%msg = 'Array too small'
        goto 100
    end if

    ioffset = 0
    do i = 1, size (self%sections)
        n = self%sections(i)%value_count()
        call self%sections(i)%to_char (char(ioffset+1:ioffset+n+1))

        ioffset = ioffset + n + 1
    end do

100 continue

    if (present(status)) status = lstatus

end subroutine



subroutine ini_file_write (self, status)
    class (ini_file), intent(in) :: self
    type (status_t), intent(out), optional :: status

    integer :: iostat, uid, nlines, i, n
    character (1024) :: lmsg
    type (status_t) :: lstatus
    character (BUFFER_SIZE), allocatable, dimension(:) :: lines
    character (BUFFER_SIZE) :: line

    lstatus = FC_STATUS_OK

    nlines = self%line_count ()

    allocate (lines(nlines))

    call self%to_char (lines)

    open (newunit=uid, file=self%path%to_char(), action='write', &
        access='sequential', iostat=iostat, iomsg=lmsg, err=60)

    do i = 1, nlines
        line = lines(i)
        n = len_trim (line)
        write (unit=uid, fmt='(a)', iomsg=lmsg, iostat=iostat, err=50) line(i:n)
    end do

    close (uid)

    goto 100

50  continue

    close (uid)

60  continue

    lstatus = FC_STATUS_IO_ERROR
    lstatus%msg = lmsg

100 continue

    if (present(status)) status = lstatus

end subroutine





subroutine ini_section_parse_str (self, contents, status)
    class (ini_section), intent(inout) :: self
    type (str), intent(in), dimension(:) :: contents
    type (status_t), intent(out), optional :: status

    integer :: ipos, nvalues, i
    type (str) :: line
    type (status_t) :: lstatus

    lstatus = FC_STATUS_OK

    line = contents(1)

    ipos = index (line, ']')
    self%name = line%substring(2, ipos-1)

    print *, self%name%to_char()

    nvalues = size(contents) - 1
    allocate (self%values(nvalues))

    do i = 2, size(contents)
        call self%values(i-1)%parse (contents(i), lstatus)
        if (any (self%values(1:i-2)%name == self%values(i-1)%name)) then
            lstatus = FC_STATUS_VALUE_ERROR
            lstatus%msg = 'Duplicate key ' // self%values(i-1)%name // &
                ' in section ' // self%name
            goto 100
        end if
        if (lstatus /= FC_STATUS_OK) goto 100
    end do

100 continue

    if (present(status)) status = lstatus

end subroutine



!pure subroutine ini_section_parse_char (self, contents, status)
!    class (ini_section), intent(inout) :: self
!    character (BUFFER_SIZE), intent(in), dimension(:) :: contents
!    type (status_t), intent(out), optional :: status
!
!    type (str), allocatable, dimension(:) :: scontents
!    integer :: i
!
!    allocate (scontents (size(contents)))
!
!    do i = 1, size (contents)
!        scontents(i) = str (contents(i))
!    end do
!end subroutine



pure function ini_section_has_key_str (self, key) result(flag)
    class (ini_section), intent(in) :: self
    type (str), intent(in) :: key
    logical :: flag

    flag = .false.

    if (.not. allocated (self%values)) return

    if (allocated(self%values)) then
        flag = any (self%values%name == key)
    end if
end function



pure function ini_section_has_key_char (self, key) result(flag)
    class (ini_section), intent(in) :: self
    character (*), intent(in) :: key
    logical :: flag

    flag = self%has_key (str (key))
end function



subroutine ini_section_get_str (self, key, val, status)
    class (ini_section), intent(in) :: self
    type (str), intent(in) :: key
    class (*), intent(out) :: val
    type (status_t), intent(out) , optional :: status

    type (status_t) :: lstatus
    integer :: i

    lstatus = FC_STATUS_OK

    if (.not. self%has_key (key)) then
        lstatus = FC_STATUS_VALUE_ERROR
        lstatus%msg = 'Key ' // key // ' not present'
        goto 100
    end if

    do i = 1, size (self%values)
        if (self%values(i)%name == key) then
            call self%values(i)%get (val, lstatus)
            exit
        end if
    end do

100 continue

    if (present(status)) status = lstatus
end subroutine



subroutine ini_section_get_char (self, key, val, status)
    class (ini_section), intent(in) :: self
    character (*), intent(in) :: key
    class (*), intent(out) :: val
    type (status_t), intent(out) , optional :: status

    call self%get (str(key), val, status)
end subroutine



subroutine ini_section_set_char (self, key, val, fmt, status)
    class (ini_section), intent(inout) :: self
    character (*), intent(in) :: key
    class (*), intent(in) :: val
    character (*), intent(in), optional :: fmt
    type (status_t), intent(out) , optional :: status

    integer :: i
    type (status_t) :: lstatus

    lstatus = FC_STATUS_OK

    if (.not. self%has_key (key)) then
        lstatus = FC_STATUS_VALUE_ERROR
        lstatus%msg = 'Key ' // key // ' not present'
        goto 100
    end if

    do i = 1, size (self%values)
        if (self%values(i)%name == key) then
            call self%values(i)%set (val, fmt, lstatus)
            exit
        end if
    end do

100 continue

    if (present (status)) status = lstatus

end subroutine



subroutine ini_section_set_str (self, key, val, fmt, status)
    class (ini_section), intent(inout) :: self
    type (str), intent(in) :: key
    class (*), intent(out) :: val
    type (str), intent(in), optional :: fmt
    type (status_t), intent(out), optional :: status

    if (present (fmt)) then
        call self%set (key%to_char(), val, fmt%to_char(), status)
    else
        call self%set (key%to_char(), val, status=status)
    end if
end subroutine



pure subroutine ini_section_to_char (self, char, status)
    class (ini_section), intent(in) :: self
    character (BUFFER_SIZE), intent(out), dimension(:) :: char
    type (status_t), intent(out), optional :: status

    type (status_t) :: lstatus
    integer :: i

    lstatus = FC_STATUS_OK

    if (size(char) < (self%value_count() + 1)) then
        lstatus = FC_STATUS_VALUE_ERROR
        lstatus%msg = 'Array smaller than number of key-value pairs'
        goto 100
    end if

    char (1) = '[' // self%name // ']'
    do i = 1, size (self%values)
        call self%values(i)%to_char (char(i+1))
    end do

100 continue

    if (present(status)) status = lstatus

end subroutine



elemental function ini_section_value_count (self) result(res)
    class (ini_section), intent(in) :: self
    integer :: res

    res = 0

    if (allocated (self%values)) then
        res = size (self%values)
    end if

end function



subroutine ini_key_value_parse_str (self, contents, status)
    class (ini_key_value), intent(inout) :: self
    type (str), intent(in) :: contents
    type (status_t), intent(out), optional :: status

    type (str) :: line
    integer :: ipos, n
    type (status_t) :: lstatus

    lstatus = FC_STATUS_OK

    line = trim (adjustl ( contents))

    ipos = index (line, '=')
    if (ipos == 0) then
        lstatus = FC_STATUS_PARSE_ERROR
        lstatus%msg = 'Missing = when parsing key-value pair'
        goto 100
    end if

    if (ipos == 1) then
        lstatus = FC_STATUS_PARSE_ERROR
        lstatus%msg = 'Zero-length key not permitted'
        goto 100
    end if

    n = len_trim (line)
    if (ipos == n) then
        lstatus = FC_STATUS_PARSE_ERROR
        lstatus%msg = 'Zero-length value not permitted'
        goto 100
    end if

    self%name = line%substring(1, ipos-1)
    self%value = line%substring(ipos+1, n)

    print *, self%name%to_char()
    print *, self%value%to_char()

100 continue

    if (present(status)) status = lstatus

end subroutine



subroutine ini_key_value_parse_char (self, contents, status)
    class (ini_key_value), intent(inout) :: self
    character (*), intent(in) :: contents
    type (status_t), intent(out), optional :: status

    call self%parse (str(contents), status)

end subroutine



subroutine ini_key_value_get (self, val, status)
    class (ini_key_value), intent(in) :: self
    class (*), intent(out) :: val
    type (status_t), intent(out) , optional :: status

    type (status_t) :: lstatus

    lstatus = FC_STATUS_OK

    select type (val)
    type is (integer(int32))
        call self%value%parse (val, lstatus)
    type is (integer(int64))
        call self%value%parse (val, lstatus)
    type is (real(real32))
        call self%value%parse (val, lstatus)
    type is (real(real64))
        call self%value%parse (val, lstatus)
    type is (logical)
        call self%get_logical (val, lstatus)
    type is (character (*))
        val = self%value%to_char()
    class is (str)
        val = self%value
    class default
        lstatus = FC_STATUS_VALUE_ERROR
        lstatus%msg = "Unsupported argument type"
        goto 100
    end select

100 continue

    if (present(status)) status = lstatus

end subroutine



subroutine ini_key_value_get_logical (self, val, status)
    class (ini_key_value), intent(in) :: self
    logical, intent(out) :: val
    type (status_t), intent(out) , optional :: status

    type (status_t) :: lstatus
    type (str) :: lvalue
    integer :: ival

    lstatus = FC_STATUS_OK

    lvalue = self%value%lower()

    if (lvalue == 'off' .or. lvalue == 'no') then
        val = .false.
        goto 100
    else if (lvalue == 'on' .or. lvalue == 'yes') then
        val = .true.
        goto 100
    else
        ! Try to parse as integer
        call self%value%parse (ival, lstatus)
        if (lstatus == FC_STATUS_OK) then
            select case (ival)
                case (0)
                    val = .false.
                    goto 100
                case (1)
                    val = .true.
                    goto 100
                case default
                    lstatus = FC_STATUS_VALUE_ERROR
                    lstatus%msg = 'Integer could not be converted to logical'
                    goto 100
            end select
        end if

        ! Default parser for logical
        call self%value%parse (val, lstatus)
    end if

100 continue

    if (present(status)) status = lstatus
end subroutine



subroutine ini_key_value_set_char (self, val, fmt, status)
    class (ini_key_value), intent(inout) :: self
    class (*), intent(in) :: val
    character (*), intent(in), optional :: fmt
    type (status_t), intent(out) , optional :: status

    type (status_t) :: lstatus

    lstatus = FC_STATUS_OK

    select type (val)
    type is (integer(int32))
        self%value = adjustl (str(val, fmt))
    type is (integer(int64))
        self%value = adjustl (str (val, fmt))
    type is (real(real32))
        self%value = adjustl (str (val, fmt))
    type is (real(real64))
        self%value = adjustl (str (val, fmt))
    type is (logical)
        self%value = adjustl (str (val, fmt))
    type is (character (*))
        self%value = adjustl (val)
    class is (str)
        self%value = adjustl (val)
    class default
        lstatus = FC_STATUS_VALUE_ERROR
        lstatus%msg = "Unsupported argument type"
        goto 100
    end select

100 continue

    if (present(status)) status = lstatus

end subroutine


pure subroutine ini_key_value_to_char (self, char)
    class (ini_key_value), intent(in) :: self
    character (*), intent(out) :: char

    char = self%name // '=' // self%value

end subroutine


end module
