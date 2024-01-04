

module fcore_io_ini
    !*  Module to implement classes and routines to read and write files in INI
    !   format.

    use, intrinsic :: iso_fortran_env
    use fcore_common

    implicit none
    private

    integer, parameter :: BUFFER_SIZE = 2**14
    character (*), parameter :: COMMENT_CHARS = ';#'

    type, public :: ini_key_value
        private

        type (str) :: name
        type (str) :: value

    contains
        procedure, pass :: ini_key_value_parse
        generic :: parse => ini_key_value_parse

        procedure, pass :: ini_key_value_get
        generic, public :: get => ini_key_value_get

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
        procedure, pass :: ini_section_parse
        generic, public :: parse => ini_section_parse

        procedure, pass :: ini_section_has_key_char
        procedure, pass :: ini_section_has_key_str
        generic, public :: has_key => ini_section_has_key_char, &
            ini_section_has_key_str

        procedure, pass :: ini_section_get_char, ini_section_get_str
        generic, public :: get => ini_section_get_char, &
            ini_section_get_str
    end type



    type, public :: ini_file
        private

        type (str), public :: path
        type (ini_section), dimension(:), allocatable :: sections

    contains
        procedure, pass :: ini_file_parse_file
        procedure, pass :: ini_file_parse_char
        procedure, pass :: ini_file_parse_char_array
        generic, public :: parse => ini_file_parse_file, ini_file_parse_char, &
            ini_file_parse_char_array

        procedure, pass :: ini_file_add_section_section
        procedure, pass :: ini_file_add_section_char_array
        generic, public :: add_section => ini_file_add_section_section, &
            ini_file_add_section_char_array

        procedure, pass :: ini_file_has_section_char
        procedure, pass :: ini_file_has_section_str
        generic, public :: has_section => ini_file_has_section_char, &
            ini_file_has_section_str

        procedure, pass :: ini_file_get_char
        procedure, pass :: ini_file_get_str
        generic, public :: get => ini_file_get_char, ini_file_get_str

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

    integer :: iostat, uid, nlines

    type (status_t) :: lstatus
    character (1024) :: lmsg
    character (BUFFER_SIZE), allocatable, dimension(:) :: data

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

    call self%parse (data, status)

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

    integer :: iostat, nlines, ipos, ioffset

    type (status_t) :: lstatus
    character (1024) :: lmsg
    character (BUFFER_SIZE), allocatable, dimension(:) :: data
    character (*), parameter :: NEWLINE_CHARS = new_line ('A')

    lstatus = FC_STATUS_UNDEFINED
    lmsg = ''

    ! Find number of lines in input
    nlines = 1
    ioffset = 0
    do while (ioffset < len (contents))
        ipos = scan (contents(ioffset+1:), NEWLINE_CHARS)
        if (ipos > 0) then
            nlines = nlines + 1
            ioffset = ioffset + ipos
        else
            ! No more newline characters
            exit
        end if
    end do

    allocate (data(nlines))

    read (contents, fmt=*, iostat=iostat, iomsg=lmsg, err=100) data

    ! End of string reached, pass on to internal parsing routine
    call self%parse (data, status)

    return

100 continue

    lstatus = FC_STATUS_IO_ERROR

    if (present(status)) then
        status = lstatus
        status%msg = lmsg
    end if

end subroutine



subroutine ini_file_parse_char_array (self, contents, status)
    class (ini_file), intent(inout) :: self
    character (BUFFER_SIZE), intent(in), dimension(:) :: contents
    type (status_t), intent(out), optional :: status

    integer :: i, j, nsections, nlines
    character (BUFFER_SIZE) :: line
    character (BUFFER_SIZE), dimension(:), allocatable :: lines
    type (status_t) :: lstatus

    lstatus = FC_STATUS_OK

    ! --- Determine numnber of non-empty lines ---

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
    end do

    ! --- Parse actual contents ---

    i = 1
    nsections = 0

    do while (i <= size (lines))
        if ( (index(lines(i), '[') == 0) .or. (index(lines(i), ']') == 0)) then
            lstatus = FC_STATUS_PARSE_ERROR
            goto 100
        end if

        ! Find end of section (either end of file or beginning of next section)
        do j = i+1, size(lines) - 1
            if (index (lines(j), '[') == 1) exit
        end do

        call self%add_section (lines(i:j))

        ! Shift index for next section
        i = j + 1
    end do

100 continue

    if (present(status)) status = lstatus

end subroutine



subroutine ini_file_add_section_char_array (self, contents, status)
    class (ini_file), intent(inout) :: self
    character (BUFFER_SIZE), intent(in), dimension(:) :: contents
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




subroutine ini_file_add_section_section (self, section, status)
    class (ini_file), intent(inout) :: self
    class (ini_section), intent(in) :: section
    type (status_t), intent(out), optional :: status

    type (status_t) :: lstatus

    integer :: n
    type (ini_section), dimension(:), allocatable :: sections

    lstatus = FC_STATUS_OK

    if (any (self%sections%name == section%name)) then
        lstatus = FC_STATUS_VALUE_ERROR
        lstatus%msg = "Duplicate section name " // section%name
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
    class (str), intent(in) :: name
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


subroutine ini_file_get_str (self, section, key, val, status)
    class (ini_file), intent(in) :: self
    class (str), intent(in) :: section
    class (str), intent(in) :: key
    class (*), intent(out), target :: val
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
    class (*), intent(out), target :: val
    type (status_t), intent(out) , optional :: status

    call self%get (str(section), str(key), val, status)
end subroutine



pure subroutine ini_section_parse (self, contents, status)
    class (ini_section), intent(inout) :: self
    character (BUFFER_SIZE), intent(in), dimension(:) :: contents
    type (status_t), intent(out), optional :: status

    integer :: ipos, nvalues, i
    character (BUFFER_SIZE) :: line
    type (status_t) :: lstatus

    lstatus = FC_STATUS_OK

    line = contents(1)

    ipos = index (line, ']')
    self%name = line(2:ipos-1)

    nvalues = size(contents) - 1
    allocate (self%values(nvalues))

    do i = 2, size(contents)
        call self%values(i-1)%parse (contents(2), lstatus)
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



pure function ini_section_has_key_str (self, key) result(flag)
    class (ini_section), intent(in) :: self
    class (str), intent(in) :: key
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
    class (str), intent(in) :: key
    class (*), intent(out), target :: val
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
    class (*), intent(out), target :: val
    type (status_t), intent(out) , optional :: status

    call self%get (str(key), val, status)
end subroutine



pure subroutine ini_key_value_parse (self, contents, status)
    class (ini_key_value), intent(inout) :: self
    character (*), intent(in) :: contents
    type (status_t), intent(out), optional :: status

    character (BUFFER_SIZE) :: line
    integer :: ipos, n
    type (status_t) :: lstatus

    lstatus = FC_STATUS_OK

    line = adjustl (contents)

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

    self%name = line(1:ipos-1)
    self%value = line(ipos+1:n)

100 continue

    if (present(status)) status = lstatus

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



subroutine ini_key_value_serialize (self, char)
    type (ini_key_value), intent(in) :: self



end subroutine


end module
