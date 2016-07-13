! type-independent implementation of str_array for 1d arrays

character (len=*), intent(in), optional :: fmt
type (str) :: res, sep
type (str), dimension(:), allocatable :: str_array

sep = ', '

allocate (str_array(size(x)))
str_array = str (x, fmt)
res = '[' // sep%join(str_array) // ']'

deallocate (str_array)
