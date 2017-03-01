module corelib_strings

    ! Compatibility module: import corelib_common_base, which now
    ! defines the str type.
    use corelib_common_base, only: str, str_array, len, len_trim, repeat, &
        index, trim, is_alnum, is_alpha, is_digit, dynamic_cast, &
        operator(==), operator(/=), operator(//), assignment (=)

    implicit none
end module
