

module fcore_strings
    !*  Module exports string-related types and routines from FCORE_COMMON_BASE.
    !   Can be USEd by client code to import only interfaces relevant
    !   to string manipulation and omit other stuff in FCORE_COMMON_BASE.

    use fcore_common_base, only: &
        str, &
        str_array, &
        char, &
        len, &
        len_trim, &
        repeat, &
        index, &
        trim, &
        is_alnum, &
        is_alpha, &
        is_digit, &
        dynamic_cast, &
        copy_alloc, &
        assignment(=), &
        operator (==), &
        operator (//)

end module
