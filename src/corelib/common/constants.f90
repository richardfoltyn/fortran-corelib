module corelib_common_constants

    use corelib_common_kinds
    implicit none

    integer (CL_ENUM_KIND), public, parameter :: CL_STATUS_UNDEFINED = 0
    integer (CL_ENUM_KIND), public, parameter :: CL_STATUS_OK = ishft(1, 0)
    integer (CL_ENUM_KIND), public, parameter :: CL_STATUS_VALUE_ERROR = ishft(1, 10)
    integer (CL_ENUM_KIND), public, parameter :: CL_STATUS_UNKNOWN = ishft(1, 11)
    integer (CL_ENUM_KIND), public, parameter :: CL_STATUS_UNSUPPORTED_OP = ishft(1, 12)
    integer (CL_ENUM_KIND), public, parameter :: CL_STATUS_INVALID_STATE = ishft(1, 13)
    integer (CL_ENUM_KIND), public, parameter :: CL_STATUS_TYPE_ERROR = ishft(1, 14)
    integer (CL_ENUM_KIND), public, parameter :: CL_STATUS_STORAGE_ERROR = ishft(1, 18)

end module
