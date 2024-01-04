module fcore_common_constants

    use fcore_common_kinds
    implicit none

    integer (FC_ENUM_KIND), public, parameter :: FC_STATUS_UNDEFINED = 0
    integer (FC_ENUM_KIND), public, parameter :: FC_STATUS_OK = ishft(1, 0)
    integer (FC_ENUM_KIND), public, parameter :: FC_STATUS_VALUE_ERROR = ishft(1, 10)
    integer (FC_ENUM_KIND), public, parameter :: FC_STATUS_UNKNOWN = ishft(1, 11)
    integer (FC_ENUM_KIND), public, parameter :: FC_STATUS_UNSUPPORTED_OP = ishft(1, 12)
    integer (FC_ENUM_KIND), public, parameter :: FC_STATUS_INVALID_STATE = ishft(1, 13)
    integer (FC_ENUM_KIND), public, parameter :: FC_STATUS_TYPE_ERROR = ishft(1, 14)
    integer (FC_ENUM_KIND), public, parameter :: FC_STATUS_STORAGE_ERROR = ishft(1, 18)
    integer (FC_ENUM_KIND), public, parameter :: FC_STATUS_NOT_ASSOCIATED = ishft(1, 19)
    integer (FC_ENUM_KIND), public, parameter :: FC_STATUS_PARSE_ERROR = ishft(1, 21)
    integer (FC_ENUM_KIND), public, parameter :: FC_STATUS_IO_ERROR = ishft(1, 22)

end module
