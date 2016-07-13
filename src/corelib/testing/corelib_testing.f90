! wrapper module to export relevant unittest components
module corelib_testing

    use test_case_mod, only: test_case
    use test_suite_mod, only: test_suite

    implicit none
    private

    public :: test_case, test_suite

end module
