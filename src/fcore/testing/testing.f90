! wrapper module to export relevant unittest components
module fcore_testing

    use fcore_testing_test_case_mod, only: test_case
    use fcore_testing_test_suite_mod, only: test_suite

    implicit none
    private

    public :: test_case, test_suite

end module
