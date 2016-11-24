# Fortran Corelib TODO #

## argparse ##

-   When parsing, verify that all required arguments are present
-   Action `ACTION_COUNT` that collects the number of times an argument was
    specified
-   Collect remaining arguments that do not match any defined argument
    in user-accessible str list
-   Predefined `--help` and `-h` switches
-   Printing of help text


## collections ##
-   Fix bug with recursive finalizer in gfortran


## strings ##

-   Create unit tests for `count()` method
-   Implement `upper()` method
-   Add unit tests for `lower()` and `upper()`
-   Unit tests from `trim()` method / generic
-   Implement other intrinsic functions such as adjustl(), adjustr()
