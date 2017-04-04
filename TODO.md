# Fortran Corelib TODO #

## argparse ##

-   When parsing, verify that all required arguments are present
-   Allow for empty value list when `ACTION_APPEND` requested
-   Action `ACTION_COUNT` that collects the number of times an argument was
    specified
-   Predefined `--help` and `-h` switches
-   Printing of help text
-   Expose `allow_empty` option to user code


## collections ##
-   Fix bug with recursive finalizer in gfortran


## strings ##

-   Create unit tests for `count()` method
-   Implement `upper()` method
-   Add unit tests for `lower()` and `upper()`
-   Unit tests from `trim()` method / generic
-   Implement other intrinsic functions such as adjustl(), adjustr()
