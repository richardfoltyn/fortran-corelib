# Fortran Corelib TODO #

## argparse ##

-   When parsing, verify that all required arguments are present
-   Action `ACTION_COUNT` that collects the number of times an argument was
    specified
-   Action `ACTION_APPEND` that collects values of argument specified
    multiple times
-   Collect remaining arguments that do not match any defined argument
    in user-accessible str list
-   Predefined `--help` and `-h` switches
-   Printing of help text

## strings ##

-   Create unit tests for count() method
