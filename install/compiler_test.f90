program compiler_test

    use fcore, only: FCORE_VERSION
    use fcore_io, only: is_dir

    implicit none

    ! Check both module compatibility and linking to a library procedure.
    if (len(FCORE_VERSION) == 0) stop 1
    if (.not. is_dir('.')) stop 1

end program
