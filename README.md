# Fortran corelib (fcore) #

Fortran library which provides come basic functionality that is taken for 
granted in other languages or their standard libraries, such as
a string type, command-line argument parsing, etc.

## Installation ##

### Linux ###

The library has been tested with the following compilers:

1.  Intel `ifort` 2024, `ifx` 2024
2.  GNU `gfortran` 11.x, 12.x and 13.x

To compile and install `fcore`, adapt to following to your environment:
```bash
# Define GCC compiler version
GCC_VERSION=12

# Path to source directory
SRC_DIR=$HOME/repos/fortran-corelib

# Define installation prefix
INSTALL_PREFIX=$HOME/.local

# Build directory
BUILD_DIR=$HOME/build/gnu/${GCC_VERSION}/fortran-corelib

mkdir -p "${BUILD_DIR}"
cd "${BUILD_DIR}"

FC=gfortran-${GCC_VERSION} CC=gcc-${GCC_VERSION} \
cmake -DCMAKE_INSTALL_PREFIX=${INSTALL_PREFIX} ${SRC_DIR}/src
```

To build and install the project, run
```bash
cmake --build .
cmake --install .
```

## Author

Richard Foltyn

## License

This program is free software: you can redistribute it and/or modify it under 
the terms of the GNU General Public License as published by the Free Software 
Foundation, either version 3 of the License, or (at your option) any later 
version.

This program is distributed in the hope that it will be useful, but WITHOUT ANY 
WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A 
PARTICULAR PURPOSE. See the GNU General Public License for more details.
