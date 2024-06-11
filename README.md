# Fortran corelib (fcore) #

Fortran library which provides come basic functionality that is taken for 
granted in other languages or their standard libraries, such as
a string type, command-line argument parsing, etc.

## Installation ##

### Linux ###

The library has been tested with the following compilers:

1.  GNU `gfortran` 11.x, 12.x and 13.x
2.  Intel `ifort` 2021.x, `ifx` 2024

To compile and install `fcore`, adapt to following to your environment:
```bash
# Define GCC compiler version
GCC_VERSION=14

# Path to source directory
SRC_DIR=$HOME/repos/fortran-corelib

# Define installation prefix
INSTALL_PREFIX=$HOME/.local

# Build directory
BUILD_DIR=$HOME/build/gnu/${GCC_VERSION}/fortran-corelib

mkdir -p "${BUILD_DIR}"
cd "${BUILD_DIR}"
```

On Debian/Ubuntu, the project can now be configured as follows:
```bash
FC=gfortran-${GCC_VERSION} CC=gcc-${GCC_VERSION} \
cmake -DCMAKE_INSTALL_PREFIX=${INSTALL_PREFIX} ${SRC_DIR}
```
On Fedora, the default GCC compiler does not have a version suffix,
so the following should be used instead:
```bash
FC=gfortran CC=gcc cmake -DCMAKE_INSTALL_PREFIX=${INSTALL_PREFIX} ${SRC_DIR}
```

To build and install the project, run
```bash
cmake --build .
cmake --install .
```

## Usage

To integrate the library in your own CMake project, augment your `CMakeLists.txt`
as follows. See also the minimal client in `examples/client/`.
```CMake
cmake_minimum_required(VERSION 3.12)

project(fcore_client Fortran)

find_package(fcore REQUIRED)

add_executable(client client.f90)
target_link_libraries(client fcore::fcore)

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
