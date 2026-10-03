# Fortran corelib (fcore) #

A Fortran library that provides some basic functionality taken for 
granted in other languages or their standard libraries, such as
a string type, command-line argument parsing, etc.

***
## Installation ##

### Linux ###

The library has been tested with the following compilers:

1.  GNU `gfortran` 11.x to 16.x
2.  Intel `ifort` 2021.x, `ifx` 2024

Set the GCC major version in your shell. On Fedora this sets the build path;
the default compiler must be the matching version.

Bash:
```bash
GCC_VERSION=16
```

Fish:
```fish
set GCC_VERSION 16
```

The remaining commands work in both shells. CMake creates the build
directory outside the source tree.

#### Ubuntu ####

On Debian/Ubuntu, the compiler executables have version suffixes:

```sh
cmake -S "$HOME/repos/fortran-corelib" \
    -B "$HOME/build/gnu/$GCC_VERSION/fortran-corelib" \
    -DCMAKE_Fortran_COMPILER=gfortran-$GCC_VERSION \
    -DCMAKE_C_COMPILER=gcc-$GCC_VERSION \
    -DCMAKE_INSTALL_PREFIX="$HOME/.local"
```

#### Fedora ####

On Fedora, the default GCC compiler does not have a version suffix:

```sh
cmake -S "$HOME/repos/fortran-corelib" \
    -B "$HOME/build/gnu/$GCC_VERSION/fortran-corelib" \
    -DCMAKE_Fortran_COMPILER=gfortran -DCMAKE_C_COMPILER=gcc \
    -DCMAKE_INSTALL_PREFIX="$HOME/.local"
```

#### Building and installing ####

```sh
cmake --build "$HOME/build/gnu/$GCC_VERSION/fortran-corelib" --parallel 8
cmake --install "$HOME/build/gnu/$GCC_VERSION/fortran-corelib"
```

For GCC 16 on Fedora, the installed libraries and CMake package are under
`~/.local/lib64/fcore-0.1-gnu-16/`; module files are under
`~/.local/include/fcore-0.1-gnu-16/`.

***
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

***
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
