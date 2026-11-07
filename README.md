# Fortran corelib (fcore) #

A Fortran library that provides some basic functionality taken for
granted in other languages or their standard libraries, such as
a string type, command-line argument parsing, etc.

***
## Installation ##

Requires CMake 4.1 or later, a C compiler, and a Fortran compiler.

### Linux ###

The current source has been built and tested on Linux with:

1.  GNU `gfortran` 16.2.1
2.  Intel `ifx` 2026.2.0

Earlier revisions also worked with GNU `gfortran` 11.x to 15.x, Intel `ifort`
2021.x, and Intel `ifx` 2024. These compiler versions have not been revalidated
against the current source.

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

### Build options and tests ###

The default standalone build is Release with a static library. Set
`CMAKE_BUILD_TYPE` explicitly for another single-configuration build, or use
`--config` when building and testing with a multi-configuration generator.

| Option | Default | Purpose |
| --- | --- | --- |
| `BUILD_SHARED_LIBS` | `OFF` | Build a shared library instead of a static library. |
| `FCORE_BUILD_TESTS` | `OFF` | Build and register the unit tests with CTest. |
| `FCORE_BUILD_EXAMPLES` | `OFF` | Build the examples. |
| `FCORE_DIAGNOSTICS` | `OFF` | Enable additional library diagnostic messages. |
| `FCORE_INSTALL_DIR_NAME` | Derived from version and compiler | Override the installation subdirectory. |

Standard `CMAKE_INSTALL_LIBDIR`, `CMAKE_INSTALL_INCLUDEDIR`, and
`CMAKE_INSTALL_BINDIR` overrides are supported. Build outputs remain inside the
build tree regardless of those installation paths. Windows shared-library DLLs
are installed below `CMAKE_INSTALL_BINDIR`; libraries and package files are
installed below `CMAKE_INSTALL_LIBDIR`.

For a development build, run from the repository root:

```sh
cmake -S . -B build -DCMAKE_BUILD_TYPE=Debug \
    -DFCORE_BUILD_TESTS=ON -DFCORE_BUILD_EXAMPLES=ON
cmake --build build --parallel
ctest --test-dir build --output-on-failure
```

Tests and examples link the selected static or shared `fcore` library. Fortran
module files use a project-local directory shared by configurations. With Ninja
Multi-Config, build configurations sequentially; use separate build trees for
concurrent Debug and Release builds.

***
## Usage

To integrate the library in your own CMake project, augment your `CMakeLists.txt`
as follows. See also the minimal client in `examples/client/`.
```CMake
cmake_minimum_required(VERSION 4.1)

project(fcore_client LANGUAGES Fortran)

find_package(fcore CONFIG REQUIRED)

add_executable(client client.f90)
target_link_libraries(client PRIVATE fcore::fcore)

```

For source-tree integration, use `add_subdirectory()` or `FetchContent` and link
against the same `fcore::fcore` target. Its module include paths propagate
automatically. The build does not set a parent's default configuration or add
compiler flags to unrelated targets.

Installed packages require the same Fortran compiler family and major version
used to build fcore. Versioned requests accept compatible releases within the
same minor version (`0.1.x` for this release); use `EXACT` for an exact version.

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
