# Fortran corelib (FCORE) #

## About ##

Fortran library which provides come basic functionality that is taken for 
granted in other languages or their standard libraries, such as
a string type, command-line argument parsing, etc.

## Obtaining the code ##

To clone the git repository, run
```bash
git clone https://bitbucket.org/richardfoltyn/fortran-corelib.git
```
## Build instructions ##

### Linux ###

The library has been tested with the following compilers:

1.  Intel `ifort` 2018 and 2019
2.  GNU `gfortran` 7.x and 8.x

To compile and install FCORE, create a build directory and run

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
make -j 16
make install
```

