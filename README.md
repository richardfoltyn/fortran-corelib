# Fortran corelib (FCORE) #

## About ##

Fortran library which provides come basic functionality that is taken for 
granted in other languages or their standard libraries, such as
a string type, command-line argument parsing, etc.

## Obtaining the code ##

To clone the git repository, run

    git clone https://bitbucket.org/richardfoltyn/fortran-corelib.git

## Build instructions ##

### Linux ###

The library has been tested with the following compilers:

1.  Intel `ifort` 2018 and 2019
2.  GNU `gfortran` 7.x and 8.x

To compile and install FCORE, create a build directory and run

    cd /path/to/build/dir
    cmake -DCMAKE_INSTALL_PREFIX=/path/to/install/dir <FCORE_REPOSITORY>/src
    make install
    
where `<FCORE_REPOSITORY>` points to the git repository.

Use the environment variables `FC` and `FFLAGS` to select a compiler 
or compiler flags other than the defaults. 
For example, to build with `ifort` and optimize
for the host machine architecture, use something like

    FC=ifort FFLAGS="-xHost" cmake -DCMAKE_INSTALL_PREFIX=/path/to/install/dir <FCORE_REPOSITORY>/src
