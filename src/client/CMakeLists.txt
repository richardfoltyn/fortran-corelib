
cmake_minimum_required(VERSION 3.0)
project(fcore_client Fortran)

find_package(fcore REQUIRED)

add_executable(hellofcore hellofcore.f90)
target_link_libraries(hellofcore fcore::fcore)
