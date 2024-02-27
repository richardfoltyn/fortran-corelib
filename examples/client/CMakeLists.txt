
cmake_minimum_required(VERSION 3.12)

project(fcore_client Fortran)

find_package(fcore REQUIRED)

add_executable(client client.f90)
target_link_libraries(client fcore::fcore)
