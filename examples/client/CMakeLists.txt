cmake_minimum_required(VERSION 4.1)

project(fcore_client LANGUAGES Fortran)

find_package(fcore CONFIG REQUIRED)

add_executable(client client.f90)
target_link_libraries(client PRIVATE fcore::fcore)
