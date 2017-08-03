cmake_minimum_required(VERSION 3.1)

project(fcore_consumer Fortran)


find_package(fcore REQUIRED)

set(SOURCE_FILES consumer.f90)

add_executable(consumer ${SOURCE_FILES})
target_link_libraries(consumer fcore::fcore)


