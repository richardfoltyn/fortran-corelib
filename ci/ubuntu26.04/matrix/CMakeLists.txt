cmake_minimum_required(VERSION 4.1)
project(fcore_ci_matrix LANGUAGES NONE)

enable_testing()
find_program(BASH_EXECUTABLE bash REQUIRED)
if(NOT DEFINED MATRIX_RUN_DIR)
    message(FATAL_ERROR "Use run-matrix.sh to select jobs and create the manifest.")
endif()
file(STRINGS "${MATRIX_RUN_DIR}/jobs.tsv" jobs)
foreach(row IN LISTS jobs)
    string(REPLACE "\t" ";" fields "${row}")
    list(GET fields 0 name)
    add_test(NAME "${name}" COMMAND "${BASH_EXECUTABLE}"
        "${CMAKE_CURRENT_SOURCE_DIR}/../run-matrix-job.sh"
        "${MATRIX_RUN_DIR}" ${fields})
endforeach()
