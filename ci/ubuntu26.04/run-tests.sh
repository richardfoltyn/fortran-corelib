#!/usr/bin/env bash
# Build and validate one fcore configuration.
#
# - Select GNU 11–16 or ifx; use gcc-15 for C with ifx.
# - Keep fresh builds, provenance, installs, and temporary files under BUILD_ROOT.
# - Run tests sequentially, then validate a relocated Fortran-only client.
# - Return nonzero when CTest or any build/install/client step fails.
# ==============================================================================
# Execution environment
# ==============================================================================
set -euo pipefail
export LC_ALL=C
ulimit -c 0

# ==============================================================================
# Configuration
# ==============================================================================
if (( $# < 1 || $# > 2 )); then
    echo 'Usage: run-tests.sh <11..16|ifx> [Debug|Release]' >&2
    exit 2
fi
compiler=${1}
config=${2:-Debug}
shared=${BUILD_SHARED_LIBS:-OFF}
tests=${FCORE_BUILD_TESTS:-ON}
examples=${FCORE_BUILD_EXAMPLES:-ON}
diagnostics=${FCORE_DIAGNOSTICS:-OFF}
if [[ ! ${compiler} =~ ^(11|12|13|14|15|16|ifx)$ || ! ${config} =~ ^(Debug|Release)$ ]]; then
    echo 'Invalid compiler or configuration.' >&2
    exit 2
fi
for value in "${shared}" "${tests}" "${examples}" "${diagnostics}"; do
    if [[ ! ${value} =~ ^(ON|OFF)$ ]]; then
        echo 'Options must be ON/OFF.' >&2
        exit 2
    fi
done
if [[ ${compiler} == ifx ]]; then
    cc=gcc-15
    fc=ifx
    toolchain=ifx
else
    cc=gcc-${compiler}
    fc=gfortran-${compiler}
    toolchain=gnu-${compiler}
fi
source_dir=${FCORE_SOURCE_DIR:-/src/fcore}
mkdir -p "${BUILD_ROOT:-/build}"
BUILD_ROOT=$(cd -- "${BUILD_ROOT:-/build}" && pwd)
export BUILD_ROOT
job_dir=${BUILD_ROOT}/${toolchain}/${config}/shared-${shared}-tests-${tests}-examples-${examples}-diagnostics-${diagnostics}
mkdir -p "${job_dir}"
# Fresh work trees prevent stale registrations or installed modules on reruns.
work=$(mktemp -d "${job_dir}/run.XXXXXX")
if [[ -n ${FCORE_WORK_RECORD:-} ]]; then
    printf '%s\n' "${work}" > "${FCORE_WORK_RECORD}"
fi
mkdir "${work}/tmp"
export TMP=${work}/tmp TEMP=${work}/tmp TMPDIR=${work}/tmp
printf 'TMP=%s\nTEMP=%s\nTMPDIR=%s\n' "${TMP}" "${TEMP}" "${TMPDIR}" > "${work}/environment.log"

# ==============================================================================
# Environment and provenance
# ==============================================================================
unset CC CXX FC F77 F90 FFLAGS FCFLAGS CFLAGS CXXFLAGS CPPFLAGS LDFLAGS
unset CMAKE_PREFIX_PATH CMAKE_MODULE_PATH CMAKE_TOOLCHAIN_FILE fcore_DIR fcore_ROOT FCORE_ROOT
unset CMAKE_LIBRARY_PATH CMAKE_INCLUDE_PATH CMAKE_FRAMEWORK_PATH CMAKE_APPBUNDLE_PATH
unset CMAKE_PROGRAM_PATH CMAKE_FIND_ROOT_PATH
unset CPATH C_INCLUDE_PATH CPLUS_INCLUDE_PATH
# with-oneapi supplies runtime/link directories required by ifx. Keep only
# existing Intel directories, excluding inherited host library search paths.
for variable in LIBRARY_PATH LD_LIBRARY_PATH; do
    retained=()
    if [[ ${compiler} == ifx ]]; then
        IFS=: read -r -a paths <<< "${!variable:-}"
        for path in "${paths[@]}"; do
            if [[ -d ${path} ]]; then
                path=$(realpath -- "${path}")
                case ${path} in /opt/intel/oneapi/*) retained+=("${path}");; esac
            fi
        done
    fi
    unset "${variable}"
    if (( ${#retained[@]} )); then
        printf -v "${variable}" '%s' "$(IFS=:; echo "${retained[*]}")"
        export "${variable?}"
    fi
done
unset PKG_CONFIG_PATH PKG_CONFIG_LIBDIR PKG_CONFIG_SYSROOT_DIR
export CMAKE_BUILD_PARALLEL_LEVEL=${CMAKE_BUILD_PARALLEL_LEVEL:-4}
{
    "${cc}" --version
    "${fc}" --version
    cmake --version
    ninja --version
    if [[ -r /opt/fortran-ci/packages.tsv ]]; then
        cp /opt/fortran-ci/packages.tsv "${work}/packages.tsv"
        cat "${work}/packages.tsv"
    elif command -v dpkg-query >/dev/null; then
        dpkg-query -W > "${work}/packages.tsv"
        cat "${work}/packages.tsv"
    else
        echo 'Package inventory unavailable on this host.'
    fi
} > "${work}/versions.log"
cat "${work}/versions.log"
common=(-G Ninja "-DCMAKE_Fortran_COMPILER=${fc}" "-DCMAKE_BUILD_TYPE=${config}"
    -DCMAKE_FIND_USE_PACKAGE_REGISTRY=OFF -DCMAKE_FIND_USE_SYSTEM_PACKAGE_REGISTRY=OFF)

# ==============================================================================
# Configure and build
# ==============================================================================
cmake -S "${source_dir}" -B "${work}/fcore" "${common[@]}" \
    "-DCMAKE_C_COMPILER=${cc}" "-DCMAKE_INSTALL_PREFIX=${work}/install" \
    "-DBUILD_SHARED_LIBS=${shared}" "-DFCORE_BUILD_TESTS=${tests}" \
    "-DFCORE_BUILD_EXAMPLES=${examples}" "-DFCORE_DIAGNOSTICS=${diagnostics}"
cmake --build "${work}/fcore"

# ==============================================================================
# Run tests and record CTest's exit status
# ==============================================================================
ctest_code=0
if [[ ${tests} == ON ]]; then
    ctest --test-dir "${work}/fcore" --parallel 1 --verbose --no-tests=error > "${work}/ctest.log" 2>&1 || ctest_code=$?
    cat "${work}/ctest.log"
else
    ctest --test-dir "${work}/fcore" --parallel 1 --show-only > "${work}/ctest.log" 2>&1
    cat "${work}/ctest.log"
    if ! grep -q 'Total Tests: 0$' "${work}/ctest.log"; then ctest_code=1; fi
fi
printf 'ctest_exit\n%s\n' "${ctest_code}" > "${work}/test-results.tsv"

# ==============================================================================
# Install, relocate, and consume the exported Fortran target
# ==============================================================================
cmake --install "${work}/fcore"
mv "${work}/install" "${work}/relocated"
mkdir "${work}/consumer"
cat > "${work}/consumer/CMakeLists.txt" <<'CMAKE'
cmake_minimum_required(VERSION 4.1)
project(consumer LANGUAGES Fortran)
find_package(fcore CONFIG REQUIRED)
add_executable(consumer main.f90)
target_link_libraries(consumer PRIVATE fcore::fcore)
CMAKE
cat > "${work}/consumer/main.f90" <<'FORTRAN'
program consumer
    use fcore_strings, only: str
    implicit none
    type(str) :: message
    message = str('relocated fcore')
    if (message%to_char() /= 'relocated fcore') error stop 1
    print *, message%to_char()
end program
FORTRAN
cmake -S "${work}/consumer" -B "${work}/client" "${common[@]}" \
    "-DCMAKE_PREFIX_PATH=${work}/relocated"
cmake --build "${work}/client"
"${work}/client/consumer"
printf 'Artifacts: %s\nCTest exit: %s\n' "${work}" "${ctest_code}"
if (( ctest_code != 0 )); then exit 1; fi
