#!/usr/bin/env bash

# Schedule fcore's compiler and build-option matrix with CTest.
#
# - Run inside the Fortran CI image with fcore sources mounted.
# - List configurations with --list, or select jobs by their matrix names.
# - Select all configurations when no job names are given.
# - Limit concurrent configurations with --jobs N (default: 1).
# - Use with-oneapi for selections that include ifx.
# - Save the manifest, worker logs, and ordered summary under BUILD_ROOT/matrix.
# - Return nonzero for failed or incomplete jobs.

# ==============================================================================
# Execution environment
# ==============================================================================
set -euo pipefail
export LC_ALL=C
ulimit -c 0
script_dir=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)

# ==============================================================================
# Matrix definition
# ==============================================================================
# Deduplicate identical configurations while keeping readable, stable job names.
declare -A seen=() jobs=()
order=()
add_job() {
    local name=${1} key
    shift
    key="${*}"
    if [[ -n ${seen[${key}]:-} ]]; then return; fi
    seen[${key}]=1
    jobs[${name}]=${key}
    order+=("${name}")
}
for major in 11 12 13 14 15 16; do
    add_job "gnu${major}-debug-static" "${major}" Debug OFF ON ON OFF
    add_job "gnu${major}-release-shared" "${major}" Release ON ON ON OFF
done
# Ubuntu 26.04's default compiler in the shared image is GCC 15.
for shared in OFF ON; do
    for tests in OFF ON; do
        for examples in OFF ON; do
            for diagnostics in OFF ON; do
                add_job "gnu15-options-${shared}-${tests}-${examples}-${diagnostics}" \
                    15 Debug "${shared}" "${tests}" "${examples}" "${diagnostics}"
            done
        done
    done
done

add_job ifx-debug-static ifx Debug OFF ON ON OFF
add_job ifx-release-shared ifx Release ON ON ON OFF

# ==============================================================================
# Job selection
# ==============================================================================
parallel=1
if [[ ${1:-} == --jobs ]]; then
    if [[ ! ${2:-} =~ ^[1-9][0-9]*$ ]]; then
        printf 'Usage: run-matrix.sh [--jobs N] [job ...]\n' >&2
        exit 2
    fi
    parallel=${2}
    shift 2
fi
if [[ ${1:-} == --list ]]; then
    if (( $# != 1 )); then
        printf 'Use --list by itself.\n' >&2
        exit 2
    fi
    printf 'job\tcompiler\tconfiguration\tshared\ttests\texamples\tdiagnostics\n'
    for name in "${order[@]}"; do
        printf '%s\t%s\n' "${name}" "${jobs[${name}]// /$'\t'}"
    done
    exit
fi
selected=("${order[@]}")
if (( $# > 0 )); then
    selected=("${@}")
fi
# Validate the whole selection before starting any builds.
declare -A selected_names=()
for name in "${selected[@]}"; do
    if [[ -z ${jobs[${name}]:-} ]]; then
        printf 'Unknown matrix job: %s (use --list).\n' "${name}" >&2
        exit 2
    fi
    if [[ -n ${selected_names[${name}]:-} ]]; then
        printf 'Duplicate matrix job: %s\n' "${name}" >&2
        exit 2
    fi
    selected_names[${name}]=1
done

# ==============================================================================
# Run directories and job manifest
# ==============================================================================
mkdir -p "${BUILD_ROOT:-/build}"
BUILD_ROOT=$(cd -- "${BUILD_ROOT:-/build}" && pwd)
export BUILD_ROOT
output=${BUILD_ROOT}/matrix
mkdir -p "${output}"
run_dir=$(mktemp -d "${output}/run.XXXXXX")
run_dir=$(cd -- "${run_dir}" && pwd)
for name in "${selected[@]}"; do
    printf '%s\t%s\n' "${name}" "${jobs[${name}]// /$'\t'}"
done > "${run_dir}/jobs.tsv"

# ==============================================================================
# CTest scheduling
# ==============================================================================
result=0
if cmake -S "${script_dir}/matrix" -B "${run_dir}/scheduler" \
    "-DMATRIX_RUN_DIR=${run_dir}" > "${run_dir}/configure.log" 2>&1; then
    # Ignore inherited stop-on-failure settings; every selected worker must run.
    CTEST_STOP_ON_FAILURE=0 ctest --test-dir "${run_dir}/scheduler" \
        --parallel "${parallel}" --output-on-failure \
        > "${run_dir}/scheduler.log" 2>&1 || result=1
    cat "${run_dir}/scheduler.log"
else
    cat "${run_dir}/configure.log"
    result=1
fi

# ==============================================================================
# Ordered result summary
# ==============================================================================
# Workers write separate rows; merge them in selection order after CTest exits.
summary=${run_dir}/summary.tsv
printf 'job\tresult\texit_code\tctest_exit\tlog\n' > "${summary}"
for name in "${selected[@]}"; do
    if [[ -f ${run_dir}/${name}.tsv ]]; then
        cat -- "${run_dir}/${name}.tsv" >> "${summary}"
        IFS=$'\t' read -r _ status _ < "${run_dir}/${name}.tsv"
        if [[ ${status} != PASS ]]; then result=1; fi
    else
        printf '%s\tINCOMPLETE\t-\t-\t%s\n' "${name}" "${run_dir}/${name}.log" >> "${summary}"
        result=1
    fi
done
cat -- "${summary}"
printf 'Matrix summary: %s\n' "${summary}"
exit "${result}"
