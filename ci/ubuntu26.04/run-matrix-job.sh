#!/usr/bin/env bash

# Execute one fcore configuration as an outer CTest worker.
#
# - Receive the manifest fields from the standalone scheduler.
# - Save independent logs, timing, artifact pointers, and a result row.
# - Record CTest's exit status and return the delegated runner's exit status.

# ==============================================================================
# Execution environment
# ==============================================================================
set -euo pipefail
export LC_ALL=C
ulimit -c 0
script_dir=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)

# ==============================================================================
# Worker arguments and artifact paths
# ==============================================================================
if (( $# != 8 )); then
    echo 'Internal worker: invoke through run-matrix.sh.' >&2
    exit 2
fi
run_dir=${1}
name=${2}
compiler=${3}
config=${4}
shared=${5}
tests=${6}
examples=${7}
diagnostics=${8}
log=${run_dir}/${name}.log
record=${run_dir}/${name}.work
started=$(date +%s.%N)

# ==============================================================================
# Configuration execution
# ==============================================================================
code=0
# Keep each invocation's artifacts together, including fresh job work trees.
BUILD_ROOT=${run_dir}/artifacts FCORE_WORK_RECORD=${record} \
    BUILD_SHARED_LIBS=${shared} FCORE_BUILD_TESTS=${tests} \
    FCORE_BUILD_EXAMPLES=${examples} FCORE_DIAGNOSTICS=${diagnostics} \
    bash "${script_dir}/run-tests.sh" "${compiler}" "${config}" > "${log}" 2>&1 || code=$?
finished=$(date +%s.%N)
printf 'start\tend\n%s\t%s\n' "${started}" "${finished}" > "${run_dir}/${name}.timing.tsv"

# ==============================================================================
# Independent result collection
# ==============================================================================
ctest_code=-
if [[ -f ${record} ]]; then
    read -r work < "${record}"
    if [[ -f ${work}/test-results.tsv ]]; then
        read -r ctest_code < <(tail -n 1 "${work}/test-results.tsv")
    fi
fi
status=PASS
if (( code != 0 )); then
    status=FAIL
fi
# Publish only complete rows. Missing rows are INCOMPLETE in the ordered merge.
printf '%s\t%s\t%s\t%s\t%s\n' "${name}" "${status}" "${code}" "${ctest_code}" "${log}" \
    > "${run_dir}/${name}.tsv.tmp"
mv -- "${run_dir}/${name}.tsv.tmp" "${run_dir}/${name}.tsv"
printf '%s: %s (exit %s, CTest exit %s)\nLog: %s\n' \
    "${name}" "${status}" "${code}" "${ctest_code}" "${log}"
exit "${code}"
