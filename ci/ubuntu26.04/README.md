# Local Ubuntu 26.04 CI

- Use the existing `fortran-ci:ubuntu26.04` image, maintained in `../fortran-ci`.
  Its `with-oneapi` wrapper initializes the Intel compiler environment.
- Run as your host user, mount sources read-only, and keep artifacts in a
  separate writable directory. No image build is required.

## Usage and parallel execution

From the repository root, run the full matrix with up to eight configurations
at once and four build processes per configuration:

```sh
mkdir -p build-ci
docker run --rm --ulimit core=0 --user "$(id -u):$(id -g)" \
    -e CMAKE_BUILD_PARALLEL_LEVEL=4 \
    -v "$(pwd):/src/fcore:ro,z" -v "$(pwd)/build-ci:/build:z" \
    fortran-ci:ubuntu26.04 with-oneapi \
    bash /src/fcore/ci/ubuntu26.04/run-matrix.sh --jobs 8
```

- `run-matrix.sh` selects all jobs and defaults to `--jobs 1`. Each selected
  configuration is one outer CTest test; inner CTest runs use `--parallel 1`.
- Use `run-matrix.sh --list` to print job names and option values without
  building. Append names to select jobs, for example,
  `run-matrix.sh --jobs 2 gnu15-debug-static ifx-debug-static`.
- Unknown names, duplicate names, and invalid parallelism are rejected before
  scheduling. Jobs continue after another configuration fails.
- Replace the matrix command with `run-tests.sh 15 Debug` or
  `run-tests.sh ifx Release` for a single configuration. GNU-only selections
  can omit `with-oneapi`.
- To run with local tools, set `FCORE_SOURCE_DIR` to this checkout and
  `BUILD_ROOT` to a writable directory. Intel jobs require the oneAPI environment.

## Matrix and options

- The matrix contains 29 deduplicated configurations. Its authoritative
  definition is in `run-matrix.sh`; the CMake scheduler reads the selected
  configurations from a generated manifest.
- GNU 11–16 jobs are named `gnuN-debug-static` and `gnuN-release-shared`.
  Both enable tests and examples and select matching versioned C and Fortran
  compilers.
- Ubuntu's default compiler also covers all tests, examples, library type, and
  diagnostics combinations in Debug mode. Names follow
  `gnu15-options-SHARED-TESTS-EXAMPLES-DIAGNOSTICS`, with ON/OFF values.
  The overlapping configuration uses the name `gnu15-debug-static`.
- `ifx-debug-static` and `ifx-release-shared` enable tests and examples, use
  `gcc-15` for C, and use `ifx` for both fcore and its installed-package client.
  There are no MKL jobs or BLAS/LAPACK dependencies.
- Single-job switches are `BUILD_SHARED_LIBS`, `FCORE_BUILD_TESTS`,
  `FCORE_BUILD_EXAMPLES`, and `FCORE_DIAGNOSTICS`, each accepting ON/OFF.
  Defaults are static libraries, tests and examples enabled, and diagnostics
  disabled. Pass overrides with Docker's `-e` before the image name.
- `CMAKE_BUILD_PARALLEL_LEVEL` controls build parallelism independently of
  `--jobs` and defaults to 4. Choose both limits to suit available CPU and memory.

## Checks and artifacts

- Each job uses a fresh build tree and isolated `TMP`, `TEMP`, and `TMPDIR`.
  Compiler flags and package paths are cleared; ifx retains only Intel oneAPI
  library paths needed for linking and execution.
- Each job runs registered tests, or verifies that none are registered when
  tests are disabled. It then installs and relocates fcore and runs a separate
  Fortran-only client using `find_package(fcore CONFIG REQUIRED)` and
  `fcore::fcore`. The original installation prefix no longer exists.
- Each matrix invocation keeps its artifacts under
  `build-ci/matrix/run.XXXXXX/`: `jobs.tsv`, `configure.log`, `scheduler.log`,
  the scheduler's CTest logs, and `summary.tsv`.
- Workers write separate `JOB.log`, `JOB.tsv`, `JOB.timing.tsv`, and `JOB.work`
  files. The summary merges results in selection order, regardless of completion
  order. Timing files record worker start and end times for checking overlap.
- `JOB.work` points to the fresh tree under `artifacts/`, organized by compiler,
  configuration, and options. It contains `versions.log`, `packages.tsv`,
  `environment.log`, `test-results.tsv`, `ctest.log`, build/install trees, and
  `fcore/Testing/Temporary/LastTest.log` when tests run.
- Core dumps are disabled in Docker and Bash to prevent desktop crash-report
  popups. Crashes still count as failures, and diagnostic logs are retained.

## Result status

- `PASS` means the job completed successfully with no failed assertions.
- `ASSERTIONS` means assertions failed despite a successful CTest exit
  ([runner issue #2](https://github.com/richardfoltyn/fortran-corelib/issues/2)).
  The job returns exit code 3 and makes the matrix fail.
- `FAIL` means the job returned another nonzero exit code. Check its log for
  configure, build, executable, or installed-client failures.
- `INCOMPLETE` means no complete worker result was produced, for example, after
  a scheduler setup failure or worker interruption. It also makes the matrix fail.
- `test-results.tsv` records CTest exit status and assertion counts separately;
  inspect both alongside the full test output.
