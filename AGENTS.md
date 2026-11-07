# Project instructions

## CI permission

- Do not create Docker images without explicit user approval of the proposed
  image and build commands, including temporary or debugging images.
- Obtain explicit user approval for the proposed commands and job scope before
  invoking the CI framework, including `--list` and container/image operations.
- Run the full matrix only when approval explicitly covers it.
- If approval has not been given, report validation as not run.

## Build and test validation

- For isolated debugging that does not require an unavailable gfortran version,
  use a temporary directory on the host instead of creating a Docker image.
- For CI validation, use the container-based framework in `ci/ubuntu26.04/`; follow
  [its README](ci/ubuntu26.04/README.md) for setup and commands.
- Use `run-tests.sh` for a single compiler/configuration or `run-matrix.sh` for
  selected named jobs.
- Preserve `build-ci` logs and matrix summaries. Check CTest exit status and
  `Testing/Temporary/LastTest.log`; failed checks must produce a nonzero test exit.

## Language and structure

- Keep explanations in Markdown and code comments brief and easy to read.
- Prefer short bullet points over long paragraphs.
- Organize Markdown documents into sections with descriptive headings.

## Main README audience

- Write the root `README.md` for fcore users, including those building from source.
- Keep internal details and instructions for developing fcore out of this README.

## User-facing documentation

- Describe only the current version.
- When refactoring or updating the CMake build system, remove outdated information
  and do not retain or add references to the previous version.
