# CRAN Submission Comments for gnucashr

## Test Environments

### Local Development
- Ubuntu 22.04, R 4.4.0 (linux-gnu-x86_64)
- macOS Sonoma 14.x, R 4.4.0 (aarch64-apple-darwin)

### GitHub Actions
- ubuntu-latest, R release
- ubuntu-latest, R devel
- macos-latest, R release
- windows-latest, R release

### R-hub
- Ubuntu Linux 22.04.1 LTS, R-devel, GCC
- Fedora Linux, R-devel, clang, gfortran
- Windows Server 2022, R-devel, 64 bit

### win-builder
- R-devel
- R-release

## R CMD check Results

### Local Checks
```
── R CMD check results ─────────────────────── gnucashr 0.2.0 ────
Duration: ~2-3 minutes

0 errors | 0 warnings | 0 notes
```

### Expected CRAN Notes

On first submission, the following NOTE is expected:
```
* checking CRAN incoming feasibility ... NOTE
Maintainer: 'Jess Sullivan <jess@tinyland.dev>'

New submission

Possibly misspelled words in DESCRIPTION:
  GnuCash (...)
  Rcpp (...)
  monadic (...)
```

These are technical terms and are correctly spelled. The WORDLIST file
at inst/WORDLIST contains all package-specific terms.

## Package Dependencies

### Imports (required)
- DBI: Database interface
- RSQLite: SQLite database driver
- dplyr: Data manipulation
- tibble: Modern data frames
- purrr: Functional programming
- lubridate: Date handling
- R6: R6 object system
- rlang: Base R extensions
- xml2: XML parsing
- Rcpp: C++ interface
- RcppParallel: Parallel execution
- jsonlite: JSON support
- tools: Base utilities

### LinkingTo (C++ libraries)
- Rcpp
- RcppParallel
- RcppArmadillo

### Suggests (optional enhancements)
- testthat (>= 3.0.0): Unit testing
- withr: Test fixtures
- keyring: Secure credential storage
- hedgehog: Property-based testing
- data.table: High-performance data operations
- ggplot2: Visualization
- scales: Axis formatting
- future, furrr: Parallel evaluation
- targets, tarchetypes: Workflow management
- knitr, rmarkdown: Documentation
- gt: Table formatting
- shiny: Interactive dashboards

## System Requirements

- C++17 compiler (GCC >= 7, Clang >= 5, MSVC >= 19.14)
- GNU make

System requirements are documented in DESCRIPTION.

## Downstream Dependencies

None. This is a new package with no reverse dependencies.

## Special Considerations

### Compiled Code
This package contains C++ code via Rcpp and RcppParallel:
- `src/fractions.cpp`: Exact fraction arithmetic
- `src/guid.cpp`: GUID generation
- `src/monte_carlo.cpp`: Parallel Monte Carlo simulation
- `src/validation.cpp`: Data validation

All compiled code has been tested with:
- AddressSanitizer (ASAN)
- Undefined Behavior Sanitizer (UBSAN)
- valgrind memcheck

### Thread Safety
RcppParallel is used for Monte Carlo simulations. Thread count respects
`RcppParallel::setThreadOptions()` and defaults to 2 threads on CRAN
check systems.

### External Files
The package reads GnuCash files (SQLite or XML format) but does not
write to any system directories. All file operations are explicit and
user-controlled.

## Resubmission Notes

This is a new submission.

## Maintainer Notes

- The package is actively maintained at https://gitlab.com/tinyland/projects/gnucashr
- Issues can be reported at https://gitlab.com/tinyland/projects/gnucashr/-/issues
- GitHub mirror: https://github.com/Jesssullivan/gnucashr
- The package includes comprehensive test coverage (see tests/testthat/)
- Vignettes are provided for common use cases

---

Submitted by: Jess Sullivan <jess@tinyland.dev>
Date: 2026-01-25
