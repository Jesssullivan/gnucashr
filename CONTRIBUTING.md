# Contributing to gnucashr

Thank you for your interest in contributing to gnucashr! This document provides guidelines and instructions for contributing.

## Code of Conduct

Please be respectful and constructive in all interactions. We welcome contributors of all experience levels.

## Getting Started

### Development Environment Setup

1. **Clone the repository:**

   ```bash
   # From GitLab (primary)
   git clone https://gitlab.com/tinyland/projects/gnucashr.git

   # Or from GitHub (mirror)
   git clone https://github.com/Jesssullivan/gnucashr.git
   ```

2. **Install system dependencies:**

   The package requires a C++17 compiler and GNU make for the Rcpp components.

   - **macOS:** `xcode-select --install`
   - **Ubuntu/Debian:** `sudo apt-get install build-essential`
   - **Fedora/RHEL:** `sudo dnf install gcc-c++ make`
   - **Windows:** Install [Rtools](https://cran.r-project.org/bin/windows/Rtools/)

3. **Set up renv (recommended):**

   ```r
   install.packages("renv")
   renv::restore()
   ```

4. **Or install dependencies manually:**

   ```r
   install.packages(c(
     "devtools", "roxygen2", "testthat", "usethis",
     "DBI", "RSQLite", "dplyr", "tibble", "purrr", "lubridate",
     "readr", "R6", "rlang", "xml2", "Rcpp", "RcppParallel",
     "RcppArmadillo", "jsonlite"
   ))
   ```

5. **Load the package for development:**

   ```r
   devtools::load_all()
   ```

### Development Workflow

We use the standard devtools workflow:

```r
# Load package during development
devtools::load_all()

# Run tests
devtools::test()

# Check package
devtools::check()

# Build documentation
devtools::document()

# Build and install
devtools::install()
```

## Code Style

### R Code

Follow the [tidyverse style guide](https://style.tidyverse.org/):

- Use `snake_case` for function and variable names
- Use `<-` for assignment, not `=`
- Limit lines to 80 characters when reasonable
- Use roxygen2 for documentation with markdown syntax
- Prefer tidyverse functions where appropriate

**Linting:** Consider using `lintr` to check your code:

```r
lintr::lint_package()
```

### C++ Code (Rcpp)

- Use modern C++17 features where beneficial
- Follow consistent naming conventions (snake_case preferred)
- Document exported functions with roxygen2 comments in the R wrapper
- Handle errors gracefully; use `Rcpp::stop()` for fatal errors
- Avoid raw pointers; prefer smart pointers or Rcpp types

Example Rcpp function structure:

```cpp
#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector my_function(NumericVector x) {
    if (x.size() == 0) {
        stop("Input vector cannot be empty");
    }
    // Implementation
    return x;
}
```

### Documentation

- All exported functions must have roxygen2 documentation
- Include `@param` for all parameters
- Include `@return` describing the return value
- Include `@examples` with runnable examples
- Use `@family` to group related functions

## Testing

### Running Tests

```r
# Run all tests
devtools::test()

# Run specific test file
testthat::test_file("tests/testthat/test-connection.R")

# Run tests with coverage
covr::package_coverage()
```

### Writing Tests

- Place tests in `tests/testthat/`
- Name test files `test-*.R`
- Use descriptive test names
- Test both success and failure cases
- Use `withr` for temporary files and state changes

Example test structure:

```r
test_that("function_name handles edge case", {
    # Setup
    test_file <- withr::local_tempfile(fileext = ".gnucash")

    # Exercise
    result <- my_function(test_file)

    # Verify
    expect_equal(result, expected_value)
})
```

### Test Data

- Do not commit real GnuCash files with sensitive financial data
- Use small, anonymized test files
- Consider generating test data programmatically

## Making Changes

### Branch Naming

Use descriptive branch names:

- `feature/add-budget-reports`
- `fix/xml-parsing-error`
- `docs/update-vignette`
- `refactor/consolidation-api`

### Commit Messages

Write clear, concise commit messages:

- Use present tense ("Add feature" not "Added feature")
- First line should be 50 characters or less
- Reference issues when applicable ("Fix #123")

### Pull Request Process

1. **Create a branch** from `main`
2. **Make your changes** following the guidelines above
3. **Run checks:**
   ```r
   devtools::check()
   devtools::test()
   ```
4. **Update documentation** if you changed exported functions
5. **Update NEWS.md** for user-facing changes
6. **Submit a pull request** to the GitLab repository (preferred) or GitHub mirror
7. **Respond to review feedback** promptly

## Rcpp Contribution Guidelines

Contributing to the C++ components requires extra care:

### Memory Safety

- Never return raw pointers from Rcpp functions
- Use RAII patterns for resource management
- Be careful with RcppParallel; avoid R API calls in parallel regions

### Thread Safety (RcppParallel)

```cpp
#include <RcppParallel.h>

struct MyWorker : public RcppParallel::Worker {
    // Input (read-only)
    const RcppParallel::RVector<double> input;

    // Output
    RcppParallel::RVector<double> output;

    // Constructor, operator(), etc.
};
```

### Testing C++ Code

- Write R-level tests that exercise the C++ code
- Test edge cases: empty inputs, large inputs, invalid inputs
- Use `hedgehog` for property-based testing when appropriate

### Compilation

If you modify `src/Makevars`, ensure it works across platforms:

```makefile
PKG_CXXFLAGS = $(SHLIB_OPENMP_CXXFLAGS)
PKG_LIBS = $(SHLIB_OPENMP_CXXFLAGS) $(LAPACK_LIBS) $(BLAS_LIBS) $(FLIBS)
```

## Reporting Issues

### Bug Reports

When reporting bugs, please include:

- R version and OS
- gnucashr version
- Minimal reproducible example
- GnuCash file format (SQLite/XML)
- Full error messages

### Feature Requests

When requesting features, please describe:

- The use case
- Proposed solution
- Alternatives considered

## Getting Help

- **Issues:** [GitLab Issues](https://gitlab.com/tinyland/projects/gnucashr/-/issues)
- **Documentation:** [Package Website](https://tinyland.gitlab.io/projects/gnucashr)

## License

By contributing to gnucashr, you agree that your contributions will be licensed under the MIT License.
