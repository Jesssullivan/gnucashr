# Contributing to gnucashr

Thank you for your interest in contributing to gnucashr! This document provides guidelines and information for contributors.

## Table of Contents

- [Code of Conduct](#code-of-conduct)
- [Getting Started](#getting-started)
- [Development Workflow](#development-workflow)
- [Code Style Guidelines](#code-style-guidelines)
- [Testing Requirements](#testing-requirements)
- [Commit Message Format](#commit-message-format)
- [Pull Request Process](#pull-request-process)

## Code of Conduct

This project follows a standard code of conduct. Be respectful, inclusive, and constructive in all interactions.

## Getting Started

### Prerequisites

- R >= 4.1.0
- C++17 compatible compiler (gcc >= 7, clang >= 5)
- devtools package
- testthat package (>= 3.0.0)

### Fork and Clone

1. Fork the repository on GitHub
2. Clone your fork locally:
   ```bash
   git clone https://github.com/YOUR_USERNAME/gnucashr.git
   cd gnucashr
   ```
3. Add the upstream remote:
   ```bash
   git remote add upstream https://github.com/Jesssullivan/gnucashr.git
   ```

### Install Development Dependencies

This project uses [renv](https://rstudio.github.io/renv/) for reproducible package
management. When you open the project, renv will automatically activate and prompt
you to restore the development environment:

```r
# On first clone, restore the development environment
renv::restore()
```

This installs all dependencies at the exact versions specified in `renv.lock`,
ensuring consistent development environments across all contributors.

**Manual installation (alternative):**

If you prefer not to use renv, you can install dependencies manually:

```r
install.packages(c("devtools", "testthat", "roxygen2", "lintr", "covr"))
devtools::install_deps(dependencies = TRUE)
```

**Updating dependencies:**

When adding new dependencies or updating package versions:

```r
# After installing new packages, update the lockfile
renv::snapshot()
```

### Build and Check

```r
devtools::document()  # Generate documentation
devtools::build()     # Build package
devtools::check()     # Run R CMD check
```

## Development Workflow

### Branch Naming Convention

- `feature/description` - New features
- `fix/description` - Bug fixes
- `docs/description` - Documentation updates
- `refactor/description` - Code refactoring
- `test/description` - Test additions or fixes

### Workflow Steps

1. **Sync with upstream** before starting work:
   ```bash
   git fetch upstream
   git checkout main
   git merge upstream/main
   ```

2. **Create a feature branch**:
   ```bash
   git checkout -b feature/your-feature-name
   ```

3. **Make changes** and commit frequently with clear messages

4. **Run tests** before pushing:
   ```r
   devtools::test()
   ```

5. **Push your branch**:
   ```bash
   git push origin feature/your-feature-name
   ```

6. **Open a Pull Request** against the `main` branch

## Code Style Guidelines

### R Code Style

We follow the [tidyverse style guide](https://style.tidyverse.org/) with these specifics:

- **Indentation**: 2 spaces (no tabs)
- **Line length**: Maximum 100 characters
- **Assignment**: Use `<-` not `=`
- **Spacing**: Space after commas, around operators
- **Naming**:
  - Functions: `snake_case` (e.g., `account_balance()`)
  - R6 classes: `PascalCase` (e.g., `GnuCashDB`)
  - Constants: `SCREAMING_SNAKE_CASE`
  - Internal functions: prefix with `.` (e.g., `.parse_xml_node()`)

### Documentation

- All exported functions must have roxygen2 documentation
- Include `@param`, `@return`, `@export`, and `@examples`
- Document R6 classes with `@description` and `@field`

```r
#' Calculate account balance
#'
#' @param gc A GnuCashDB object
#' @param account_path Character string specifying the account path
#' @param as_of Date for balance calculation (default: today)
#'
#' @return Numeric balance value
#' @export
#'
#' @examples
#' gc <- read_gnucash("books.gnucash")
#' account_balance(gc, "Assets:Bank:Checking")
account_balance <- function(gc, account_path, as_of = Sys.Date()) {
  # implementation
}
```

### C++ Code Style

- Follow [Google C++ Style Guide](https://google.github.io/styleguide/cppguide.html)
- Use `// [[Rcpp::export]]` for functions exposed to R
- Include proper Rcpp headers
- Use `Rcpp::stop()` for error handling, not exceptions

### Linting

Run the linter before submitting:

```r
lintr::lint_package()
```

Fix all linting errors before submitting a PR.

## Testing Requirements

### Test Coverage

- All new functions must have tests
- Aim for > 80% code coverage
- Test edge cases and error conditions

### Running Tests

```r
# Run all tests
devtools::test()

# Run specific test file
devtools::test(filter = "connection")

# Check coverage
covr::package_coverage()
covr::report()
```

### Test Structure

Tests are located in `tests/testthat/`. Follow this structure:

```r
test_that("function does expected behavior", {
  # Setup
  gc <- read_gnucash(test_fixture("sample.gnucash"))
  on.exit(close(gc))

  # Exercise
  result <- account_balance(gc, "Assets:Bank")

  # Verify

  expect_equal(result, 1000.00)
})
```

### Test Fixtures

- Place test data in `tests/testthat/fixtures/`
- Use small, anonymized GnuCash files
- Never include real financial data

## Commit Message Format

We follow [Conventional Commits](https://www.conventionalcommits.org/):

```
<type>(<scope>): <description>

[optional body]

[optional footer]
```

### Types

- `feat`: New feature
- `fix`: Bug fix
- `docs`: Documentation only
- `style`: Code style (formatting, no logic change)
- `refactor`: Code refactoring
- `test`: Adding or fixing tests
- `chore`: Maintenance tasks

### Examples

```
feat(forecast): add Monte Carlo parallel execution

Implement parallel scenarios using Rcpp for 10x speedup
on multi-core systems.

Closes #42
```

```
fix(xml): handle compressed GnuCash files

XML parser now detects and decompresses .gnucash files
that use gzip compression.

Fixes #37
```

## Pull Request Process

### Before Submitting

1. **Rebase on main** to ensure clean history:
   ```bash
   git fetch upstream
   git rebase upstream/main
   ```

2. **Run full check**:
   ```r
   devtools::check()
   ```

3. **Update documentation** if needed:
   ```r
   devtools::document()
   ```

4. **Update NEWS.md** for user-facing changes

### PR Requirements

- Clear description of changes
- Reference related issues
- All tests passing
- No linting errors
- Documentation updated
- DESCRIPTION version bumped (for releases)

### Review Process

1. Maintainers will review within 1 week
2. Address feedback with additional commits
3. Squash commits if requested
4. Maintainer merges after approval

## Questions?

- Open an issue for bugs or feature requests
- Use discussions for questions
- Tag maintainers for urgent issues

Thank you for contributing to gnucashr!
