# gnucashr - Claude Code Configuration

## Project Overview

R package for reading, analyzing, and modeling GnuCash accounting data.
Uses Rcpp, RcppParallel, and RcppArmadillo for performance-critical operations.

## CI/CD Configuration

- **Primary CI**: GitLab CI (`.gitlab-ci.yml`)
- **Secondary CI**: GitHub Actions (`.github/workflows/R-CMD-check.yaml`)
- **Coverage**: Codecov (`codecov.yml`)

### CI Monitoring Rules

**IMPORTANT**: CI monitoring tasks MUST run in background mode:

1. When checking CI status, use `run_in_background: true` for Bash commands
2. Use the `/ci-status` skill for quick status checks
3. Use the `/ci-watch` skill for long-running monitoring
4. NEVER block the conversation waiting for CI to complete

Example pattern for CI checks:
```
# Quick status check - use skill
/ci-status

# Long monitoring - always background
Bash with run_in_background: true
```

## Repository Remotes

- `origin`: GitHub (Jesssullivan/gnucashr)
- `upstream`: GitLab (tinyland/projects/gnucashr)

## R Package Development

### Running Tests
```bash
Rscript -e "devtools::test()"
```

### R CMD check
```bash
R CMD build . && R CMD check --as-cran gnucashr_*.tar.gz
```

### Documentation
```bash
Rscript -e "devtools::document()"
```

## Key Files

| File | Purpose |
|------|---------|
| `DESCRIPTION` | Package metadata |
| `R/` | R source files |
| `src/` | C++ source (Rcpp) |
| `tests/testthat/` | Test files |
| `.github/workflows/R-CMD-check.yaml` | GitHub Actions CI |
| `.gitlab-ci.yml` | GitLab CI |
| `codecov.yml` | Coverage config |

## Common Issues

### macOS CI Binary Downloads
macOS arm64 binary downloads from CRAN can fail. The workflow uses
`remotes::install_deps(type = "source")` on macOS to work around this.

### GitLab Runner Timeouts
r-cmd-check has a 90-minute timeout. If exceeded, check for cache misses
causing full recompilation.

## Nix Development Environment

### Prerequisites
- Nix with flakes enabled
- direnv (optional, for automatic environment activation)

### Quick Start
```bash
# Enter development shell
nix develop

# Or with direnv (auto-activates on cd)
direnv allow
```

### Available in Nix Shell
- R with all package dependencies (Imports, LinkingTo, Suggests)
- C++ toolchain with OpenMP and TBB support
- Development tools: devtools, roxygen2, testthat, covr

### Nix Commands
```bash
# Validate flake
nix flake check

# Enter shell
nix develop

# Show flake info
nix flake show
```

### Files
| File | Purpose |
|------|---------|
| `flake.nix` | Nix flake definition |
| `flake.lock` | Pinned dependency versions |
| `.envrc` | direnv integration |
