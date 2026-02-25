# gnucashr - Claude Code Configuration

## Project Overview

Monorepo for agentic financial management built on GnuCash.
The gnucashr R package (in `packages/gnucashr/`) provides the core interface
for reading, analyzing, and modeling GnuCash accounting data using Rcpp,
RcppParallel, and RcppArmadillo.

## Monorepo Structure

```
gnucashr/
├── packages/gnucashr/    # R package (CRAN-submittable)
│   ├── R/                # R source files
│   ├── src/              # C++ source (Rcpp)
│   ├── tests/testthat/   # Test files
│   ├── man/              # Documentation
│   ├── vignettes/        # Vignettes
│   └── DESCRIPTION       # Package metadata
├── docs/epic/            # 8-week development epic
├── flake.nix             # Nix flake (root)
├── justfile              # Central command runner
├── BUILD.bazel           # Bazel root module
└── .gitlab-ci.yml        # Primary CI
```

## CI/CD Configuration

- **Primary CI**: GitLab CI (`.gitlab-ci.yml`)
- **Secondary CI**: GitHub Actions (`.github/workflows/`)
- **Coverage**: Codecov (`codecov.yml`)

### CI Monitoring Rules

**IMPORTANT**: CI monitoring tasks MUST run in background mode:

1. When checking CI status, use `run_in_background: true` for Bash commands
2. Use the `/ci-status` skill for quick status checks
3. Use the `/ci-watch` skill for long-running monitoring
4. NEVER block the conversation waiting for CI to complete

## Repository Remotes

- `origin`: GitHub (Jesssullivan/gnucashr)
- `upstream`: GitLab (tinyland/projects/gnucashr)

## Development Commands (Justfile)

```bash
just test          # Run tests
just check         # devtools::check()
just document      # Generate docs
just build         # R CMD build
just cran-check    # Full CRAN check
just nix-build     # Nix flake build
just nix-check     # Nix R CMD check
```

## R Package Development

All R package commands run from `packages/gnucashr/`:

```bash
cd packages/gnucashr
Rscript -e "devtools::test()"
Rscript -e "devtools::check()"
R CMD build . && R CMD check --as-cran gnucashr_*.tar.gz
```

## Key Files

| File | Purpose |
|------|---------|
| `packages/gnucashr/DESCRIPTION` | Package metadata |
| `packages/gnucashr/R/` | R source files |
| `packages/gnucashr/src/` | C++ source (Rcpp) |
| `packages/gnucashr/tests/testthat/` | Test files |
| `flake.nix` | Nix flake definition |
| `justfile` | Central command runner |
| `.gitlab-ci.yml` | GitLab CI |
| `.github/workflows/` | GitHub Actions CI |
| `codecov.yml` | Coverage config |
| `docs/epic/` | Development epic plan |

## Common Issues

### macOS CI Binary Downloads
macOS arm64 binary downloads from CRAN can fail. The workflow uses
`remotes::install_deps(type = "source")` on macOS to work around this.

### GitLab Runner Timeouts
r-cmd-check has a 90-minute timeout. If exceeded, check for cache misses
causing full recompilation.

## Nix Development Environment

### Quick Start
```bash
# Enter development shell (from repo root)
nix develop

# Or with direnv (auto-activates on cd)
direnv allow
```

### Available in Nix Shell
- R with all package dependencies (Imports, LinkingTo, Suggests)
- C++ toolchain with OpenMP and TBB support
- Development tools: devtools, roxygen2, testthat, covr

### Nix Cacheable Derivations
```bash
nix build .#rDeps      # R dependencies
nix build .#cppBuild   # C++ object files
nix build .#tarball    # Source tarball
nix build .#coverage   # Coverage report
nix build .#pkgdown    # Documentation site
```
