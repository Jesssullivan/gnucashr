# gnucashr monorepo - central command runner
# Replaces Makefile as the primary developer interface
#
# Usage: just --list

set dotenv-load
set positional-arguments

# ============================================================
# R Package (packages/gnucashr)
# ============================================================

# Run R CMD check on the package
check:
    cd packages/gnucashr && Rscript -e "devtools::check()"

# Run R package tests
test:
    cd packages/gnucashr && Rscript -e "devtools::test()"

# Generate roxygen2 documentation
document:
    cd packages/gnucashr && Rscript -e "devtools::document()"

# Build R source tarball
build:
    cd packages/gnucashr && R CMD build .

# Run R CMD check --as-cran
cran-check: document
    cd packages/gnucashr && R CMD build . && R CMD check --as-cran gnucashr_*.tar.gz

# Install package locally
install:
    cd packages/gnucashr && R CMD INSTALL .

# Run code coverage
coverage:
    cd packages/gnucashr && Rscript -e 'covr::package_coverage(type = "tests")'

# Lint R code
lint:
    cd packages/gnucashr && Rscript -e "lintr::lint_package()"

# ============================================================
# Nix
# ============================================================

# Enter Nix development shell
shell:
    nix develop

# Build R package tarball via Nix
nix-build:
    nix build .#tarball

# Run R CMD check via Nix
nix-check:
    nix build .#checks.x86_64-linux.r-cmd-check

# Build all Nix derivations
nix-all:
    nix build .#rDeps .#cppBuild .#tarball --max-jobs 4

# ============================================================
# C++ Library (lib/gnucash-core)
# ============================================================

# Configure C++ library build
cpp-configure:
    cd lib/gnucash-core && mkdir -p build && cd build && cmake .. -DCMAKE_BUILD_TYPE=Debug

# Build C++ library
cpp-build: cpp-configure
    cd lib/gnucash-core/build && cmake --build . -j$(nproc)

# Run C++ library tests
cpp-test: cpp-build
    cd lib/gnucash-core/build && ctest --output-on-failure

# Clean C++ build artifacts
cpp-clean:
    rm -rf lib/gnucash-core/build

# Build C++ library via Nix (hermetic)
cpp-nix-build:
    nix build .#gnucashCore

# Run C++ tests via Nix (hermetic)
cpp-nix-test:
    nix build .#gnucashCoreTests

# Build JSON bridge via Nix (hermetic)
cpp-nix-bridge:
    nix build .#gnucashBridge

# Run JSON bridge interactively (stdin/stdout)
bridge book:
    cd lib/gnucash-core/build && ./gnucash-bridge <<< '{"method":"open","params":{"path":"{{book}}"},"id":1}'

# ============================================================
# Dhall
# ============================================================

# Type-check all Dhall configuration
dhall-check:
    dhall type --file dhall/package.dhall --quiet
    @echo "All Dhall types OK"

# Export authorization rules to JSON
dhall-export-rules:
    dhall-to-json --file dhall/rules/authorization.dhall

# Export all templates to JSON
dhall-export-templates:
    @echo "=== SaaS Spend ===" && dhall-to-json --file dhall/templates/saas-spend.dhall
    @echo "=== Tax Categories ===" && dhall-to-json --file dhall/templates/tax-categories.dhall
    @echo "=== Income Categories ===" && dhall-to-json --file dhall/templates/income-categories.dhall

# Lint all Dhall files
dhall-lint:
    find dhall -name "*.dhall" -exec dhall lint --inplace {} \;

# ============================================================
# Agents (Week 5+)
# ============================================================

# Run an agent against a GnuCash book
agent-run agent book:
    @echo "Agent framework not yet built (Week 5)"

# ============================================================
# Development Utilities
# ============================================================

# Show project status
status:
    @echo "=== gnucashr monorepo ==="
    @echo ""
    @echo "R Package:"
    @grep "^Version:" packages/gnucashr/DESCRIPTION
    @echo ""
    @echo "Git:"
    @git log --oneline -5
    @echo ""
    @echo "Nix:"
    @ls -la result-* 2>/dev/null || echo "  (no result symlinks)"

# Clean build artifacts
clean:
    rm -f packages/gnucashr/gnucashr_*.tar.gz
    rm -rf packages/gnucashr/gnucashr.Rcheck
    rm -rf packages/gnucashr/src/*.o packages/gnucashr/src/*.so packages/gnucashr/src/*.dll
    rm -rf lib/gnucash-core/build
    rm -f result result-*
