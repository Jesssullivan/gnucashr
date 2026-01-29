{
  description = "gnucashr - R package for GnuCash accounting data analysis";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-24.11";
    flake-utils.url = "github:numtide/flake-utils";
  };

  nixConfig = {
    extra-substituters = [ "https://nix-cache.fuzzy-dev.tinyland.dev/main" ];
    extra-trusted-public-keys = [ "main:PBDvqG8OP3W2XF4QzuqWwZD/RhLRsE7ONxwM09kqTtw=" ];
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
          inherit system;
          config.allowUnfree = true;
        };

        # R packages from DESCRIPTION Imports and LinkingTo
        rPackages = with pkgs.rPackages; [
          # Core dependencies (Imports)
          DBI
          RSQLite
          dplyr
          tibble
          purrr
          lubridate
          readr
          R6
          rlang
          xml2
          Rcpp
          RcppParallel
          jsonlite
          # Note: 'tools' is a base R package, included with R itself

          # LinkingTo dependencies
          RcppArmadillo

          # Suggested packages for development/testing
          testthat
          withr
          keyring
          hedgehog
          data_table
          ggplot2
          scales
          future
          furrr
          targets
          tarchetypes
          knitr
          rmarkdown
          gt
          shiny
          renv

          # Development tools
          devtools
          roxygen2
          pkgdown
          lintr
          covr
        ];

        # System libraries needed for compilation
        systemDeps = with pkgs; [
          # C++ toolchain with OpenMP support
          gcc
          gnumake
          pkg-config

          # TBB for RcppParallel
          tbb

          # XML parsing (for xml2)
          libxml2

          # SQLite (for RSQLite)
          sqlite

          # OpenSSL (for various packages)
          openssl

          # curl (for network operations)
          curl

          # zlib (compression)
          zlib

          # For building vignettes
          pandoc
        ];

        rWithPackages = pkgs.rWrapper.override {
          packages = rPackages;
        };

        # Version from DESCRIPTION (updated manually or via CI)
        version = "0.2.0.9000";

      in {
        # ============================================================
        # CACHEABLE SUB-DERIVATIONS
        # Split build into stages that can be cached independently
        # ============================================================

        # Stage 1: R Dependencies Derivation
        # Cached unless DESCRIPTION changes (R package list)
        # This derivation validates R environment and outputs version info
        packages.rDeps = pkgs.runCommand "gnucashr-r-deps-${version}" {
          buildInputs = [ rWithPackages ] ++ systemDeps;
          # Hash based on R packages - changes rarely
        } ''
          mkdir -p $out
          # Record R and package versions for cache validation
          R --version > $out/r-version.txt
          Rscript -e 'installed.packages()[,c("Package","Version")]' > $out/packages.txt
          # Verify key dependencies are available
          Rscript -e '
            pkgs <- c("Rcpp", "RcppParallel", "RcppArmadillo", "DBI", "RSQLite")
            for (pkg in pkgs) {
              if (!requireNamespace(pkg, quietly = TRUE)) {
                stop(paste("Missing required package:", pkg))
              }
            }
            cat("All dependencies verified\n")
          '
          echo "R dependencies cached successfully" > $out/status.txt
        '';

        # Stage 2: C++ Object Files Derivation
        # Cached unless src/*.cpp files change
        # Compiles individual .o files that can be reused
        packages.cppBuild = pkgs.stdenv.mkDerivation {
          name = "gnucashr-cpp-${version}";
          src = ./src;

          buildInputs = [ rWithPackages pkgs.tbb ];
          nativeBuildInputs = [ pkgs.gcc ];

          # TBB include/lib paths
          TBB_INCLUDE = "${pkgs.tbb.dev}/include";
          TBB_LIB = "${pkgs.tbb}/lib";

          buildPhase = ''
            echo "Compiling C++ source files..."

            # Get R and package include paths using R itself
            R_CPPFLAGS=$(R CMD config --cppflags)
            R_INCLUDE_DIR=$(Rscript -e 'cat(R.home("include"))')
            RCPP_INCLUDE=$(Rscript -e 'cat(system.file("include", package="Rcpp"))')
            RCPP_ARMA_INCLUDE=$(Rscript -e 'cat(system.file("include", package="RcppArmadillo"))')
            RCPP_PARALLEL_INCLUDE=$(Rscript -e 'cat(system.file("include", package="RcppParallel"))')

            echo "R CPPFLAGS: $R_CPPFLAGS"
            echo "R include dir: $R_INCLUDE_DIR"
            echo "Rcpp include: $RCPP_INCLUDE"
            echo "RcppArmadillo include: $RCPP_ARMA_INCLUDE"
            echo "RcppParallel include: $RCPP_PARALLEL_INCLUDE"

            # Compile each .cpp file to .o
            for f in *.cpp; do
              if [ -f "$f" ]; then
                echo "  Compiling $f..."
                $CXX -c "$f" -o "''${f%.cpp}.o" \
                  -I"$R_INCLUDE_DIR" \
                  -I"$RCPP_INCLUDE" \
                  -I"$RCPP_ARMA_INCLUDE" \
                  -I"$RCPP_PARALLEL_INCLUDE" \
                  -I"$TBB_INCLUDE" \
                  -std=c++17 -fopenmp -fPIC -DNDEBUG -O2
              fi
            done

            echo "C++ compilation complete"
            ls -la *.o 2>/dev/null || echo "No object files generated"
          '';

          installPhase = ''
            mkdir -p $out
            if ls *.o 1>/dev/null 2>&1; then
              cp *.o $out/
              echo "Object files installed:"
              ls -la $out/*.o
            else
              echo "Warning: No object files to install"
              exit 1
            fi
            # Also copy headers if any exist
            for h in *.h; do
              if [ -f "$h" ]; then
                cp "$h" $out/
              fi
            done
          '';
        };

        # Stage 3: Source Tarball (uses cached components)
        # Rebuilds quickly using pre-compiled C++ objects
        packages.tarball = pkgs.runCommand "gnucashr-${version}.tar.gz" {
          buildInputs = [ rWithPackages ] ++ systemDeps;
          src = self;
          rDeps = self.packages.${system}.rDeps;
          cppObjects = self.packages.${system}.cppBuild;
          # Disable renv auto-activation (we use Nix for dependencies)
          RENV_ACTIVATE_PROJECT = "FALSE";
        } ''
          # Copy source tree
          cp -r $src source
          chmod -R u+w source
          cd source

          # Remove renv activation to avoid bootstrap attempts
          rm -f .Rprofile

          # Copy pre-compiled C++ objects if available
          if [ -d "$cppObjects" ] && ls $cppObjects/*.o 1>/dev/null 2>&1; then
            echo "Using cached C++ object files..."
            cp $cppObjects/*.o src/ 2>/dev/null || true
            ls -la src/*.o
          fi

          # Generate documentation
          Rscript -e "roxygen2::roxygenise()"

          # Build source tarball
          R CMD build . --no-manual --no-build-vignettes
          cp gnucashr_*.tar.gz $out
        '';

        packages.default = self.packages.${system}.tarball;

        devShells.default = pkgs.mkShell {
          buildInputs = [ rWithPackages ] ++ systemDeps;

          shellHook = ''
            echo "gnucashr development environment"
            echo "R version: $(R --version | head -1)"
            echo ""
            echo "Available commands:"
            echo "  R                        - Start R console"
            echo "  Rscript -e 'devtools::test()'     - Run tests"
            echo "  Rscript -e 'devtools::check()'    - Run R CMD check"
            echo "  Rscript -e 'devtools::document()' - Generate documentation"
            echo ""
            echo "Cacheable derivations:"
            echo "  nix build .#rDeps      - Build R dependencies (cached)"
            echo "  nix build .#cppBuild   - Build C++ objects (cached)"
            echo "  nix build .#tarball    - Build full package"
            echo ""
          '';

          # Environment variables for C++ compilation
          NIX_CFLAGS_COMPILE = "-I${pkgs.tbb.dev}/include";
          NIX_LDFLAGS = "-L${pkgs.tbb}/lib -ltbb";

          # Ensure OpenMP is available
          CXXFLAGS = "-fopenmp";
          LDFLAGS = "-fopenmp";
        };

        # R CMD check (full validation) - uses cached components
        checks.r-cmd-check = pkgs.runCommand "gnucashr-r-cmd-check" {
          buildInputs = [ rWithPackages ] ++ systemDeps;
          src = self;
          rDeps = self.packages.${system}.rDeps;
          cppObjects = self.packages.${system}.cppBuild;
          NIX_CFLAGS_COMPILE = "-I${pkgs.tbb.dev}/include";
          NIX_LDFLAGS = "-L${pkgs.tbb}/lib -ltbb";
          # Disable renv auto-activation
          RENV_ACTIVATE_PROJECT = "FALSE";
        } ''
          cp -r $src source
          chmod -R u+w source
          cd source

          # Remove renv activation to avoid bootstrap attempts
          rm -f .Rprofile

          # Copy pre-compiled C++ objects if available
          if [ -d "$cppObjects" ] && ls $cppObjects/*.o 1>/dev/null 2>&1; then
            echo "Using cached C++ object files..."
            cp $cppObjects/*.o src/ 2>/dev/null || true
          fi

          # Run R CMD check
          R CMD build . --no-manual --no-build-vignettes
          R CMD check gnucashr_*.tar.gz --no-manual --no-vignettes --no-examples
          mkdir -p $out
          cp -r gnucashr.Rcheck $out/
        '';

        checks.default = self.checks.${system}.r-cmd-check;
      }
    );
}
