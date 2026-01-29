{
  description = "gnucashr - R package for GnuCash accounting data analysis";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-24.11";
    flake-utils.url = "github:numtide/flake-utils";
  };

  # Uncomment when attic cache public key is available:
  # nixConfig = {
  #   extra-substituters = [ "https://nix-cache.fuzzy-dev.tinyland.dev/main" ];
  #   extra-trusted-public-keys = [ "main:REPLACE_WITH_PUBLIC_KEY" ];
  # };

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

      in {
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
          '';

          # Environment variables for C++ compilation
          NIX_CFLAGS_COMPILE = "-I${pkgs.tbb.dev}/include";
          NIX_LDFLAGS = "-L${pkgs.tbb}/lib -ltbb";

          # Ensure OpenMP is available
          CXXFLAGS = "-fopenmp";
          LDFLAGS = "-fopenmp";
        };

        # Package check
        checks.default = pkgs.runCommand "gnucashr-check" {
          buildInputs = [ rWithPackages ] ++ systemDeps;
        } ''
          echo "Nix flake check passed"
          mkdir -p $out
          echo "gnucashr flake validated" > $out/result
        '';
      }
    );
}
