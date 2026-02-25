# Phase 1: Foundation (Weeks 1-2)

**Goal**: Restructure into monorepo with Justfile, Dhall bootstrap, R package still passes
all checks from new location.

---

## Week 1: Monorepo Restructure

### Tasks

- [ ] **1.1 Move R package into `packages/gnucashr/`**
  - Move R/, src/, tests/, man/, vignettes/, inst/, DESCRIPTION, NAMESPACE, .Rbuildignore
  - Update all relative paths in CI workflows
  - Verify `R CMD build packages/gnucashr && R CMD check --as-cran` passes
  - **Success**: Zero new NOTEs/WARNINGs compared to current baseline

- [ ] **1.2 Create root Justfile**
  - Replace Makefile with Justfile as central command runner
  - Module system: `mod gnucashr 'packages/gnucashr/justfile'`
  - Core recipes:
    ```just
    # Root justfile
    set dotenv-load

    mod gnucashr 'packages/gnucashr'
    mod agents 'agents'
    mod lib 'lib'

    # Top-level commands
    check: (gnucashr::check) (lib::check)
    test: (gnucashr::test) (lib::test)
    build: (gnucashr::build) (lib::build)
    fmt: (gnucashr::fmt) (lib::fmt)

    # Nix operations
    nix-build:
      nix build .#gnucashr-tarball

    nix-check:
      nix build .#r-cmd-check

    nix-shell:
      nix develop

    # Dhall operations
    dhall-check:
      dhall type --file dhall/package.dhall
      dhall lint --inplace dhall/*.dhall
    ```
  - **Success**: `just --list` shows all recipes; `just check` runs R CMD check

- [ ] **1.3 Update flake.nix for monorepo**
  - Source paths point to `packages/gnucashr/` for R package derivations
  - Add Dhall and Just to devShell
  - Add C++ standalone lib build target (stub)
  - Keep Attic cache integration working
  - **Success**: `nix build .#gnucashr-tarball` produces valid tarball

- [ ] **1.4 Update CI for monorepo layout**
  - GitLab CI: Update paths in all jobs
  - GitHub Actions: Update working-directory for R CMD check
  - Add path filters to avoid rebuilding everything on docs-only changes
  - **Success**: CI green on both platforms after restructure

- [ ] **1.5 Update .Rbuildignore and .gitignore**
  - .Rbuildignore excludes monorepo-level files (justfile, agents/, lib/, docs/epic/)
  - .gitignore covers Dhall cache, Bazel outputs, agent state
  - **Success**: `R CMD build packages/gnucashr` produces clean tarball

### User Interaction Point
> After 1.1-1.5: Review the restructured repo. Verify your GnuCash books still load.
> Run `just check` and confirm all tests pass. This is the foundation everything builds on.

---

## Week 2: Dhall Bootstrap + Justfile Polish

### Tasks

- [ ] **2.1 Bootstrap Dhall configuration layer**
  - Create `dhall/` directory at monorepo root
  - Define core types:
    ```dhall
    -- dhall/types/Account.dhall
    let AccountType = < Asset | Liability | Income | Expense | Equity >

    let AccountMapping =
      { gnucash_path : Text
      , category : Text
      , tags : List Text
      , auto_categorize : Bool
      }

    -- dhall/types/AgentRule.dhall
    let AuthorizationLevel = < Auto | Review | Approve >

    let AgentRule =
      { name : Text
      , description : Text
      , authorization : AuthorizationLevel
      , max_amount : Optional Natural  -- cents
      , rate_limit : Natural            -- per hour
      , enabled : Bool
      }
    ```
  - **Success**: `dhall type --file dhall/package.dhall` passes

- [ ] **2.2 Define agent authorization rules in Dhall**
  - Map financial operations to authorization tiers:
    - **Auto**: Read balances, generate reports, categorize transactions, calculate taxes
    - **Review**: Create invoices, flag subscriptions for cancellation, modify budgets
    - **Approve**: Pay bills, cancel subscriptions, transfer funds, modify accounts
  - Export as JSON for agent consumption: `dhall-to-json --file dhall/rules.dhall`
  - **Success**: `just dhall-check` validates all Dhall configs

- [ ] **2.3 Define account mapping templates in Dhall**
  - SaaS spend categories (compute, storage, AI/ML, dev tools, productivity)
  - Tax categories (business expense, home office, travel, equipment)
  - Income categories (consulting, subscriptions, product sales)
  - **Success**: Templates compile to JSON matching gnucashr's existing template format

- [ ] **2.4 Justfile package-level recipes**
  - `packages/gnucashr/justfile`:
    ```just
    # R package recipes
    check:
      Rscript -e "devtools::check()"

    test:
      Rscript -e "devtools::test()"

    document:
      Rscript -e "devtools::document()"

    build:
      R CMD build .

    coverage:
      Rscript -e "covr::package_coverage()"

    lint:
      Rscript -e "lintr::lint_package()"

    fmt:
      Rscript -e "styler::style_pkg()"
    ```
  - **Success**: All package development workflows accessible via `just gnucashr::*`

- [ ] **2.5 Bazel MODULE.bazel update for monorepo**
  - Move C++ targets under `lib/gnucash-core/BUILD.bazel`
  - Keep `packages/gnucashr/src/BUILD.bazel` for Rcpp-specific builds
  - Shared C++ code compiles as standalone library AND as Rcpp module
  - **Success**: `bazel build //lib/gnucash-core:all` compiles

- [ ] **2.6 Documentation: CONTRIBUTING.md update**
  - Update development workflow to use `just` commands
  - Document monorepo structure
  - Document Dhall configuration patterns
  - **Success**: New contributor can follow guide to build and test

### User Interaction Point
> After Week 2: Run `just --list` to see all available commands. Try `just dhall-check`.
> Review the Dhall types in `dhall/types/` -- do the authorization tiers make sense for
> your financial operations? Adjust thresholds before we build agents against them.

---

## Go/No-Go Gate G1

| Criterion | How to Verify | Required? |
|-----------|--------------|-----------|
| R CMD check passes from packages/gnucashr/ | `just gnucashr::check` | Yes |
| Nix build produces tarball | `nix build .#gnucashr-tarball` | Yes |
| Justfile lists all recipes | `just --list` | Yes |
| Dhall types compile | `just dhall-check` | Yes |
| CI green on both platforms | Check GitLab + GitHub | Yes |
| Existing tests pass (21 files, 4692 lines) | `just gnucashr::test` | Yes |

**Decision**: If all criteria pass, proceed to Phase 2. If R CMD check regresses,
fix before proceeding -- the R package must remain CRAN-submittable.
