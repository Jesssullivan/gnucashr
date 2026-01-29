# Changelog

## gnucashr 0.2.0.9000 (development)

*Development version - unreleased*

### Changes

- Increase CI timeouts for test (30m → 60m) and pages (20m → 60m) jobs

------------------------------------------------------------------------

## gnucashr 0.2.0

### New Features

#### Data Import

- Add OFX/QFX bank statement import with Rcpp-accelerated parsing
- Add PayPal CSV import with automatic format detection and fee
  extraction
- Add Stripe CSV import with cents-to-dollars conversion
- Add QuickBooks CSV import supporting QBO and Desktop formats
- Add
  [`combine_csv_imports()`](https://tinyland.gitlab.io/projects/gnucashr/reference/combine_csv_imports.md)
  for merging multiple import sources
- Add
  [`validate_ofx_import()`](https://tinyland.gitlab.io/projects/gnucashr/reference/validate_ofx_import.md)
  for duplicate detection
- Add
  [`preview_csv_mapping()`](https://tinyland.gitlab.io/projects/gnucashr/reference/preview_csv_mapping.md)
  debug helper

#### CI/CD Infrastructure

- Add GitLab CI pipeline (primary) with R CMD check, coverage, pkgdown
  Pages
- Add GitHub Actions (secondary) with multi-platform matrix
- Add codecov integration with 80% coverage target

#### Testing Framework

- Add property-based testing with hedgehog for fraction arithmetic
- Add PBT tests for GUID generation, Monte Carlo reproducibility
- Add test fixtures for SQLite databases
- Add KeePassXC-style credential providers for integration tests

#### Documentation

- Add pkgdown site with dark mode support
- Add `importing-data` vignette for CSV/OFX workflows
- Add `performance-benchmarks` technical article
- Add `rcpp-internals` contributor documentation
- Add CONTRIBUTING.md, SECURITY.md, REPOSITORY_SETUP.md

### Bug Fixes

- Fix
  [`account_transactions()`](https://tinyland.gitlab.io/projects/gnucashr/reference/account_transactions.md)
  parameter documentation

------------------------------------------------------------------------

## gnucashr 0.3.0 (planned)

### Planned Features

#### Performance Improvements

- Add data.table backend option for large books
- Improve memory efficiency for multi-book consolidation
- Add incremental loading for large transaction histories

#### Reporting Enhancements

- Add comparative balance sheets (period-over-period)
- Add cash flow statement generation
- Add budget vs actual variance reports

#### Integration

- Add targets pipeline integration vignette
- Add Plumber API template for serving reports
- Add Shiny module library for dashboard components

------------------------------------------------------------------------

## gnucashr 0.2.0

*Released: 2026-01-15* *Initial public release*

### Core Features

#### GnuCash File Support

- Read GnuCash SQLite files via
  [`read_gnucash()`](https://tinyland.gitlab.io/projects/gnucashr/reference/read_gnucash.md)
- Read GnuCash XML files with automatic format detection
- Create new GnuCash databases with
  [`create_gnucash()`](https://tinyland.gitlab.io/projects/gnucashr/reference/create_gnucash.md)
- `GnuCashDB` R6 class for database connection management

#### Account Management

- Account tree navigation with full path support
- Path-based account lookup (e.g., “Assets:Bank:Checking”)
- GUID-based account access
- Account type classification (ASSET, LIABILITY, EQUITY, INCOME,
  EXPENSE)

#### Financial Statements

- Trial balance generation via
  [`trial_balance()`](https://tinyland.gitlab.io/projects/gnucashr/reference/trial_balance.md)
- Balance sheet via
  [`balance_sheet()`](https://tinyland.gitlab.io/projects/gnucashr/reference/balance_sheet.md)
  with standard formatting
- Income statement via
  [`income_statement()`](https://tinyland.gitlab.io/projects/gnucashr/reference/income_statement.md)
  with period selection
- `gt` table output for publication-quality reports

#### Transaction Queries

- [`account_transactions()`](https://tinyland.gitlab.io/projects/gnucashr/reference/account_transactions.md)
  for filtered transaction retrieval
- Date range filtering with lubridate integration
- Running balance calculation (Rcpp-accelerated)
- Split-level detail access

#### Commodities and Prices

- `Commodity` R6 class for currency and security tracking
- `PriceDB` R6 class for historical price management
- Currency conversion support
- ISO 4217 currency namespace handling

#### Budgets and Schedules

- `Budget` R6 class for budget management
- `ScheduledTransaction` for recurring transaction templates
- Monthly, quarterly, and annual budget views
- Budget vs actual comparison

#### Lot Tracking

- `Lot` R6 class for investment lot tracking
- `LotManager` for portfolio cost basis
- FIFO/LIFO lot matching support
- Realized gain/loss calculation

### Multi-Entity Features

#### Book Collection

- `BookCollection` R6 class for multi-book management
- Named book access with automatic connection pooling
- Parallel loading support via future/furrr

#### Consolidation

- [`build_consolidated_trial_balance()`](https://tinyland.gitlab.io/projects/gnucashr/reference/build_consolidated_trial_balance.md)
  for combined statements
- Intercompany elimination rules
- [`apply_ic_eliminations()`](https://tinyland.gitlab.io/projects/gnucashr/reference/apply_ic_eliminations.md)
  for automatic IC removal
- Consolidation validation and balance checking
- Entity-level and consolidated views

### Forecasting Features

#### Lazy Evaluation

- `LazyForecast` R6 class for deferred computation
- Chainable operations:
  [`lf_grow()`](https://tinyland.gitlab.io/projects/gnucashr/reference/lf_grow.md),
  [`lf_filter()`](https://tinyland.gitlab.io/projects/gnucashr/reference/lf_filter.md),
  [`lf_scenario()`](https://tinyland.gitlab.io/projects/gnucashr/reference/lf_scenario.md)
- Collect only when needed via
  [`lf_collect()`](https://tinyland.gitlab.io/projects/gnucashr/reference/lf_collect.md)

#### Monte Carlo Simulation

- [`monte_carlo_parallel()`](https://tinyland.gitlab.io/projects/gnucashr/reference/monte_carlo_parallel.md)
  Rcpp-accelerated simulation
- Multi-entity Monte Carlo via
  [`monte_carlo_multi_entity()`](https://tinyland.gitlab.io/projects/gnucashr/reference/monte_carlo_multi_entity.md)
- Configurable parameters: growth rates, expense ratios, periods
- Reproducible results with seed control

#### Sensitivity Analysis

- [`parallel_sensitivity_grid()`](https://tinyland.gitlab.io/projects/gnucashr/reference/parallel_sensitivity_grid.md)
  for parameter sweeps
- [`quick_sensitivity()`](https://tinyland.gitlab.io/projects/gnucashr/reference/quick_sensitivity.md)
  for rapid what-if analysis
- Visualization via
  [`plot_sensitivity()`](https://tinyland.gitlab.io/projects/gnucashr/reference/plot_sensitivity.md)

#### Scenario Comparison

- [`compare_forecasts()`](https://tinyland.gitlab.io/projects/gnucashr/reference/compare_forecasts.md)
  for side-by-side scenarios
- [`parallel_project_scenarios()`](https://tinyland.gitlab.io/projects/gnucashr/reference/parallel_project_scenarios.md)
  for batch projections
- Growth trajectory visualization

### Write Operations

#### Account Creation

- [`create_account()`](https://tinyland.gitlab.io/projects/gnucashr/reference/create_account.md)
  with type validation
- [`create_account_path()`](https://tinyland.gitlab.io/projects/gnucashr/reference/create_account_path.md)
  for nested account creation
- [`update_account()`](https://tinyland.gitlab.io/projects/gnucashr/reference/update_account.md)
  for account modification
- [`delete_account()`](https://tinyland.gitlab.io/projects/gnucashr/reference/delete_account.md)
  with child handling

#### Transaction Posting

- [`post_transaction()`](https://tinyland.gitlab.io/projects/gnucashr/reference/post_transaction.md)
  for new transactions
- [`post_transfer()`](https://tinyland.gitlab.io/projects/gnucashr/reference/post_transfer.md)
  convenience wrapper
- [`batch_post_transactions()`](https://tinyland.gitlab.io/projects/gnucashr/reference/batch_post_transactions.md)
  for bulk operations
- [`void_transaction()`](https://tinyland.gitlab.io/projects/gnucashr/reference/void_transaction.md)
  and
  [`delete_transaction()`](https://tinyland.gitlab.io/projects/gnucashr/reference/delete_transaction.md)

#### Backup System

- [`create_backup()`](https://tinyland.gitlab.io/projects/gnucashr/reference/create_backup.md)
  before write operations
- [`with_backup()`](https://tinyland.gitlab.io/projects/gnucashr/reference/with_backup.md)
  wrapper for safe writes
- [`list_backups()`](https://tinyland.gitlab.io/projects/gnucashr/reference/list_backups.md)
  and
  [`restore_backup()`](https://tinyland.gitlab.io/projects/gnucashr/reference/restore_backup.md)
- Automatic backup cleanup via
  [`clean_old_backups()`](https://tinyland.gitlab.io/projects/gnucashr/reference/clean_old_backups.md)

### Error Handling

#### Result Monad

- [`ok()`](https://tinyland.gitlab.io/projects/gnucashr/reference/ok.md)
  and
  [`err()`](https://tinyland.gitlab.io/projects/gnucashr/reference/err.md)
  constructors
- [`is_ok()`](https://tinyland.gitlab.io/projects/gnucashr/reference/is_ok.md),
  [`is_err()`](https://tinyland.gitlab.io/projects/gnucashr/reference/is_err.md)
  predicates
- [`unwrap()`](https://tinyland.gitlab.io/projects/gnucashr/reference/unwrap.md)
  with optional default
- [`result_map()`](https://tinyland.gitlab.io/projects/gnucashr/reference/result_map.md),
  [`result_bind()`](https://tinyland.gitlab.io/projects/gnucashr/reference/result_bind.md)
  for chaining
- [`result_match()`](https://tinyland.gitlab.io/projects/gnucashr/reference/result_match.md)
  for pattern matching
- [`try_result()`](https://tinyland.gitlab.io/projects/gnucashr/reference/try_result.md)
  for exception capture

#### Logged Monad

- [`logged()`](https://tinyland.gitlab.io/projects/gnucashr/reference/logged.md)
  for audit trails
- [`log_append()`](https://tinyland.gitlab.io/projects/gnucashr/reference/log_append.md),
  [`log_map()`](https://tinyland.gitlab.io/projects/gnucashr/reference/log_map.md),
  [`log_bind()`](https://tinyland.gitlab.io/projects/gnucashr/reference/log_bind.md)
- [`write_audit_log()`](https://tinyland.gitlab.io/projects/gnucashr/reference/write_audit_log.md)
  for persistence
- Integration with forecast operations

#### Safe Wrappers

- [`safe_read_gnucash()`](https://tinyland.gitlab.io/projects/gnucashr/reference/safe_read_gnucash.md) -
  file opening
- [`safe_trial_balance()`](https://tinyland.gitlab.io/projects/gnucashr/reference/safe_trial_balance.md) -
  report generation
- [`safe_post_transaction()`](https://tinyland.gitlab.io/projects/gnucashr/reference/safe_post_transaction.md) -
  write operations
- [`safe_consolidation()`](https://tinyland.gitlab.io/projects/gnucashr/reference/safe_consolidation.md) -
  multi-book operations
- [`safe_quick_monte_carlo()`](https://tinyland.gitlab.io/projects/gnucashr/reference/safe_quick_monte_carlo.md) -
  simulations

### Rcpp Components

#### Fraction Arithmetic

- [`fraction_to_double()`](https://tinyland.gitlab.io/projects/gnucashr/reference/fraction_to_double.md) -
  exact to floating conversion
- [`double_to_fraction()`](https://tinyland.gitlab.io/projects/gnucashr/reference/double_to_fraction.md) -
  floating to exact conversion
- [`add_fractions()`](https://tinyland.gitlab.io/projects/gnucashr/reference/add_fractions.md) -
  exact addition
- [`validate_splits_balance()`](https://tinyland.gitlab.io/projects/gnucashr/reference/validate_splits_balance.md) -
  double-entry validation

#### GUID Operations

- [`generate_guid()`](https://tinyland.gitlab.io/projects/gnucashr/reference/generate_guid.md) -
  GnuCash format GUID
- [`generate_guids()`](https://tinyland.gitlab.io/projects/gnucashr/reference/generate_guids.md) -
  batch generation
- [`validate_guid()`](https://tinyland.gitlab.io/projects/gnucashr/reference/validate_guid.md) -
  format validation
- [`validate_guids()`](https://tinyland.gitlab.io/projects/gnucashr/reference/validate_guids.md) -
  batch validation
- [`check_guid_uniqueness()`](https://tinyland.gitlab.io/projects/gnucashr/reference/check_guid_uniqueness.md) -
  duplicate detection

#### Validation

- [`validate_transaction_balance()`](https://tinyland.gitlab.io/projects/gnucashr/reference/validate_transaction_balance.md) -
  split balance check
- [`validate_split_values()`](https://tinyland.gitlab.io/projects/gnucashr/reference/validate_split_values.md) -
  value integrity
- [`calculate_running_balance()`](https://tinyland.gitlab.io/projects/gnucashr/reference/calculate_running_balance.md) -
  efficient balance calculation

#### Parallel Simulation

- [`monte_carlo_parallel()`](https://tinyland.gitlab.io/projects/gnucashr/reference/monte_carlo_parallel.md) -
  parallel MC engine
- [`monte_carlo_multi_entity()`](https://tinyland.gitlab.io/projects/gnucashr/reference/monte_carlo_multi_entity.md) -
  multi-entity MC
- [`parallel_project_scenarios()`](https://tinyland.gitlab.io/projects/gnucashr/reference/parallel_project_scenarios.md) -
  batch scenarios
- [`parallel_sensitivity_grid()`](https://tinyland.gitlab.io/projects/gnucashr/reference/parallel_sensitivity_grid.md) -
  parameter sweeps
- [`batch_project_growth()`](https://tinyland.gitlab.io/projects/gnucashr/reference/batch_project_growth.md) -
  bulk projections

### Integration Features

#### Quarto/Shiny

- [`reactive_gnucash()`](https://tinyland.gitlab.io/projects/gnucashr/reference/reactive_gnucash.md)
  for reactive connections
- [`reactive_trial_balance()`](https://tinyland.gitlab.io/projects/gnucashr/reference/reactive_trial_balance.md)
  for reactive reports
- [`reactive_balance_sheet()`](https://tinyland.gitlab.io/projects/gnucashr/reference/reactive_balance_sheet.md),
  [`reactive_income_statement()`](https://tinyland.gitlab.io/projects/gnucashr/reference/reactive_income_statement.md)
- [`reactive_monte_carlo()`](https://tinyland.gitlab.io/projects/gnucashr/reference/reactive_monte_carlo.md)
  for reactive simulations
- Dashboard metrics via
  [`dashboard_metrics()`](https://tinyland.gitlab.io/projects/gnucashr/reference/dashboard_metrics.md)

#### Account Templates

- C-corporation chart of accounts
- Small business chart of accounts
- Personal finance chart of accounts
- [`load_template()`](https://tinyland.gitlab.io/projects/gnucashr/reference/load_template.md),
  [`apply_template()`](https://tinyland.gitlab.io/projects/gnucashr/reference/apply_template.md)
- [`compare_to_template()`](https://tinyland.gitlab.io/projects/gnucashr/reference/compare_to_template.md)
  for structure validation

### Documentation

#### Vignettes

- `getting-started`: Installation and basic usage
- `forecasting`: Monte Carlo and scenario analysis
- `consolidation`: Multi-book reporting

#### Testing

- testthat test suite with 90%+ coverage
- Property-based testing via hedgehog
- Fixtures for SQLite and XML formats
