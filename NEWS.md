# gnucashr 0.2.0

## New Features

### Data Import
* Add OFX/QFX bank statement import with Rcpp-accelerated parsing
* Add PayPal CSV import with automatic format detection and fee extraction
* Add Stripe CSV import with cents-to-dollars conversion
* Add QuickBooks CSV import supporting QBO and Desktop formats
* Add `combine_csv_imports()` for merging multiple import sources
* Add `validate_ofx_import()` for duplicate detection
* Add `preview_csv_mapping()` debug helper

### CI/CD Infrastructure
* Add GitLab CI pipeline (primary) with R CMD check, coverage, pkgdown Pages
* Add GitHub Actions (secondary) with multi-platform matrix
* Add codecov integration with 80% coverage target

### Testing Framework
* Add property-based testing with hedgehog for fraction arithmetic
* Add PBT tests for GUID generation, Monte Carlo reproducibility
* Add test fixtures for SQLite databases
* Add KeePassXC-style credential providers for integration tests

### Documentation
* Add pkgdown site with dark mode support
* Add `importing-data` vignette for CSV/OFX workflows
* Add `performance-benchmarks` technical article
* Add `rcpp-internals` contributor documentation
* Add CONTRIBUTING.md, SECURITY.md, REPOSITORY_SETUP.md

## Bug Fixes

* Fix `account_transactions()` parameter documentation

---

# gnucashr 0.3.0 (planned)

## Planned Features

### Performance Improvements
* Add data.table backend option for large books
* Improve memory efficiency for multi-book consolidation
* Add incremental loading for large transaction histories

### Reporting Enhancements
* Add comparative balance sheets (period-over-period)
* Add cash flow statement generation
* Add budget vs actual variance reports

### Integration
* Add targets pipeline integration vignette
* Add Plumber API template for serving reports
* Add Shiny module library for dashboard components

---

# gnucashr 0.2.0

*Released: 2026-01-15*
*Initial public release*

## Core Features

### GnuCash File Support
* Read GnuCash SQLite files via `read_gnucash()`
* Read GnuCash XML files with automatic format detection
* Create new GnuCash databases with `create_gnucash()`
* `GnuCashDB` R6 class for database connection management

### Account Management
* Account tree navigation with full path support
* Path-based account lookup (e.g., "Assets:Bank:Checking")
* GUID-based account access
* Account type classification (ASSET, LIABILITY, EQUITY, INCOME, EXPENSE)

### Financial Statements
* Trial balance generation via `trial_balance()`
* Balance sheet via `balance_sheet()` with standard formatting
* Income statement via `income_statement()` with period selection
* `gt` table output for publication-quality reports

### Transaction Queries
* `account_transactions()` for filtered transaction retrieval
* Date range filtering with lubridate integration
* Running balance calculation (Rcpp-accelerated)
* Split-level detail access

### Commodities and Prices
* `Commodity` R6 class for currency and security tracking
* `PriceDB` R6 class for historical price management
* Currency conversion support
* ISO 4217 currency namespace handling

### Budgets and Schedules
* `Budget` R6 class for budget management
* `ScheduledTransaction` for recurring transaction templates
* Monthly, quarterly, and annual budget views
* Budget vs actual comparison

### Lot Tracking
* `Lot` R6 class for investment lot tracking
* `LotManager` for portfolio cost basis
* FIFO/LIFO lot matching support
* Realized gain/loss calculation

## Multi-Entity Features

### Book Collection
* `BookCollection` R6 class for multi-book management
* Named book access with automatic connection pooling
* Parallel loading support via future/furrr

### Consolidation
* `build_consolidated_trial_balance()` for combined statements
* Intercompany elimination rules
* `apply_ic_eliminations()` for automatic IC removal
* Consolidation validation and balance checking
* Entity-level and consolidated views

## Forecasting Features

### Lazy Evaluation
* `LazyForecast` R6 class for deferred computation
* Chainable operations: `lf_grow()`, `lf_filter()`, `lf_scenario()`
* Collect only when needed via `lf_collect()`

### Monte Carlo Simulation
* `monte_carlo_parallel()` Rcpp-accelerated simulation
* Multi-entity Monte Carlo via `monte_carlo_multi_entity()`
* Configurable parameters: growth rates, expense ratios, periods
* Reproducible results with seed control

### Sensitivity Analysis
* `parallel_sensitivity_grid()` for parameter sweeps
* `quick_sensitivity()` for rapid what-if analysis
* Visualization via `plot_sensitivity()`

### Scenario Comparison
* `compare_forecasts()` for side-by-side scenarios
* `parallel_project_scenarios()` for batch projections
* Growth trajectory visualization

## Write Operations

### Account Creation
* `create_account()` with type validation
* `create_account_path()` for nested account creation
* `update_account()` for account modification
* `delete_account()` with child handling

### Transaction Posting
* `post_transaction()` for new transactions
* `post_transfer()` convenience wrapper
* `batch_post_transactions()` for bulk operations
* `void_transaction()` and `delete_transaction()`

### Backup System
* `create_backup()` before write operations
* `with_backup()` wrapper for safe writes
* `list_backups()` and `restore_backup()`
* Automatic backup cleanup via `clean_old_backups()`

## Error Handling

### Result Monad
* `ok()` and `err()` constructors
* `is_ok()`, `is_err()` predicates
* `unwrap()` with optional default
* `result_map()`, `result_bind()` for chaining
* `result_match()` for pattern matching
* `try_result()` for exception capture

### Logged Monad
* `logged()` for audit trails
* `log_append()`, `log_map()`, `log_bind()`
* `write_audit_log()` for persistence
* Integration with forecast operations

### Safe Wrappers
* `safe_read_gnucash()` - file opening
* `safe_trial_balance()` - report generation
* `safe_post_transaction()` - write operations
* `safe_consolidation()` - multi-book operations
* `safe_quick_monte_carlo()` - simulations

## Rcpp Components

### Fraction Arithmetic
* `fraction_to_double()` - exact to floating conversion
* `double_to_fraction()` - floating to exact conversion
* `add_fractions()` - exact addition
* `validate_splits_balance()` - double-entry validation

### GUID Operations
* `generate_guid()` - GnuCash format GUID
* `generate_guids()` - batch generation
* `validate_guid()` - format validation
* `validate_guids()` - batch validation
* `check_guid_uniqueness()` - duplicate detection

### Validation
* `validate_transaction_balance()` - split balance check
* `validate_split_values()` - value integrity
* `calculate_running_balance()` - efficient balance calculation

### Parallel Simulation
* `monte_carlo_parallel()` - parallel MC engine
* `monte_carlo_multi_entity()` - multi-entity MC
* `parallel_project_scenarios()` - batch scenarios
* `parallel_sensitivity_grid()` - parameter sweeps
* `batch_project_growth()` - bulk projections

## Integration Features

### Quarto/Shiny
* `reactive_gnucash()` for reactive connections
* `reactive_trial_balance()` for reactive reports
* `reactive_balance_sheet()`, `reactive_income_statement()`
* `reactive_monte_carlo()` for reactive simulations
* Dashboard metrics via `dashboard_metrics()`

### Account Templates
* C-corporation chart of accounts
* Small business chart of accounts
* Personal finance chart of accounts
* `load_template()`, `apply_template()`
* `compare_to_template()` for structure validation

## Documentation

### Vignettes
* `getting-started`: Installation and basic usage
* `forecasting`: Monte Carlo and scenario analysis
* `consolidation`: Multi-book reporting

### Testing
* testthat test suite with 90%+ coverage
* Property-based testing via hedgehog
* Fixtures for SQLite and XML formats
