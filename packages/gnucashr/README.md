# gnucashr

[![R-CMD-check](https://github.com/Jesssullivan/gnucashr/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/Jesssullivan/gnucashr/actions/workflows/R-CMD-check.yaml)
[![codecov](https://app.codecov.io/gh/Jesssullivan/gnucashr/branch/main/graph/badge.svg)](https://app.codecov.io/gh/Jesssullivan/gnucashr)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

R interface to GnuCash accounting data. Reads SQLite and XML files, provides R6 classes for accounts and transactions, supports multi-book consolidation with intercompany elimination.

## Installation

```r
# From GitHub
remotes::install_github("Jesssullivan/gnucashr")

# From GitLab (primary repository)
remotes::install_gitlab("tinyland/projects/gnucashr")
```

Requires R >= 4.1.0, C++17 compiler.

## Usage

```r
library(gnucashr)

# Open GnuCash file (SQLite or XML)
gc <- read_gnucash("books.gnucash")

# Trial balance
tb <- trial_balance(gc, as_of = "2026-01-15")

# Financial statements (take trial balance as input)
balance_sheet(tb)
income_statement(tb)

# Account transactions
account_transactions(gc, "Assets:Bank:Checking",
                     start = "2025-01-01", end = "2025-12-31")

close(gc)
```

## Features

### Core Reading & Writing
Read GnuCash SQLite and XML files; create accounts and post transactions with automatic backup.
- `read_gnucash()`, `GnuCashDB`, `parse_gnucash_xml()`
- `create_account()`, `post_transaction()`, `post_transfer()`
- `create_backup()`, `restore_backup()`, `with_backup()`

### Account Operations
Navigate account hierarchies and query balances with path-based access.
- `account_tree()`, `account_balance()`, `account_balances()`
- `account_transactions()`, `aggregate_by_type()`

### Financial Reports
Generate standard financial statements with formatting and comparison tools.
- `trial_balance()`, `balance_sheet()`, `income_statement()`
- `gt_trial_balance()`, `gt_balance_sheet()`, `gt_income_statement()`
- `compare_income_statements()`, `plot_balance_sheet()`

### Multi-Book Consolidation
Manage corporate structures with multiple GnuCash books and intercompany elimination.
- `BookCollection`, `consolidation_summary()`
- `build_consolidated_trial_balance()`, `validate_consolidation()`
- `apply_ic_eliminations()`, `create_standard_ic_rules()`

### Forecasting - Lazy Evaluation
Build deferred computation pipelines for financial projections.
- `LazyForecast`, `from_book()`, `from_collection()`
- `lf_grow()`, `lf_scenario()`, `lf_monte_carlo()`, `lf_sensitivity()`

### Forecasting - Monte Carlo & Sensitivity
Rcpp-accelerated parallel Monte Carlo simulation and sensitivity analysis.
- `monte_carlo_parallel()`, `quick_monte_carlo()`, `extract_mc_summary()`
- `parallel_sensitivity_grid()`, `quick_sensitivity()`, `plot_sensitivity()`

### Error Handling - Result Monad
Functional error handling with explicit Ok/Err types for safe operations.
- `ok()`, `err()`, `is_ok()`, `is_err()`, `unwrap()`
- `result_bind()`, `result_map()`, `try_result()`
- `safe_read_gnucash()`, `safe_post_transaction()`, `safe_trial_balance()`

### Error Handling - Logger Monad
Track computation steps with audit trail logging for forecasts.
- `logged()`, `logged_value()`, `logged_log()`
- `audited_forecast()`, `audited_monte_carlo()`, `write_forecast_audit()`

### Data Import - OFX/QFX
Import bank statements with Rcpp-accelerated parsing and validation.
- `import_ofx()`, `parse_ofx_cpp()`, `detect_ofx_version()`
- `import_ofx_to_gnucash()`, `validate_ofx_import()`

### Data Import - CSV
Import from PayPal, Stripe, and QuickBooks with duplicate detection.
- `import_paypal_csv()`, `import_stripe_csv()`, `import_quickbooks_csv()`
- `combine_csv_imports()`, `preview_csv_mapping()`

### Rcpp Utilities
High-performance C++ for fraction arithmetic, GUID handling, and validation.
- `fraction_to_double()`, `double_to_fraction()`, `add_fractions()`
- `generate_guid()`, `validate_guid()`, `check_guid_uniqueness()`
- `validate_transaction_balance()`, `calculate_running_balance()`

### Budgets & Scheduled Transactions
Budget creation/comparison and recurring transaction management.
- `Budget`, `budget_from_df()`, `annual_budget()`, `monthly_budget()`
- `ScheduledTransactionManager`, `scheduled_from_df()`

### Commodities, Prices & Lots
Track currencies, securities, historical prices, and investment lots for cost basis.
- `Commodity`, `currency()`, `security()`
- `PriceDB`, `new_price()`, `prices_from_df()`
- `LotManager`, `lots_from_df()`

### Shiny/Quarto Integration
Reactive wrappers for building interactive dashboards.
- `reactive_gnucash()`, `reactive_trial_balance()`, `reactive_forecast()`
- `reactive_balance_sheet()`, `reactive_monte_carlo()`
- Account templates (C-corp, small business, personal)

## Documentation

### Vignettes
- `getting-started`: Basic usage and concepts
- `forecasting`: Monte Carlo and scenario analysis
- `consolidation`: Multi-book reporting
- `importing-data`: OFX and CSV import workflows

### Technical Articles
- `performance-benchmarks`: Rcpp acceleration benchmarks
- `rcpp-internals`: C++ implementation details


--- 

This repo is a downstream artifact from Tinyland.dev, Inc & xoxd.ai.  
