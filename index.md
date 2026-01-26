# gnucashr

[![R-CMD-check](https://github.com/Jesssullivan/gnucashr/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/Jesssullivan/gnucashr/actions/workflows/R-CMD-check.yaml)
[![GitLab
CI](https://gitlab.com/tinyland/projects/gnucashr/badges/main/pipeline.svg)](https://gitlab.com/tinyland/projects/gnucashr/-/pipelines)
[![codecov](https://app.codecov.io/gh/Jesssullivan/gnucashr/branch/main/graph/badge.svg)](https://app.codecov.io/gh/Jesssullivan/gnucashr)
[![License:
MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

R interface to GnuCash accounting data. Reads SQLite and XML files,
provides R6 classes for accounts and transactions, supports multi-book
consolidation with intercompany elimination.

## Installation

``` r
# From GitHub
remotes::install_github("Jesssullivan/gnucashr")

# From GitLab (primary repository)
remotes::install_gitlab("tinyland/projects/gnucashr")
```

Requires R \>= 4.1.0, C++17 compiler.

## Usage

``` r
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

Read GnuCash SQLite and XML files; create accounts and post transactions
with automatic backup. -
[`read_gnucash()`](https://tinyland.gitlab.io/projects/gnucashr/reference/read_gnucash.md),
`GnuCashDB`,
[`parse_gnucash_xml()`](https://tinyland.gitlab.io/projects/gnucashr/reference/parse_gnucash_xml.md) -
[`create_account()`](https://tinyland.gitlab.io/projects/gnucashr/reference/create_account.md),
[`post_transaction()`](https://tinyland.gitlab.io/projects/gnucashr/reference/post_transaction.md),
[`post_transfer()`](https://tinyland.gitlab.io/projects/gnucashr/reference/post_transfer.md) -
[`create_backup()`](https://tinyland.gitlab.io/projects/gnucashr/reference/create_backup.md),
[`restore_backup()`](https://tinyland.gitlab.io/projects/gnucashr/reference/restore_backup.md),
[`with_backup()`](https://tinyland.gitlab.io/projects/gnucashr/reference/with_backup.md)

### Account Operations

Navigate account hierarchies and query balances with path-based
access. -
[`account_tree()`](https://tinyland.gitlab.io/projects/gnucashr/reference/account_tree.md),
[`account_balance()`](https://tinyland.gitlab.io/projects/gnucashr/reference/account_balance.md),
[`account_balances()`](https://tinyland.gitlab.io/projects/gnucashr/reference/account_balances.md) -
[`account_transactions()`](https://tinyland.gitlab.io/projects/gnucashr/reference/account_transactions.md),
[`aggregate_by_type()`](https://tinyland.gitlab.io/projects/gnucashr/reference/aggregate_by_type.md)

### Financial Reports

Generate standard financial statements with formatting and comparison
tools. -
[`trial_balance()`](https://tinyland.gitlab.io/projects/gnucashr/reference/trial_balance.md),
[`balance_sheet()`](https://tinyland.gitlab.io/projects/gnucashr/reference/balance_sheet.md),
[`income_statement()`](https://tinyland.gitlab.io/projects/gnucashr/reference/income_statement.md) -
[`gt_trial_balance()`](https://tinyland.gitlab.io/projects/gnucashr/reference/gt_trial_balance.md),
[`gt_balance_sheet()`](https://tinyland.gitlab.io/projects/gnucashr/reference/gt_balance_sheet.md),
[`gt_income_statement()`](https://tinyland.gitlab.io/projects/gnucashr/reference/gt_income_statement.md) -
[`compare_income_statements()`](https://tinyland.gitlab.io/projects/gnucashr/reference/compare_income_statements.md),
[`plot_balance_sheet()`](https://tinyland.gitlab.io/projects/gnucashr/reference/plot_balance_sheet.md)

### Multi-Book Consolidation

Manage corporate structures with multiple GnuCash books and intercompany
elimination. - `BookCollection`,
[`consolidation_summary()`](https://tinyland.gitlab.io/projects/gnucashr/reference/consolidation_summary.md) -
[`build_consolidated_trial_balance()`](https://tinyland.gitlab.io/projects/gnucashr/reference/build_consolidated_trial_balance.md),
[`validate_consolidation()`](https://tinyland.gitlab.io/projects/gnucashr/reference/validate_consolidation.md) -
[`apply_ic_eliminations()`](https://tinyland.gitlab.io/projects/gnucashr/reference/apply_ic_eliminations.md),
[`create_standard_ic_rules()`](https://tinyland.gitlab.io/projects/gnucashr/reference/create_standard_ic_rules.md)

### Forecasting - Lazy Evaluation

Build deferred computation pipelines for financial projections. -
`LazyForecast`,
[`from_book()`](https://tinyland.gitlab.io/projects/gnucashr/reference/from_book.md),
[`from_collection()`](https://tinyland.gitlab.io/projects/gnucashr/reference/from_collection.md) -
[`lf_grow()`](https://tinyland.gitlab.io/projects/gnucashr/reference/lf_grow.md),
[`lf_scenario()`](https://tinyland.gitlab.io/projects/gnucashr/reference/lf_scenario.md),
[`lf_monte_carlo()`](https://tinyland.gitlab.io/projects/gnucashr/reference/lf_monte_carlo.md),
[`lf_sensitivity()`](https://tinyland.gitlab.io/projects/gnucashr/reference/lf_sensitivity.md)

### Forecasting - Monte Carlo & Sensitivity

Rcpp-accelerated parallel Monte Carlo simulation and sensitivity
analysis. -
[`monte_carlo_parallel()`](https://tinyland.gitlab.io/projects/gnucashr/reference/monte_carlo_parallel.md),
[`quick_monte_carlo()`](https://tinyland.gitlab.io/projects/gnucashr/reference/quick_monte_carlo.md),
[`extract_mc_summary()`](https://tinyland.gitlab.io/projects/gnucashr/reference/extract_mc_summary.md) -
[`parallel_sensitivity_grid()`](https://tinyland.gitlab.io/projects/gnucashr/reference/parallel_sensitivity_grid.md),
[`quick_sensitivity()`](https://tinyland.gitlab.io/projects/gnucashr/reference/quick_sensitivity.md),
[`plot_sensitivity()`](https://tinyland.gitlab.io/projects/gnucashr/reference/plot_sensitivity.md)

### Error Handling - Result Monad

Functional error handling with explicit Ok/Err types for safe
operations. -
[`ok()`](https://tinyland.gitlab.io/projects/gnucashr/reference/ok.md),
[`err()`](https://tinyland.gitlab.io/projects/gnucashr/reference/err.md),
[`is_ok()`](https://tinyland.gitlab.io/projects/gnucashr/reference/is_ok.md),
[`is_err()`](https://tinyland.gitlab.io/projects/gnucashr/reference/is_err.md),
[`unwrap()`](https://tinyland.gitlab.io/projects/gnucashr/reference/unwrap.md) -
[`result_bind()`](https://tinyland.gitlab.io/projects/gnucashr/reference/result_bind.md),
[`result_map()`](https://tinyland.gitlab.io/projects/gnucashr/reference/result_map.md),
[`try_result()`](https://tinyland.gitlab.io/projects/gnucashr/reference/try_result.md) -
[`safe_read_gnucash()`](https://tinyland.gitlab.io/projects/gnucashr/reference/safe_read_gnucash.md),
[`safe_post_transaction()`](https://tinyland.gitlab.io/projects/gnucashr/reference/safe_post_transaction.md),
[`safe_trial_balance()`](https://tinyland.gitlab.io/projects/gnucashr/reference/safe_trial_balance.md)

### Error Handling - Logger Monad

Track computation steps with audit trail logging for forecasts. -
[`logged()`](https://tinyland.gitlab.io/projects/gnucashr/reference/logged.md),
[`logged_value()`](https://tinyland.gitlab.io/projects/gnucashr/reference/logged_value.md),
[`logged_log()`](https://tinyland.gitlab.io/projects/gnucashr/reference/logged_log.md) -
[`audited_forecast()`](https://tinyland.gitlab.io/projects/gnucashr/reference/audited_forecast.md),
[`audited_monte_carlo()`](https://tinyland.gitlab.io/projects/gnucashr/reference/audited_monte_carlo.md),
[`write_forecast_audit()`](https://tinyland.gitlab.io/projects/gnucashr/reference/write_forecast_audit.md)

### Data Import - OFX/QFX

Import bank statements with Rcpp-accelerated parsing and validation. -
[`import_ofx()`](https://tinyland.gitlab.io/projects/gnucashr/reference/import_ofx.md),
[`parse_ofx_cpp()`](https://tinyland.gitlab.io/projects/gnucashr/reference/parse_ofx_cpp.md),
[`detect_ofx_version()`](https://tinyland.gitlab.io/projects/gnucashr/reference/detect_ofx_version.md) -
[`import_ofx_to_gnucash()`](https://tinyland.gitlab.io/projects/gnucashr/reference/import_ofx_to_gnucash.md),
[`validate_ofx_import()`](https://tinyland.gitlab.io/projects/gnucashr/reference/validate_ofx_import.md)

### Data Import - CSV

Import from PayPal, Stripe, and QuickBooks with duplicate detection. -
[`import_paypal_csv()`](https://tinyland.gitlab.io/projects/gnucashr/reference/import_paypal_csv.md),
[`import_stripe_csv()`](https://tinyland.gitlab.io/projects/gnucashr/reference/import_stripe_csv.md),
[`import_quickbooks_csv()`](https://tinyland.gitlab.io/projects/gnucashr/reference/import_quickbooks_csv.md) -
[`combine_csv_imports()`](https://tinyland.gitlab.io/projects/gnucashr/reference/combine_csv_imports.md),
[`preview_csv_mapping()`](https://tinyland.gitlab.io/projects/gnucashr/reference/preview_csv_mapping.md)

### Rcpp Utilities

High-performance C++ for fraction arithmetic, GUID handling, and
validation. -
[`fraction_to_double()`](https://tinyland.gitlab.io/projects/gnucashr/reference/fraction_to_double.md),
[`double_to_fraction()`](https://tinyland.gitlab.io/projects/gnucashr/reference/double_to_fraction.md),
[`add_fractions()`](https://tinyland.gitlab.io/projects/gnucashr/reference/add_fractions.md) -
[`generate_guid()`](https://tinyland.gitlab.io/projects/gnucashr/reference/generate_guid.md),
[`validate_guid()`](https://tinyland.gitlab.io/projects/gnucashr/reference/validate_guid.md),
[`check_guid_uniqueness()`](https://tinyland.gitlab.io/projects/gnucashr/reference/check_guid_uniqueness.md) -
[`validate_transaction_balance()`](https://tinyland.gitlab.io/projects/gnucashr/reference/validate_transaction_balance.md),
[`calculate_running_balance()`](https://tinyland.gitlab.io/projects/gnucashr/reference/calculate_running_balance.md)

### Budgets & Scheduled Transactions

Budget creation/comparison and recurring transaction management. -
`Budget`,
[`budget_from_df()`](https://tinyland.gitlab.io/projects/gnucashr/reference/budget_from_df.md),
[`annual_budget()`](https://tinyland.gitlab.io/projects/gnucashr/reference/annual_budget.md),
[`monthly_budget()`](https://tinyland.gitlab.io/projects/gnucashr/reference/monthly_budget.md) -
`ScheduledTransactionManager`,
[`scheduled_from_df()`](https://tinyland.gitlab.io/projects/gnucashr/reference/scheduled_from_df.md)

### Commodities, Prices & Lots

Track currencies, securities, historical prices, and investment lots for
cost basis. - `Commodity`,
[`currency()`](https://tinyland.gitlab.io/projects/gnucashr/reference/currency.md),
[`security()`](https://tinyland.gitlab.io/projects/gnucashr/reference/security.md) -
`PriceDB`,
[`new_price()`](https://tinyland.gitlab.io/projects/gnucashr/reference/new_price.md),
[`prices_from_df()`](https://tinyland.gitlab.io/projects/gnucashr/reference/prices_from_df.md) -
`LotManager`,
[`lots_from_df()`](https://tinyland.gitlab.io/projects/gnucashr/reference/lots_from_df.md)

### Shiny/Quarto Integration

Reactive wrappers for building interactive dashboards. -
[`reactive_gnucash()`](https://tinyland.gitlab.io/projects/gnucashr/reference/reactive_gnucash.md),
[`reactive_trial_balance()`](https://tinyland.gitlab.io/projects/gnucashr/reference/reactive_trial_balance.md),
[`reactive_forecast()`](https://tinyland.gitlab.io/projects/gnucashr/reference/reactive_forecast.md) -
[`reactive_balance_sheet()`](https://tinyland.gitlab.io/projects/gnucashr/reference/reactive_balance_sheet.md),
[`reactive_monte_carlo()`](https://tinyland.gitlab.io/projects/gnucashr/reference/reactive_monte_carlo.md) -
Account templates (C-corp, small business, personal)

## Documentation

### Vignettes

- `getting-started`: Basic usage and concepts
- `forecasting`: Monte Carlo and scenario analysis
- `consolidation`: Multi-book reporting
- `importing-data`: OFX and CSV import workflows

### Technical Articles

- `performance-benchmarks`: Rcpp acceleration benchmarks
- `rcpp-internals`: C++ implementation details

## Contributing

Contributions are welcome! Please see our [Contributing
Guide](https://gitlab.com/tinyland/projects/gnucashr/-/blob/main/docs/CONTRIBUTING.md)
for details on:

- Development workflow
- Code style guidelines
- Testing requirements
- Pull request process

For repository setup and mirroring information, see [Repository
Setup](https://gitlab.com/tinyland/projects/gnucashr/-/blob/main/docs/REPOSITORY_SETUP.md).

## Security

For security concerns, please review our [Security
Policy](https://gitlab.com/tinyland/projects/gnucashr/-/blob/main/docs/SECURITY.md).
Do not report security vulnerabilities through public issues.

## License

MIT
