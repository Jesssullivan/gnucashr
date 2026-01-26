# gnucashr

[![R-CMD-check](https://github.com/Jesssullivan/gnucashr/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/Jesssullivan/gnucashr/actions/workflows/R-CMD-check.yaml)
[![GitLab CI](https://gitlab.com/tinyland/projects/gnucashr/badges/main/pipeline.svg)](https://gitlab.com/tinyland/projects/gnucashr/-/pipelines)
[![codecov](https://codecov.io/gh/Jesssullivan/gnucashr/branch/main/graph/badge.svg)](https://codecov.io/gh/Jesssullivan/gnucashr)
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

### Core (v0.2.0)
- SQLite and XML file support via `GnuCashDB` R6 class
- Account tree navigation with path-based access
- Trial balance, balance sheet, income statement
- Transaction queries with date filtering
- Commodity and price database access
- Budget and scheduled transaction support
- Lot tracking for cost basis

### Multi-Entity
- `BookCollection` for consolidated reporting
- Intercompany transaction elimination
- Per-entity and consolidated views

### Forecasting
- Lazy evaluation via `LazyForecast` class
- Monte Carlo simulation (Rcpp-accelerated)
- Sensitivity analysis
- Scenario comparison

### Write Operations
- Create accounts and transactions
- Automatic backup before writes
- GUID generation and validation

### Error Handling
- `Result` type (Ok/Err) for explicit error handling
- `Logged` type for audit trails
- Safe wrappers for all operations

### Data Import
- OFX/QFX bank statement import (Rcpp-accelerated parsing)
- PayPal CSV import with fee extraction
- Stripe CSV import with payout matching
- QuickBooks CSV import (QBO/Desktop formats)
- Duplicate detection and validation

### Integration
- Quarto reactive widgets for dashboards
- Account templates (C-corp, small business, personal)
- targets pipeline compatible

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

Contributions are welcome! Please see our [Contributing Guide](docs/CONTRIBUTING.md) for details on:

- Development workflow
- Code style guidelines
- Testing requirements
- Pull request process

For repository setup and mirroring information, see [Repository Setup](docs/REPOSITORY_SETUP.md).

## Security

For security concerns, please review our [Security Policy](docs/SECURITY.md). Do not report security vulnerabilities through public issues.

## License

MIT
