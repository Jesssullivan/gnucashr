# gnucashr

R interface to GnuCash accounting data. Reads SQLite and XML files, provides R6 classes for accounts and transactions, supports multi-book consolidation with intercompany elimination.

## Installation

```r
remotes::install_github("Jesssullivan/gnucashr")
```

Requires R >= 4.1.0, C++17 compiler.

## Usage

```r
library(gnucashr)

# Open GnuCash file (SQLite or XML)
gc <- read_gnucash("books.gnucash")

# Trial balance
trial_balance(gc, as_of = "2026-01-15")

# Financial statements
balance_sheet(gc, as_of = "2026-01-15")
income_statement(gc, start = "2026-01-01", end = "2026-01-31")

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

### Integration
- Quarto reactive widgets for dashboards
- Account templates (C-corp, small business, personal)
- targets pipeline compatible

## Vignettes

- `getting-started`: Basic usage and concepts
- `forecasting`: Monte Carlo and scenario analysis
- `consolidation`: Multi-book reporting

## License

MIT
