# gnucashr

Read and analyze GnuCash accounting data in R.

## Installation

```r
# Install from GitHub
remotes::install_github("Jesssullivan/gnucashr")
```
## Quick Start

```r
library(gnucashr)

# Open a GnuCash SQLite file
gc <- read_gnucash("path/to/books.gnucash")

# Get trial balance
trial_balance(gc, as_of = "2026-01-15")

# Get account transactions
account_transactions(gc,
                     account = "Assets:Bank:Checking",
                     start = "2025-01-01",
                     end = "2025-12-31")

# Close when done
close(gc)
```

## Features

- **Read-only access** to GnuCash SQLite files
- **Lazy evaluation** with dbplyr for efficient queries
- **Tidy data** - all outputs are tibbles
- **Account navigation** with path-based access (`Assets:Bank:Checking`)
- **Multi-entity support** for consolidated reporting

## Roadmap

See `docs/gnucashr-architecture.md` for the full design document.

### Phase 1: Core Read-Only (Current)
- [ ] SQLite connection with RSQLite
- [ ] Basic table accessors
- [ ] `read_gnucash()` function
- [ ] `trial_balance()` with date filtering
- [ ] `account_transactions()` function

### Phase 2: Reporting Functions
- [ ] `income_statement()`
- [ ] `balance_sheet()`
- [ ] `account_tree()` navigation
- [ ] `validate_gnucash()` checks

### Phase 3: Multi-Entity
- [ ] Entity filtering and consolidation
- [ ] Intercompany elimination
- [ ] targets integration vignette

## License

MIT
