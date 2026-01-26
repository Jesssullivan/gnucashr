# Test Fixtures

This directory contains static test data files used by gnucashr tests.

## Directory Structure

```
fixtures/
  databases/
    minimal.gnucash      - Minimal valid GnuCash SQLite database
    with-accounts.gnucash - Database with standard account hierarchy
  README.md              - This file
```

## Database Fixtures

### minimal.gnucash

A minimal valid GnuCash SQLite database containing only the required structure:

- **Books**: Single book record
- **Commodities**: USD currency only
- **Accounts**: Root account only
- **Transactions**: Empty
- **Splits**: Empty

Use this fixture when you need a valid GnuCash file but do not need any account hierarchy.

### with-accounts.gnucash

A GnuCash SQLite database with a standard personal finance account hierarchy:

- **Root Account**
  - **Assets** (placeholder)
    - Current Assets (placeholder)
      - Checking Account (BANK)
      - Savings Account (BANK)
      - Cash on Hand (CASH)
  - **Liabilities** (placeholder)
    - Credit Card (CREDIT)
    - Accounts Payable (PAYABLE)
  - **Equity** (placeholder)
    - Opening Balances
    - Retained Earnings
  - **Income** (placeholder)
    - Salary
    - Interest Income
  - **Expenses** (placeholder)
    - Groceries
    - Utilities
    - Rent
    - Transportation

Use this fixture when you need a realistic account structure for testing.

## Using Fixtures in Tests

```r
# Load a fixture database
gc <- load_test_db("minimal.gnucash")

# Or with automatic cleanup
test_that("example test", {
  gc <- load_test_db_scoped("with-accounts.gnucash")

  expect_gte(nrow(gc$accounts(collected = TRUE)), 10)
})

# Skip if fixture not available
test_that("requires specific fixture", {
  skip_if_no_db_fixture("with-accounts.gnucash")
  # ... test code
})
```

## Regenerating Fixtures

If you need to regenerate the fixtures (e.g., after schema changes), you can use the following Python script:

```python
import sqlite3
import os

def create_gnucash_schema(conn):
    cursor = conn.cursor()

    cursor.execute('''
        CREATE TABLE books (
            guid TEXT PRIMARY KEY,
            root_account_guid TEXT,
            root_template_guid TEXT
        )
    ''')

    cursor.execute('''
        CREATE TABLE commodities (
            guid TEXT PRIMARY KEY,
            namespace TEXT, mnemonic TEXT, fullname TEXT,
            cusip TEXT, fraction INTEGER, quote_flag INTEGER,
            quote_source TEXT, quote_tz TEXT
        )
    ''')

    cursor.execute('''
        CREATE TABLE accounts (
            guid TEXT PRIMARY KEY,
            name TEXT, account_type TEXT, commodity_guid TEXT,
            commodity_scu INTEGER, non_std_scu INTEGER,
            parent_guid TEXT, code TEXT, description TEXT,
            hidden INTEGER, placeholder INTEGER
        )
    ''')

    cursor.execute('''
        CREATE TABLE transactions (
            guid TEXT PRIMARY KEY,
            currency_guid TEXT, num TEXT,
            post_date TEXT, enter_date TEXT, description TEXT
        )
    ''')

    cursor.execute('''
        CREATE TABLE splits (
            guid TEXT PRIMARY KEY,
            tx_guid TEXT, account_guid TEXT, memo TEXT,
            action TEXT, reconcile_state TEXT, reconcile_date TEXT,
            value_num INTEGER, value_denom INTEGER,
            quantity_num INTEGER, quantity_denom INTEGER, lot_guid TEXT
        )
    ''')

    cursor.execute('''
        CREATE TABLE prices (
            guid TEXT PRIMARY KEY,
            commodity_guid TEXT, currency_guid TEXT,
            date TEXT, source TEXT, type TEXT,
            value_num INTEGER, value_denom INTEGER
        )
    ''')

    # Indexes
    cursor.execute('CREATE INDEX idx_accounts_parent ON accounts(parent_guid)')
    cursor.execute('CREATE INDEX idx_splits_tx ON splits(tx_guid)')
    cursor.execute('CREATE INDEX idx_splits_account ON splits(account_guid)')
    cursor.execute('CREATE INDEX idx_transactions_date ON transactions(post_date)')

    conn.commit()

# Then add appropriate INSERT statements for each fixture
```

## GUID Format

GnuCash uses 32-character lowercase hexadecimal GUIDs without dashes:
- Example: `a1000000000000000000000000000001`
- Pattern: `^[a-f0-9]{32}$`

Fixture GUIDs follow a naming convention:
- `a1...` - Account GUIDs
- `c1...` - Commodity GUIDs
- `f0...` - Book (file) GUIDs
- `t1...` - Transaction GUIDs
- `s1...` - Split GUIDs

## Notes

- Fixture files should be treated as immutable in version control
- Tests should copy fixtures to temp directories before modifying
- Use `load_test_db()` with `copy = TRUE` (default) for safe testing
