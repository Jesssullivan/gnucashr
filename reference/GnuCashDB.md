# GnuCash Database Connection

GnuCash Database Connection

GnuCash Database Connection

## Details

R6 class for managing connections to GnuCash files (SQLite or XML
format). Provides unified interface for both file formats with lazy
evaluation.

## Methods

### Public methods

- [`GnuCashDB$new()`](#method-GnuCashDB-new)

- [`GnuCashDB$accounts()`](#method-GnuCashDB-accounts)

- [`GnuCashDB$transactions()`](#method-GnuCashDB-transactions)

- [`GnuCashDB$splits()`](#method-GnuCashDB-splits)

- [`GnuCashDB$commodities()`](#method-GnuCashDB-commodities)

- [`GnuCashDB$prices()`](#method-GnuCashDB-prices)

- [`GnuCashDB$get_account()`](#method-GnuCashDB-get_account)

- [`GnuCashDB$get_transaction()`](#method-GnuCashDB-get_transaction)

- [`GnuCashDB$account_tree()`](#method-GnuCashDB-account_tree)

- [`GnuCashDB$metadata()`](#method-GnuCashDB-metadata)

- [`GnuCashDB$is_connected()`](#method-GnuCashDB-is_connected)

- [`GnuCashDB$refresh()`](#method-GnuCashDB-refresh)

- [`GnuCashDB$close()`](#method-GnuCashDB-close)

- [`GnuCashDB$print()`](#method-GnuCashDB-print)

- [`GnuCashDB$clone()`](#method-GnuCashDB-clone)

------------------------------------------------------------------------

### Method `new()`

Create a new GnuCashDB connection

#### Usage

    GnuCashDB$new(path, read_only = TRUE)

#### Arguments

- `path`:

  Path to GnuCash file (.gnucash SQLite or XML)

- `read_only`:

  Open in read-only mode (default TRUE)

------------------------------------------------------------------------

### Method `accounts()`

Get accounts table (lazy for SQLite, tibble for XML)

#### Usage

    GnuCashDB$accounts(collected = FALSE)

#### Arguments

- `collected`:

  If TRUE, always return collected tibble

------------------------------------------------------------------------

### Method `transactions()`

Get transactions table

#### Usage

    GnuCashDB$transactions(collected = FALSE)

#### Arguments

- `collected`:

  If TRUE, always return collected tibble

------------------------------------------------------------------------

### Method `splits()`

Get splits table

#### Usage

    GnuCashDB$splits(collected = FALSE)

#### Arguments

- `collected`:

  If TRUE, always return collected tibble

------------------------------------------------------------------------

### Method `commodities()`

Get commodities table

#### Usage

    GnuCashDB$commodities(collected = FALSE)

#### Arguments

- `collected`:

  If TRUE, always return collected tibble

------------------------------------------------------------------------

### Method `prices()`

Get prices table

#### Usage

    GnuCashDB$prices(collected = FALSE)

#### Arguments

- `collected`:

  If TRUE, always return collected tibble

------------------------------------------------------------------------

### Method `get_account()`

Get a specific account by GUID or path

#### Usage

    GnuCashDB$get_account(identifier)

#### Arguments

- `identifier`:

  Account GUID or colon-separated path (e.g., "Assets:Bank:Checking")

------------------------------------------------------------------------

### Method `get_transaction()`

Get a specific transaction by GUID

#### Usage

    GnuCashDB$get_transaction(guid)

#### Arguments

- `guid`:

  Transaction GUID

------------------------------------------------------------------------

### Method [`account_tree()`](https://tinyland.gitlab.io/projects/gnucashr/reference/account_tree.md)

Build account tree with full paths

#### Usage

    GnuCashDB$account_tree()

------------------------------------------------------------------------

### Method `metadata()`

Get book metadata

#### Usage

    GnuCashDB$metadata()

------------------------------------------------------------------------

### Method `is_connected()`

Check if database is connected/loaded

#### Usage

    GnuCashDB$is_connected()

------------------------------------------------------------------------

### Method `refresh()`

Refresh cached data from database

#### Usage

    GnuCashDB$refresh()

------------------------------------------------------------------------

### Method [`close()`](https://rdrr.io/r/base/connections.html)

Close the database connection

#### Usage

    GnuCashDB$close()

------------------------------------------------------------------------

### Method [`print()`](https://rdrr.io/r/base/print.html)

Print method Initialize SQLite connection Initialize from XML file Load
default currency for SQLite format

#### Usage

    GnuCashDB$print()

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    GnuCashDB$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
