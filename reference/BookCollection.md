# BookCollection Class

BookCollection Class

BookCollection Class

## Details

R6 class for managing multiple GnuCash books with consolidation support.
Enables multi-entity financial modeling with intercompany elimination.

## Methods

### Public methods

- [`BookCollection$new()`](#method-BookCollection-new)

- [`BookCollection$add_book()`](#method-BookCollection-add_book)

- [`BookCollection$remove_book()`](#method-BookCollection-remove_book)

- [`BookCollection$get_book()`](#method-BookCollection-get_book)

- [`BookCollection$list_books()`](#method-BookCollection-list_books)

- [`BookCollection$each()`](#method-BookCollection-each)

- [`BookCollection$all_accounts()`](#method-BookCollection-all_accounts)

- [`BookCollection$all_transactions()`](#method-BookCollection-all_transactions)

- [`BookCollection$all_splits()`](#method-BookCollection-all_splits)

- [`BookCollection$add_ic_rule()`](#method-BookCollection-add_ic_rule)

- [`BookCollection$get_ic_rules()`](#method-BookCollection-get_ic_rules)

- [`BookCollection$setup_tinyland_ic_rules()`](#method-BookCollection-setup_tinyland_ic_rules)

- [`BookCollection$consolidated_trial_balance()`](#method-BookCollection-consolidated_trial_balance)

- [`BookCollection$consolidated_balance_sheet()`](#method-BookCollection-consolidated_balance_sheet)

- [`BookCollection$consolidated_income_statement()`](#method-BookCollection-consolidated_income_statement)

- [`BookCollection$intercompany_reconciliation()`](#method-BookCollection-intercompany_reconciliation)

- [`BookCollection$close()`](#method-BookCollection-close)

- [`BookCollection$print()`](#method-BookCollection-print)

- [`BookCollection$clone()`](#method-BookCollection-clone)

------------------------------------------------------------------------

### Method `new()`

Create a new BookCollection

#### Usage

    BookCollection$new(entity_config = NULL)

#### Arguments

- `entity_config`:

  Optional entity configuration from load_entity_config()

------------------------------------------------------------------------

### Method `add_book()`

Add a book to the collection

#### Usage

    BookCollection$add_book(name, path, read_only = TRUE)

#### Arguments

- `name`:

  Unique identifier for this book (e.g., "inc", "products")

- `path`:

  Path to GnuCash file

- `read_only`:

  Open in read-only mode (default TRUE)

------------------------------------------------------------------------

### Method `remove_book()`

Remove a book from the collection

#### Usage

    BookCollection$remove_book(name)

#### Arguments

- `name`:

  Book identifier to remove

------------------------------------------------------------------------

### Method `get_book()`

Get a specific book

#### Usage

    BookCollection$get_book(name)

#### Arguments

- `name`:

  Book identifier

------------------------------------------------------------------------

### Method `list_books()`

List all books in the collection

#### Usage

    BookCollection$list_books()

------------------------------------------------------------------------

### Method `each()`

Apply a function to each book (monadic each)

#### Usage

    BookCollection$each(fn)

#### Arguments

- `fn`:

  Function to apply, receives (book, name) arguments

#### Returns

List of results keyed by book name

------------------------------------------------------------------------

### Method `all_accounts()`

Get combined accounts from all books

#### Usage

    BookCollection$all_accounts(with_book_name = TRUE)

#### Arguments

- `with_book_name`:

  Add book_name column (default TRUE)

------------------------------------------------------------------------

### Method `all_transactions()`

Get combined transactions from all books

#### Usage

    BookCollection$all_transactions(with_book_name = TRUE)

#### Arguments

- `with_book_name`:

  Add book_name column (default TRUE)

------------------------------------------------------------------------

### Method `all_splits()`

Get combined splits from all books

#### Usage

    BookCollection$all_splits(with_book_name = TRUE)

#### Arguments

- `with_book_name`:

  Add book_name column (default TRUE)

------------------------------------------------------------------------

### Method `add_ic_rule()`

Add intercompany elimination rule

#### Usage

    BookCollection$add_ic_rule(
      from_book,
      from_account,
      to_book,
      to_account,
      description = NULL
    )

#### Arguments

- `from_book`:

  Source book name

- `from_account`:

  Source account path pattern

- `to_book`:

  Target book name

- `to_account`:

  Target account path pattern

- `description`:

  Rule description

------------------------------------------------------------------------

### Method `get_ic_rules()`

Get all intercompany rules

#### Usage

    BookCollection$get_ic_rules()

------------------------------------------------------------------------

### Method `setup_tinyland_ic_rules()`

Set up default IC rules for Tinyland structure

#### Usage

    BookCollection$setup_tinyland_ic_rules()

------------------------------------------------------------------------

### Method `consolidated_trial_balance()`

Generate consolidated trial balance

#### Usage

    BookCollection$consolidated_trial_balance(
      as_of = Sys.Date(),
      eliminate_ic = TRUE
    )

#### Arguments

- `as_of`:

  Date for balance calculation (default: today)

- `eliminate_ic`:

  Apply intercompany eliminations (default TRUE)

------------------------------------------------------------------------

### Method `consolidated_balance_sheet()`

Generate consolidated balance sheet

#### Usage

    BookCollection$consolidated_balance_sheet(
      as_of = Sys.Date(),
      eliminate_ic = TRUE
    )

#### Arguments

- `as_of`:

  Date for balance calculation

- `eliminate_ic`:

  Apply intercompany eliminations

------------------------------------------------------------------------

### Method `consolidated_income_statement()`

Generate consolidated income statement

#### Usage

    BookCollection$consolidated_income_statement(
      start_date,
      end_date = Sys.Date(),
      eliminate_ic = TRUE
    )

#### Arguments

- `start_date`:

  Start of period

- `end_date`:

  End of period

- `eliminate_ic`:

  Apply intercompany eliminations

------------------------------------------------------------------------

### Method `intercompany_reconciliation()`

Intercompany reconciliation report

#### Usage

    BookCollection$intercompany_reconciliation(as_of = Sys.Date())

#### Arguments

- `as_of`:

  Date for reconciliation

------------------------------------------------------------------------

### Method [`close()`](https://rdrr.io/r/base/connections.html)

Close all books

#### Usage

    BookCollection$close()

------------------------------------------------------------------------

### Method [`print()`](https://rdrr.io/r/base/print.html)

Print method

#### Usage

    BookCollection$print()

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    BookCollection$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
