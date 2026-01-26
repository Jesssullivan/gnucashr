# Importing Data from Payment Platforms

## Introduction

gnucashr provides standardized importers for common payment platforms
and accounting systems. These importers convert platform-specific CSV
exports into a common format that can be easily analyzed or imported
into GnuCash.

Currently supported platforms:

- **PayPal** - Activity download exports
- **Stripe** - Balance transaction exports
- **QuickBooks** - Online (QBO) and Desktop CSV exports

## Quick Start

``` r
library(gnucashr)

# Import from PayPal
paypal_data <- import_paypal_csv("paypal_activity.csv")

# Import from Stripe
stripe_data <- import_stripe_csv("stripe_balance.csv")

# Import from QuickBooks
qb_data <- import_quickbooks_csv("quickbooks_export.csv")

# Combine multiple sources
all_transactions <- combine_csv_imports(paypal_data, stripe_data, qb_data)
```

## Standard Output Format

All importers return a tibble with the `gnucashr_csv_import` class
containing these standardized columns:

| Column        | Type      | Description                                              |
|---------------|-----------|----------------------------------------------------------|
| `date`        | Date      | Transaction date                                         |
| `description` | character | Transaction description or payee name                    |
| `amount`      | numeric   | Transaction amount (positive = credit, negative = debit) |
| `currency`    | character | ISO 4217 currency code (e.g., “USD”, “EUR”)              |
| `external_id` | character | Unique ID from the source system                         |
| `account`     | character | Source account name (e.g., “PayPal”, “Stripe”)           |
| `memo`        | character | Additional notes or memo text                            |
| `category`    | character | Transaction category (payment, refund, fee, etc.)        |

Some importers include additional columns like `fee` for transaction
fees.

## PayPal Imports

### Exporting from PayPal

1.  Log in to PayPal Business or Personal account
2.  Go to Activity \> All Transactions
3.  Click “Download” and select CSV format
4.  Choose your date range

### Column Mapping

PayPal exports vary slightly depending on account type and region.
gnucashr auto-detects the format and maps these columns:

| PayPal Column  | gnucashr Column |
|----------------|-----------------|
| Date + Time    | `date`          |
| Name           | `description`   |
| Gross          | `amount`        |
| Currency       | `currency`      |
| Transaction ID | `external_id`   |
| Note           | `memo`          |
| Type           | `category`      |
| Fee            | `fee`           |

### Usage

``` r
# Basic import
paypal <- import_paypal_csv("paypal_activity.csv")

# Include pending transactions (excluded by default)
paypal_all <- import_paypal_csv("paypal_activity.csv", include_pending = TRUE)

# Specify timezone for date conversion
paypal_pst <- import_paypal_csv("paypal_activity.csv", timezone = "America/Los_Angeles")

# View results
print(paypal)
head(paypal)
```

### PayPal Categories

The `category` column classifies transactions:

- `payment` - Received payments
- `refund` - Refunds issued
- `fee` - PayPal fees
- `transfer` - Bank transfers or withdrawals
- `conversion` - Currency conversions
- `hold` - Temporary holds
- `reversal` - Payment reversals
- `other` - Other transaction types

## Stripe Imports

### Exporting from Stripe

1.  Log in to Stripe Dashboard
2.  Go to Balance \> Balance history
3.  Click “Export” and select CSV
4.  Choose columns and date range

### Column Mapping

| Stripe Column | gnucashr Column |
|---------------|-----------------|
| Created (UTC) | `date`          |
| Description   | `description`   |
| Amount        | `amount`        |
| Currency      | `currency`      |
| id            | `external_id`   |
| Customer      | `memo`          |
| Type          | `category`      |
| Fee           | `fee`           |

### Usage

``` r
# Basic import
stripe <- import_stripe_csv("stripe_balance.csv")

# Include fee breakdown (default is TRUE)
stripe <- import_stripe_csv("stripe_balance.csv", include_fees = TRUE)

# Specify timezone
stripe <- import_stripe_csv("stripe_balance.csv", timezone = "UTC")

print(stripe)
```

### Stripe Amount Handling

Stripe exports amounts in the smallest currency unit (cents for USD).
The importer automatically detects this and converts to standard decimal
format:

- `10000` (cents) becomes `100.00` (dollars)
- `-2500` (cents) becomes `-25.00` (dollars)

### Stripe Categories

- `charge` - Customer charges/payments
- `refund` - Refunds to customers
- `payout` - Payouts to bank account
- `fee` - Stripe fees
- `adjustment` - Balance adjustments
- `dispute` - Chargebacks and disputes
- `other` - Other transaction types

## QuickBooks Imports

### Exporting from QuickBooks

**QuickBooks Online:** 1. Go to Reports \> Transaction List by Date 2.
Customize date range and columns 3. Export to CSV

**QuickBooks Desktop:** 1. Go to Reports \> Accountant & Taxes \>
Transaction List by Date 2. Set date range 3. Export to Excel/CSV

### Format Auto-Detection

gnucashr automatically detects whether the export is from:

- **QBO format** - QuickBooks Online (has “Transaction Type” column)
- **Desktop format** - QuickBooks Desktop (has separate Debit/Credit
  columns)

``` r
# Auto-detect format
qb <- import_quickbooks_csv("qb_export.csv")

# Force specific format
qb_online <- import_quickbooks_csv("qb_export.csv", format = "qbo")
qb_desktop <- import_quickbooks_csv("qb_export.csv", format = "desktop")

print(qb)
```

### QuickBooks Categories

- `invoice` - Customer invoices
- `payment` - Received payments
- `expense` - Bills and expenses
- `check` - Check payments
- `transfer` - Account transfers
- `journal` - Journal entries
- `deposit` - Bank deposits
- `refund` - Customer refunds
- `sale` - Sales transactions
- `other` - Other transaction types

## Combining Multiple Imports

Use
[`combine_csv_imports()`](https://tinyland.gitlab.io/projects/gnucashr/reference/combine_csv_imports.md)
to merge transactions from multiple sources:

``` r
# Import from multiple sources
paypal <- import_paypal_csv("paypal.csv")
stripe <- import_stripe_csv("stripe.csv")
qb <- import_quickbooks_csv("quickbooks.csv")

# Combine all
all_data <- combine_csv_imports(paypal, stripe, qb)

# With duplicate detection
all_data <- combine_csv_imports(
  paypal, stripe, qb,
  deduplicate = TRUE,
  dedupe_key = c("date", "amount", "description")
)

# View combined data
print(all_data)
nrow(all_data)
```

### Handling Duplicates

When importing from multiple sources, you may have duplicate
transactions (e.g., a PayPal payment that also appears in QuickBooks).
Use the `deduplicate` option:

``` r
# Deduplicate by date, amount, and description
combined <- combine_csv_imports(
  paypal, stripe,
  deduplicate = TRUE,
  dedupe_key = c("date", "amount", "description")
)

# Custom deduplication key
combined <- combine_csv_imports(
  paypal, stripe,
  deduplicate = TRUE,
  dedupe_key = c("date", "amount")  # Less strict matching
)
```

## Working with Multiple Currencies

Imports preserve currency information from the source:

``` r
# Check currencies in import
unique(paypal$currency)

# Filter by currency
usd_only <- paypal |>
  dplyr::filter(currency == "USD")

# Group by currency
paypal |>
  dplyr::group_by(currency) |>
  dplyr::summarise(
    count = dplyr::n(),
    total = sum(amount)
  )
```

## Validation and Debugging

### Validating Imports

Use
[`validate_csv_import()`](https://tinyland.gitlab.io/projects/gnucashr/reference/validate_csv_import.md)
to check for common issues:

``` r
# Check for issues
validation <- validate_csv_import(paypal)

if (!validation$valid) {
  print(validation$issues)
}

# Strict mode throws error on issues
validate_csv_import(paypal, strict = TRUE)
```

### Previewing Column Mapping

Use
[`preview_csv_mapping()`](https://tinyland.gitlab.io/projects/gnucashr/reference/preview_csv_mapping.md)
to debug import issues:

``` r
# See how columns will be mapped before importing
preview_csv_mapping("paypal.csv", import_paypal_csv)
```

This shows: - Detected format - Column names in the file - How each
column maps to the standard format - Sample values from first few rows

## Filtering and Analysis

Once imported, use standard dplyr operations:

``` r
library(dplyr)

# Filter by date range
recent <- paypal |>
  filter(date >= as.Date("2024-01-01"))

# Summarize by category
paypal |>
  group_by(category) |>
  summarise(
    count = n(),
    total = sum(amount),
    avg = mean(amount)
  )

# Monthly totals
paypal |>
  mutate(month = lubridate::floor_date(date, "month")) |>
  group_by(month) |>
  summarise(total = sum(amount))

# Find large transactions
paypal |>
  filter(abs(amount) > 1000) |>
  arrange(desc(abs(amount)))
```

## Next Steps

After importing data, you can:

- Create GnuCash transactions using `create_transaction()`
- Generate financial reports with
  [`trial_balance()`](https://tinyland.gitlab.io/projects/gnucashr/reference/trial_balance.md)
  and
  [`income_statement()`](https://tinyland.gitlab.io/projects/gnucashr/reference/income_statement.md)
- Perform cash flow analysis with the forecasting tools

See the [Getting
Started](https://tinyland.gitlab.io/projects/gnucashr/articles/getting-started.md)
vignette for more on working with GnuCash data in R.
