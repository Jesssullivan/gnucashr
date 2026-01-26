# Create Standard IC Rule Set

Generate standard intercompany elimination rules for a parent company
with multiple subsidiaries.

## Usage

``` r
create_standard_ic_rules(
  parent_name = "inc",
  subsidiary_names = c("products", "services", "software", "operations"),
  parent_ar_pattern = "Assets:Due from %s",
  sub_ap_pattern = "Liabilities:Due to Parent",
  mgmt_fee_income = "Income:Management Fees:%s",
  mgmt_fee_expense = "Expenses:Management Fees"
)
```

## Arguments

- parent_name:

  Name of parent company book

- subsidiary_names:

  Character vector of subsidiary book names

- parent_ar_pattern:

  Pattern for parent's receivable accounts

- sub_ap_pattern:

  Pattern for subsidiary's payable accounts

- mgmt_fee_income:

  Pattern for management fee income

- mgmt_fee_expense:

  Pattern for management fee expense

## Value

List of IC rules
