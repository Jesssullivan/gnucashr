# Identify Uneliminated IC Transactions

Find intercompany transactions that should have been eliminated but
weren't matched by existing rules.

## Usage

``` r
identify_uneliminated_ic(
  collection,
  ic_keywords = c("due from", "due to", "intercompany", "affiliate", "management fee",
    "license fee", "inter-co")
)
```

## Arguments

- collection:

  BookCollection object

- ic_keywords:

  Keywords that indicate intercompany accounts

## Value

tibble of potentially uneliminated IC transactions
