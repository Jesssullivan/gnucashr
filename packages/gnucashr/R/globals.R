#' @useDynLib gnucashr, .registration = TRUE
#' @importFrom Rcpp evalCpp
#' @importFrom RcppParallel RcppParallelLibs
#' @importFrom utils head tail
#' @importFrom stats median na.omit
#' @importFrom dplyr if_else
#' @importFrom R6 R6Class
#' @importFrom RSQLite SQLite SQLITE_RO SQLITE_RW
NULL

# Suppress R CMD check notes for NSE column names
# These are used with dplyr verbs and rlang

utils::globalVariables(c(
  # Data frame columns
  "guid",
  "name",
  "account",
  "account_type",
  "type",
  "account_guid",
  "parent_guid",
  "tx_guid",
  "post_date",
  "description",
  "value_num",
  "value_denom",
  "balance",
  "debit",
  "credit",
  "full_path",
  "book_name",
  "section",
  "amount",
  "month",
  "total",
  "potential_ic",
  "suggested_counterparty",

  # Write operations
  "mnemonic",
  "age_hours",
  "timestamp",
  "placeholder",
  "hidden",

  # Forecast operations
  "revenue",
  "operating_rate",
  "cogs_rate",
  "growth_rate",
  "entity",
  "projected_revenue",
  "cumulative_revenue",
  "scenario",
  "mean",
  "p50",
  "p5",
  "p25",
  "p75",
  "p95",
  "range",

  # Commodity/Price operations
  "namespace",
  "fullname",
  "cusip",
  "fraction",
  "quote_flag",
  "quote_source",
  "quote_tz",
  "commodity_guid",
  "currency_guid",
  "source",
  "date",

  # Lot operations
  "is_closed",
  "title",
  "notes",
  "quantity",
  "cost_basis",
  "avg_cost",
  "open_date",
  "holding_days",

  # Budget operations
  "budget_guid",
  "period_num",
  "amount_num",
  "amount_denom",
  "num_periods",
  "recurrence_period_type",
  "recurrence_mult",
  "recurrence_start",
  "file_type",
  "template_type",

  # Scheduled transaction operations
  "enabled",
  "auto_create",
  "auto_notify",
  "adv_creation",
  "adv_notify",
  "start_date",
  "end_date",
  "last_occur",
  "num_occur",
  "rem_occur",
  "instance_count",
  "template_act_guid",
  "next_occurrence",

  # data.table NSE (if used)
  ":=",

  # Quarto widget variables
  "period",
  "growth",
  "expense",
  "value",

  # CSV import operations
  "currency",
  "external_id",
  "memo",
  "category",
  "fee",
  "status",

  # OFX import operations
  "transaction_type",
  "is_duplicate",
  "imported_tx_guid"
))
