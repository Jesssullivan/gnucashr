#' Financial Reports
#'
#' Core financial reporting functions for GnuCash data.
#'
#' @name reports
NULL

#' Generate Trial Balance
#'
#' Calculate trial balance as of a specific date.
#'
#' @param gc GnuCashDB object
#' @param as_of Date for balance calculation (default: today)
#' @return tibble with account, type, debit, credit, balance columns
#' @export
trial_balance <- function(gc, as_of = Sys.Date()) {
  as_of <- as.Date(as_of)

  # Get data (works for both SQLite and XML)
  splits <- gc$splits(collected = TRUE)
  transactions <- gc$transactions(collected = TRUE)
  accounts <- gc$accounts(collected = TRUE)

  # Filter transactions by date and join
  splits |>
    dplyr::inner_join(
      transactions |> dplyr::select(guid, post_date),
      by = c("tx_guid" = "guid")
    ) |>
    dplyr::filter(post_date <= as_of) |>
    dplyr::inner_join(
      accounts |> dplyr::select(guid, name, account_type),
      by = c("account_guid" = "guid")
    ) |>
    dplyr::group_by(account_guid, name, account_type) |>
    dplyr::summarize(
      balance = sum(value_num / value_denom, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      debit = dplyr::if_else(balance > 0, balance, 0),
      credit = dplyr::if_else(balance < 0, abs(balance), 0)
    ) |>
    dplyr::select(account = name, type = account_type, debit, credit, balance) |>
    dplyr::arrange(type, account)
}


#' Get Account Transactions
#'
#' Retrieve transactions for a specific account.
#'
#' @param gc GnuCashDB object
#' @param account Account name or path
#' @param start Start date (optional)
#' @param end End date (optional)
#' @return tibble of transactions
#' @export
account_transactions <- function(gc, account, start = NULL, end = NULL) {
  splits <- gc$splits(collected = TRUE)
  transactions <- gc$transactions(collected = TRUE)
  accounts <- gc$accounts(collected = TRUE)

  # Find the account
  target_accounts <- accounts |>
    dplyr::filter(name == account | guid == account)

  if (nrow(target_accounts) == 0) {
    # Try path match via account_tree
    tree <- gc$account_tree()
    target_accounts <- tree |>
      dplyr::filter(full_path == account | grepl(account, full_path, fixed = TRUE))
  }

  if (nrow(target_accounts) == 0) {
    rlang::warn(paste("Account not found:", account))
    return(tibble::tibble(
      date = as.Date(character()),
      description = character(),
      amount = numeric()
    ))
  }

  target_guids <- target_accounts$guid

  query <- splits |>
    dplyr::filter(account_guid %in% target_guids) |>
    dplyr::inner_join(
      transactions |> dplyr::select(guid, post_date, description),
      by = c("tx_guid" = "guid")
    )

  if (!is.null(start)) {
    query <- query |> dplyr::filter(post_date >= as.Date(start))
  }

  if (!is.null(end)) {
    query <- query |> dplyr::filter(post_date <= as.Date(end))
  }

  query |>
    dplyr::mutate(
      date = as.Date(post_date),
      amount = value_num / value_denom
    ) |>
    dplyr::select(date, description, amount) |>
    dplyr::arrange(date)
}


#' Get Account Tree
#'
#' Build hierarchical account structure with full paths.
#'
#' @param gc GnuCashDB object
#' @return tibble with account hierarchy
#' @export
account_tree <- function(gc) {
  gc$account_tree()
}


#' Account Balance
#'
#' Get the balance of a specific account as of a date.
#'
#' @param gc GnuCashDB object
#' @param account Account name, path, or GUID
#' @param as_of Date for balance calculation
#' @return Numeric balance
#' @export
account_balance <- function(gc, account, as_of = Sys.Date()) {
  as_of <- as.Date(as_of)

  # Find account
  acct <- gc$get_account(account)
  if (is.null(acct)) {
    rlang::warn(paste("Account not found:", account))
    return(NA_real_)
  }

  splits <- gc$splits(collected = TRUE)
  transactions <- gc$transactions(collected = TRUE)

  balance <- splits |>
    dplyr::filter(account_guid == acct$guid) |>
    dplyr::inner_join(
      transactions |> dplyr::select(guid, post_date),
      by = c("tx_guid" = "guid")
    ) |>
    dplyr::filter(post_date <= as_of) |>
    dplyr::summarize(balance = sum(value_num / value_denom, na.rm = TRUE)) |>
    dplyr::pull(balance)

  balance
}


#' Account Balances for Multiple Accounts
#'
#' Get balances for multiple accounts efficiently.
#'
#' @param gc GnuCashDB object
#' @param accounts Character vector of account names/paths/GUIDs
#' @param as_of Date for balance calculation
#' @return Named numeric vector of balances
#' @export
account_balances <- function(gc, accounts, as_of = Sys.Date()) {
  purrr::set_names(
    purrr::map_dbl(accounts, ~account_balance(gc, .x, as_of)),
    accounts
  )
}


#' Monthly Activity Summary
#'
#' Summarize account activity by month.
#'
#' @param gc GnuCashDB object
#' @param account Account name or path (NULL for all accounts)
#' @param start Start date
#' @param end End date
#' @return tibble with monthly summaries
#' @export
monthly_activity <- function(gc, account = NULL, start = NULL, end = NULL) {
  splits <- gc$splits(collected = TRUE)
  transactions <- gc$transactions(collected = TRUE)
  accounts <- gc$accounts(collected = TRUE)

  data <- splits |>
    dplyr::inner_join(
      transactions |> dplyr::select(guid, post_date),
      by = c("tx_guid" = "guid")
    ) |>
    dplyr::inner_join(
      accounts |> dplyr::select(guid, name, account_type),
      by = c("account_guid" = "guid")
    )

  if (!is.null(account)) {
    target <- gc$get_account(account)
    if (!is.null(target)) {
      data <- data |> dplyr::filter(account_guid == target$guid)
    }
  }

  if (!is.null(start)) {
    data <- data |> dplyr::filter(post_date >= as.Date(start))
  }

  if (!is.null(end)) {
    data <- data |> dplyr::filter(post_date <= as.Date(end))
  }

  data |>
    dplyr::mutate(
      month = lubridate::floor_date(as.Date(post_date), "month"),
      amount = value_num / value_denom
    ) |>
    dplyr::group_by(month, name, account_type) |>
    dplyr::summarize(
      total = sum(amount, na.rm = TRUE),
      n_transactions = dplyr::n(),
      .groups = "drop"
    ) |>
    dplyr::arrange(month, name)
}
