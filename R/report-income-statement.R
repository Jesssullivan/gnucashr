#' Income Statement Report
#'
#' Generate income statement (profit and loss) from accounting data.
#'
#' @name income-statement
NULL

#' Get Income Statement Activity
#'
#' Extract income and expense activity for a specific period.
#'
#' @param gc GnuCashDB object
#' @param start_date Start of period
#' @param end_date End of period
#' @return tibble with income/expense activity
#' @export
income_statement_activity <- function(gc, start_date, end_date = Sys.Date()) {
  start_date <- as.Date(start_date)
  end_date <- as.Date(end_date)

  # Get splits within the date range for income/expense accounts
  splits <- gc$splits(collected = TRUE)
  transactions <- gc$transactions(collected = TRUE)
  accounts <- gc$accounts(collected = TRUE)

  # Join and filter
  activity <- splits |>
    dplyr::inner_join(
      transactions |> dplyr::select(guid, post_date, description),
      by = c("tx_guid" = "guid")
    ) |>
    dplyr::inner_join(
      accounts |> dplyr::select(guid, name, account_type),
      by = c("account_guid" = "guid")
    ) |>
    dplyr::filter(
      account_type %in% c("INCOME", "EXPENSE"),
      post_date >= start_date,
      post_date <= end_date
    ) |>
    dplyr::group_by(name, account_type) |>
    dplyr::summarize(
      balance = sum(value_num / value_denom, na.rm = TRUE),
      n_transactions = dplyr::n(),
      .groups = "drop"
    ) |>
    dplyr::rename(account = name, type = account_type) |>
    dplyr::mutate(
      debit = dplyr::if_else(balance > 0, balance, 0),
      credit = dplyr::if_else(balance < 0, abs(balance), 0)
    )

  activity
}

#' Generate Income Statement
#'
#' Create income statement from activity data or trial balance.
#'
#' @param data Activity tibble from income_statement_activity() or trial_balance()
#' @return tibble with income statement structure
#' @export
income_statement <- function(data) {
  # Filter to income and expense accounts
  income <- data |>
    dplyr::filter(type == "INCOME") |>
    dplyr::mutate(
      section = "REVENUE",
      # Income is normally credit (negative in our convention)
      amount = abs(balance)
    ) |>
    dplyr::arrange(account)

  expenses <- data |>
    dplyr::filter(type == "EXPENSE") |>
    dplyr::mutate(
      section = "EXPENSES",
      amount = balance  # Expenses are debit (positive)
    ) |>
    dplyr::arrange(account)

  # Calculate totals
  total_revenue <- sum(income$amount, na.rm = TRUE)
  total_expenses <- sum(expenses$amount, na.rm = TRUE)
  net_income <- total_revenue - total_expenses

  # Build statement
  statement <- dplyr::bind_rows(income, expenses) |>
    dplyr::select(section, account, type, amount)

  attr(statement, "total_revenue") <- total_revenue
  attr(statement, "total_expenses") <- total_expenses
  attr(statement, "net_income") <- net_income
  attr(statement, "gross_margin") <- if (total_revenue > 0) {
    (total_revenue - total_expenses) / total_revenue
  } else {
    NA_real_
  }

  statement
}

#' Format Income Statement for Display
#'
#' Create a formatted income statement suitable for printing.
#'
#' @param is Income statement from income_statement()
#' @param currency Currency symbol (default "$")
#' @return Character vector of formatted lines
#' @export
format_income_statement <- function(is, currency = "$") {
  total_revenue <- attr(is, "total_revenue")
  total_expenses <- attr(is, "total_expenses")
  net_income <- attr(is, "net_income")
  gross_margin <- attr(is, "gross_margin")

  lines <- character()

  lines <- c(lines, "=" |> rep(60) |> paste(collapse = ""))
  lines <- c(lines, "                  INCOME STATEMENT")
  lines <- c(lines, "=" |> rep(60) |> paste(collapse = ""))
  lines <- c(lines, "")

  # Revenue
  lines <- c(lines, "REVENUE")
  lines <- c(lines, "-" |> rep(60) |> paste(collapse = ""))
  revenue <- is |> dplyr::filter(section == "REVENUE")
  for (i in seq_len(nrow(revenue))) {
    lines <- c(lines, sprintf("  %-40s %s%12.2f",
                              substr(revenue$account[i], 1, 40),
                              currency,
                              revenue$amount[i]))
  }
  lines <- c(lines, "-" |> rep(60) |> paste(collapse = ""))
  lines <- c(lines, sprintf("  %-40s %s%12.2f", "TOTAL REVENUE", currency, total_revenue))
  lines <- c(lines, "")

  # Expenses
  lines <- c(lines, "EXPENSES")
  lines <- c(lines, "-" |> rep(60) |> paste(collapse = ""))
  expenses <- is |> dplyr::filter(section == "EXPENSES")
  for (i in seq_len(nrow(expenses))) {
    lines <- c(lines, sprintf("  %-40s %s%12.2f",
                              substr(expenses$account[i], 1, 40),
                              currency,
                              expenses$amount[i]))
  }
  lines <- c(lines, "-" |> rep(60) |> paste(collapse = ""))
  lines <- c(lines, sprintf("  %-40s %s%12.2f", "TOTAL EXPENSES", currency, total_expenses))
  lines <- c(lines, "")

  # Net Income
  lines <- c(lines, "=" |> rep(60) |> paste(collapse = ""))
  lines <- c(lines, sprintf("  %-40s %s%12.2f", "NET INCOME", currency, net_income))

  if (!is.na(gross_margin)) {
    lines <- c(lines, sprintf("  %-40s %11.1f%%", "GROSS MARGIN", gross_margin * 100))
  }

  lines
}

#' Print Income Statement
#'
#' @param is Income statement from income_statement()
#' @param ... Additional arguments passed to format_income_statement()
#' @export
print_income_statement <- function(is, ...) {
  lines <- format_income_statement(is, ...)
  cat(paste(lines, collapse = "\n"), "\n")
  invisible(is)
}

#' Compare Income Statements
#'
#' Compare income statements across multiple periods or entities.
#'
#' @param ... Named income statements to compare
#' @return tibble with side-by-side comparison
#' @export
compare_income_statements <- function(...) {
  statements <- list(...)

  # Get all unique accounts
  all_accounts <- purrr::map(statements, ~unique(.$account)) |>
    purrr::reduce(union)

  # Build comparison
  purrr::imap_dfc(statements, function(is, name) {
    # Match accounts
    values <- purrr::map_dbl(all_accounts, function(acct) {
      row <- is |> dplyr::filter(account == acct)
      if (nrow(row) > 0) row$amount[1] else 0
    })

    tibble::tibble(!!name := values)
  }) |>
    dplyr::mutate(account = all_accounts, .before = 1)
}
