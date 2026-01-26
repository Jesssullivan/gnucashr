#' Balance Sheet Report
#'
#' Generate balance sheet from trial balance data.
#'
#' @name balance-sheet
NULL

#' Generate Balance Sheet
#'
#' Create a balance sheet from trial balance data, organized by
#' Assets, Liabilities, and Equity sections.
#'
#' @param tb Trial balance tibble (from trial_balance() or consolidated)
#' @return tibble with balance sheet structure
#' @export
balance_sheet <- function(tb) {
  # Map account types to balance sheet sections
  asset_types <- c("ASSET", "BANK", "CASH", "RECEIVABLE", "STOCK", "MUTUAL")
  liability_types <- c("LIABILITY", "PAYABLE", "CREDIT")
  equity_types <- c("EQUITY")

  # Assets section
  assets <- tb |>
    dplyr::filter(type %in% asset_types) |>
    dplyr::mutate(section = "ASSETS") |>
    dplyr::arrange(account)

  # Liabilities section
  liabilities <- tb |>
    dplyr::filter(type %in% liability_types) |>
    dplyr::mutate(
      section = "LIABILITIES",
      balance = -balance  # Liabilities have normal credit balance
    ) |>
    dplyr::arrange(account)

  # Equity section
  equity <- tb |>
    dplyr::filter(type %in% equity_types) |>
    dplyr::mutate(
      section = "EQUITY",
      balance = -balance  # Equity has normal credit balance
    ) |>
    dplyr::arrange(account)

  # Calculate retained earnings from income - expenses
  income_types <- c("INCOME")
  expense_types <- c("EXPENSE")

  total_income <- tb |>
    dplyr::filter(type %in% income_types) |>
    dplyr::summarize(total = sum(balance, na.rm = TRUE)) |>
    dplyr::pull(total)

  total_expenses <- tb |>
    dplyr::filter(type %in% expense_types) |>
    dplyr::summarize(total = sum(balance, na.rm = TRUE)) |>
    dplyr::pull(total)

  net_income <- abs(total_income) - total_expenses  # Income is normally negative (credit)

  # Add retained earnings / current year earnings to equity
  retained <- tibble::tibble(
    account = "Retained Earnings (Current Period)",
    type = "EQUITY",
    debit = 0,
    credit = net_income,
    balance = net_income,
    section = "EQUITY"
  )

  if (net_income > 0.01) {
    equity <- dplyr::bind_rows(equity, retained)
  }

  # Combine sections
  balance_sheet <- dplyr::bind_rows(assets, liabilities, equity) |>
    dplyr::select(section, account, type, balance)

  # Add totals
  total_assets <- sum(assets$balance, na.rm = TRUE)
  total_liabilities <- sum(liabilities$balance, na.rm = TRUE)
  total_equity <- sum(equity$balance, na.rm = TRUE) + max(net_income, 0)

  totals <- tibble::tibble(
    section = c("ASSETS", "LIABILITIES", "EQUITY", "CHECK"),
    account = c("TOTAL ASSETS", "TOTAL LIABILITIES", "TOTAL EQUITY",
                "L+E should equal A"),
    type = "TOTAL",
    balance = c(total_assets, total_liabilities, total_equity,
                total_liabilities + total_equity)
  )

  attr(balance_sheet, "totals") <- totals
  attr(balance_sheet, "is_balanced") <- abs(total_assets - (total_liabilities + total_equity)) < 0.01

  balance_sheet
}

#' Format Balance Sheet for Display
#'
#' Create a formatted balance sheet suitable for printing or reports.
#'
#' @param bs Balance sheet from balance_sheet()
#' @param currency Currency symbol (default "$")
#' @return Character vector of formatted lines
#' @export
format_balance_sheet <- function(bs, currency = "$") {
  totals <- attr(bs, "totals")
  is_balanced <- attr(bs, "is_balanced")

  lines <- character()

  lines <- c(lines, "=" |> rep(60) |> paste(collapse = ""))
  lines <- c(lines, "                    BALANCE SHEET")
  lines <- c(lines, "=" |> rep(60) |> paste(collapse = ""))
  lines <- c(lines, "")

  # Assets
  lines <- c(lines, "ASSETS")
  lines <- c(lines, "-" |> rep(60) |> paste(collapse = ""))
  assets <- bs |> dplyr::filter(section == "ASSETS")
  for (i in seq_len(nrow(assets))) {
    lines <- c(lines, sprintf("  %-40s %s%12.2f",
                              substr(assets$account[i], 1, 40),
                              currency,
                              assets$balance[i]))
  }
  lines <- c(lines, "-" |> rep(60) |> paste(collapse = ""))
  lines <- c(lines, sprintf("  %-40s %s%12.2f", "TOTAL ASSETS", currency,
                            totals$balance[totals$section == "ASSETS"]))
  lines <- c(lines, "")

  # Liabilities
  lines <- c(lines, "LIABILITIES")
  lines <- c(lines, "-" |> rep(60) |> paste(collapse = ""))
  liabilities <- bs |> dplyr::filter(section == "LIABILITIES")
  for (i in seq_len(nrow(liabilities))) {
    lines <- c(lines, sprintf("  %-40s %s%12.2f",
                              substr(liabilities$account[i], 1, 40),
                              currency,
                              liabilities$balance[i]))
  }
  lines <- c(lines, "-" |> rep(60) |> paste(collapse = ""))
  lines <- c(lines, sprintf("  %-40s %s%12.2f", "TOTAL LIABILITIES", currency,
                            totals$balance[totals$section == "LIABILITIES"]))
  lines <- c(lines, "")

  # Equity
  lines <- c(lines, "EQUITY")
  lines <- c(lines, "-" |> rep(60) |> paste(collapse = ""))
  equity <- bs |> dplyr::filter(section == "EQUITY")
  for (i in seq_len(nrow(equity))) {
    lines <- c(lines, sprintf("  %-40s %s%12.2f",
                              substr(equity$account[i], 1, 40),
                              currency,
                              equity$balance[i]))
  }
  lines <- c(lines, "-" |> rep(60) |> paste(collapse = ""))
  lines <- c(lines, sprintf("  %-40s %s%12.2f", "TOTAL EQUITY", currency,
                            totals$balance[totals$section == "EQUITY"]))
  lines <- c(lines, "")

  # Footer
  lines <- c(lines, "=" |> rep(60) |> paste(collapse = ""))
  lines <- c(lines, sprintf("  %-40s %s%12.2f", "LIABILITIES + EQUITY", currency,
                            totals$balance[totals$section == "CHECK"]))

  if (is_balanced) {
    lines <- c(lines, "  STATUS: BALANCED")
  } else {
    lines <- c(lines, "  STATUS: *** OUT OF BALANCE ***")
  }

  lines
}

#' Print Balance Sheet
#'
#' @param bs Balance sheet from balance_sheet()
#' @param ... Additional arguments passed to format_balance_sheet()
#' @export
print_balance_sheet <- function(bs, ...) {
  lines <- format_balance_sheet(bs, ...)
  cat(paste(lines, collapse = "\n"), "\n")
  invisible(bs)
}
