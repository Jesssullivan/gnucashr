#' Consolidation Engine
#'
#' Functions for consolidating multiple GnuCash books into
#' unified financial statements with proper eliminations.
#'
#' @name consolidation
NULL

#' Combine Trial Balances from Multiple Books
#'
#' Merge trial balances from multiple books with book identification.
#'
#' @param books List of GnuCashDB objects (named)
#' @param as_of Date for balance calculation
#' @return Combined tibble with book_name column
#' @export
combine_trial_balances <- function(books, as_of = Sys.Date()) {
  as_of <- as.Date(as_of)

  purrr::imap_dfr(books, function(book, name) {
    trial_balance(book, as_of) |>
      dplyr::mutate(book_name = name, .before = 1)
  })
}

#' Aggregate by Account Type
#'
#' Roll up trial balance to account type level.
#'
#' @param tb Trial balance tibble
#' @return Aggregated tibble by account type
#' @export
aggregate_by_type <- function(tb) {
  tb |>
    dplyr::group_by(type) |>
    dplyr::summarize(
      debit = sum(debit, na.rm = TRUE),
      credit = sum(credit, na.rm = TRUE),
      balance = sum(balance, na.rm = TRUE),
      n_accounts = dplyr::n(),
      .groups = "drop"
    ) |>
    dplyr::arrange(type)
}

#' Calculate Consolidation Adjustments
#'
#' Generate adjustment entries needed for consolidation.
#'
#' @param combined Combined trial balance
#' @param ic_rules Intercompany elimination rules
#' @return tibble of consolidation adjustments
#' @export
calculate_consolidation_adjustments <- function(combined, ic_rules) {
  if (length(ic_rules) == 0) {
    return(tibble::tibble(
      adjustment_type = character(),
      book_name = character(),
      account = character(),
      type = character(),
      debit = numeric(),
      credit = numeric(),
      balance = numeric()
    ))
  }

  # Apply IC eliminations
  eliminations <- purrr::map_dfr(ic_rules, function(rule) {
    from_entries <- combined |>
      dplyr::filter(
        book_name == rule$from_book,
        grepl(rule$from_account, account, fixed = TRUE)
      ) |>
      dplyr::mutate(adjustment_type = "IC_ELIMINATION")

    to_entries <- combined |>
      dplyr::filter(
        book_name == rule$to_book,
        grepl(rule$to_account, account, fixed = TRUE)
      ) |>
      dplyr::mutate(adjustment_type = "IC_ELIMINATION")

    # Create reversing entries
    dplyr::bind_rows(
      from_entries |>
        dplyr::mutate(
          debit = -debit,
          credit = -credit,
          balance = -balance,
          book_name = "CONSOLIDATION"
        ),
      to_entries |>
        dplyr::mutate(
          debit = -debit,
          credit = -credit,
          balance = -balance,
          book_name = "CONSOLIDATION"
        )
    )
  })

  eliminations
}

#' Build Consolidated Trial Balance
#'
#' Create fully consolidated trial balance with all adjustments.
#'
#' @param books List of GnuCashDB objects
#' @param ic_rules Intercompany rules
#' @param as_of Date for balances
#' @return Consolidated trial balance tibble
#' @export
build_consolidated_trial_balance <- function(books, ic_rules = list(), as_of = Sys.Date()) {
  as_of <- as.Date(as_of)

  # Get combined trial balances
  combined <- combine_trial_balances(books, as_of)

  # Calculate adjustments
  adjustments <- calculate_consolidation_adjustments(combined, ic_rules)

  # Combine and aggregate
  dplyr::bind_rows(combined, adjustments) |>
    dplyr::group_by(account, type) |>
    dplyr::summarize(
      debit = sum(debit, na.rm = TRUE),
      credit = sum(credit, na.rm = TRUE),
      balance = sum(balance, na.rm = TRUE),
      source_books = paste(unique(book_name), collapse = ", "),
      .groups = "drop"
    ) |>
    dplyr::filter(abs(balance) > 0.01) |>
    dplyr::arrange(type, account)
}

#' Validate Consolidation
#'
#' Check that consolidated trial balance is in balance.
#'
#' @param consolidated_tb Consolidated trial balance
#' @return List with validation results
#' @export
validate_consolidation <- function(consolidated_tb) {
  total_debits <- sum(consolidated_tb$debit, na.rm = TRUE)
  total_credits <- sum(consolidated_tb$credit, na.rm = TRUE)
  difference <- abs(total_debits - total_credits)

  list(
    total_debits = total_debits,
    total_credits = total_credits,
    difference = difference,
    is_balanced = difference < 0.01,
    summary = if (difference < 0.01) {
      "Consolidated trial balance is in balance"
    } else {
      sprintf("OUT OF BALANCE: Difference of %.2f", difference)
    }
  )
}

#' Consolidation Summary Report
#'
#' Generate a summary of the consolidation process.
#'
#' @param books List of GnuCashDB objects
#' @param ic_rules Intercompany rules
#' @param as_of Date for balances
#' @return List with consolidation details
#' @export
consolidation_summary <- function(books, ic_rules = list(), as_of = Sys.Date()) {
  as_of <- as.Date(as_of)

  # Get individual trial balances
  individual_tbs <- purrr::imap(books, function(book, name) {
    tb <- trial_balance(book, as_of)
    list(
      book = name,
      total_assets = sum(tb$balance[tb$type %in% c("ASSET", "BANK", "CASH", "RECEIVABLE")], na.rm = TRUE),
      total_liabilities = sum(tb$balance[tb$type %in% c("LIABILITY", "PAYABLE", "CREDIT")], na.rm = TRUE),
      total_equity = sum(tb$balance[tb$type == "EQUITY"], na.rm = TRUE),
      total_income = sum(tb$balance[tb$type == "INCOME"], na.rm = TRUE),
      total_expenses = sum(tb$balance[tb$type == "EXPENSE"], na.rm = TRUE)
    )
  })

  # Get consolidated
  consolidated_tb <- build_consolidated_trial_balance(books, ic_rules, as_of)
  validation <- validate_consolidation(consolidated_tb)

  # IC elimination totals
  combined <- combine_trial_balances(books, as_of)
  adjustments <- calculate_consolidation_adjustments(combined, ic_rules)
  ic_elimination_total <- sum(abs(adjustments$balance), na.rm = TRUE) / 2  # Divide by 2 as both sides

  list(
    as_of = as_of,
    books = individual_tbs,
    consolidated = list(
      total_assets = sum(consolidated_tb$balance[consolidated_tb$type %in% c("ASSET", "BANK", "CASH", "RECEIVABLE")], na.rm = TRUE),
      total_liabilities = sum(consolidated_tb$balance[consolidated_tb$type %in% c("LIABILITY", "PAYABLE", "CREDIT")], na.rm = TRUE),
      total_equity = sum(consolidated_tb$balance[consolidated_tb$type == "EQUITY"], na.rm = TRUE),
      total_income = sum(consolidated_tb$balance[consolidated_tb$type == "INCOME"], na.rm = TRUE),
      total_expenses = sum(consolidated_tb$balance[consolidated_tb$type == "EXPENSE"], na.rm = TRUE)
    ),
    ic_eliminations = ic_elimination_total,
    validation = validation
  )
}
