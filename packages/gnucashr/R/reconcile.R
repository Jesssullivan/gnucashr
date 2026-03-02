# reconcile.R - Account reconciliation operations
#
# Provides functions for reconciling accounts against bank statements
# and finding cross-institution transfer matches.

#' Reconcile Account
#'
#' Mark unreconciled splits as cleared up to the statement date and
#' compute the balance difference. Requires a read-write book handle.
#'
#' @param book_ptr External pointer from \code{gc_open(read_only = FALSE)}
#' @param account_guid Account GUID to reconcile
#' @param statement_date Statement date (YYYY-MM-DD format)
#' @param statement_balance Expected balance from the bank statement
#' @return Named list with:
#'   \describe{
#'     \item{splits_reconciled}{Number of splits marked as cleared}
#'     \item{statement_balance}{Expected balance}
#'     \item{book_balance}{Actual book balance}
#'     \item{difference}{statement - book balance}
#'     \item{balanced}{TRUE if difference is zero}
#'   }
#' @export
reconcile_account <- function(book_ptr, account_guid, statement_date,
                              statement_balance) {
  gc_reconcile_account(book_ptr, account_guid, statement_date,
                       as.double(statement_balance))
}

#' Find Cross-Institution Transfer Matches
#'
#' Identify potential transfers between two accounts by matching
#' transactions with inverted amounts, similar dates, and
#' similar descriptions.
#'
#' @param book_ptr External pointer from \code{gc_open()}
#' @param account_a_guid First account GUID
#' @param account_b_guid Second account GUID
#' @param from_date Start date (YYYY-MM-DD)
#' @param to_date End date (YYYY-MM-DD)
#' @param date_window Maximum days between matching transactions (default 3)
#' @param min_similarity Minimum match confidence 0-1 (default 0.5)
#' @return Data frame with columns: split_a_guid, split_b_guid,
#'   tx_a_guid, tx_b_guid, amount, date_a, date_b, desc_a, desc_b, similarity
#' @export
find_transfer_matches <- function(book_ptr, account_a_guid, account_b_guid,
                                  from_date, to_date,
                                  date_window = 3L, min_similarity = 0.5) {
  gc_find_transfer_matches(book_ptr, account_a_guid, account_b_guid,
                           from_date, to_date,
                           as.integer(date_window), as.double(min_similarity))
}
