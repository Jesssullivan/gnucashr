# plaid-map.R - Internal functions mapping Plaid transactions to gnucashr format
#
# Plaid transactions are mapped to the same tibble format used by import_ofx()
# so they can flow through the existing bank feed import pipeline.

#' Map Plaid Transactions to OFX-Compatible Format
#'
#' Converts Plaid transaction records (as returned by `/transactions/sync`)
#' to the standard tibble format expected by \code{validate_ofx_import()} and
#' \code{import_ofx_to_gnucash()}.
#'
#' @param plaid_txns List of Plaid transaction objects (each a named list).
#' @param default_currency Fallback currency code (default "USD").
#' @return A tibble matching \code{import_ofx()} output: date, description,
#'   amount, currency, external_id, memo, transaction_type.
#' @keywords internal
.plaid_to_ofx_transactions <- function(plaid_txns, default_currency = "USD") {
  if (length(plaid_txns) == 0) {
    return(tibble::tibble(
      date = as.Date(character()),
      description = character(),
      amount = numeric(),
      currency = character(),
      external_id = character(),
      memo = character(),
      transaction_type = character()
    ))
  }

  tibble::tibble(
    date = as.Date(vapply(plaid_txns, function(tx) {
      tx$date %||% NA_character_
    }, character(1))),

    description = vapply(plaid_txns, function(tx) {
      # Prefer merchant_name, fall back to name
      tx$merchant_name %||% tx$name %||% NA_character_
    }, character(1)),

    # Plaid: positive = debit (money out); OFX/gnucashr: positive = credit
    amount = -vapply(plaid_txns, function(tx) {
      tx$amount %||% 0
    }, numeric(1)),

    currency = vapply(plaid_txns, function(tx) {
      tx$iso_currency_code %||% default_currency
    }, character(1)),

    external_id = vapply(plaid_txns, function(tx) {
      .plaid_fitid(tx$transaction_id)
    }, character(1)),

    memo = vapply(plaid_txns, function(tx) {
      # Combine name + category for context
      parts <- c()
      if (!is.null(tx$name) && tx$name != (tx$merchant_name %||% "")) {
        parts <- c(parts, tx$name)
      }
      cat_hint <- .plaid_category_hint(tx$personal_finance_category)
      if (!is.null(cat_hint)) parts <- c(parts, paste0("[", cat_hint, "]"))
      if (length(parts) > 0) paste(parts, collapse = " ") else NA_character_
    }, character(1)),

    transaction_type = vapply(plaid_txns, function(tx) {
      # Map Plaid payment_channel to OFX-like type
      channel <- tx$payment_channel %||% "other"
      switch(channel,
        "online" = "DIRECTDEBIT",
        "in store" = "POS",
        "other" = "OTHER",
        "OTHER"
      )
    }, character(1))
  )
}


#' Generate FITID from Plaid Transaction ID
#'
#' Creates a deterministic FITID string from a Plaid transaction_id.
#' Prefixed with "plaid:" to distinguish from bank-native FITIDs.
#'
#' @param transaction_id Plaid transaction ID string.
#' @return FITID string for dedup via \code{find_split_by_fitid()}.
#' @keywords internal
.plaid_fitid <- function(transaction_id) {
  if (is.null(transaction_id) || is.na(transaction_id) || transaction_id == "") {
    return(NA_character_)
  }
  paste0("plaid:", transaction_id)
}


#' Map Plaid Category to Account Hint
#'
#' Maps Plaid Personal Finance Category v2 (PFCv2) to a GnuCash account
#' path hint that can guide the transaction categorizer agent.
#'
#' @param pfc Named list with \code{primary} and optionally \code{detailed}
#'   fields from Plaid's \code{personal_finance_category}.
#' @return Account path hint string, or NULL if unmappable.
#' @keywords internal
.plaid_category_hint <- function(pfc) {
  if (is.null(pfc)) return(NULL)

  primary <- pfc$primary %||% pfc$PRIMARY %||% NULL
  if (is.null(primary)) return(NULL)

  # Map PFCv2 primary categories to GnuCash account tree hints
  hint <- switch(toupper(primary),
    "INCOME" = "Income",
    "TRANSFER_IN" = "Income:Transfer",
    "TRANSFER_OUT" = "Expenses:Transfer",
    "LOAN_PAYMENTS" = "Expenses:Loan Payments",
    "BANK_FEES" = "Expenses:Bank Charges",
    "ENTERTAINMENT" = "Expenses:Entertainment",
    "FOOD_AND_DRINK" = "Expenses:Dining",
    "GENERAL_MERCHANDISE" = "Expenses:Merchandise",
    "HOME_IMPROVEMENT" = "Expenses:Home Improvement",
    "MEDICAL" = "Expenses:Medical",
    "PERSONAL_CARE" = "Expenses:Personal Care",
    "GENERAL_SERVICES" = "Expenses:Services",
    "GOVERNMENT_AND_NON_PROFIT" = "Expenses:Taxes",
    "TRANSPORTATION" = "Expenses:Transportation",
    "TRAVEL" = "Expenses:Travel",
    "RENT_AND_UTILITIES" = "Expenses:Utilities",
    NULL
  )

  # Append detailed category if available
  detailed <- pfc$detailed %||% pfc$DETAILED %||% NULL
  if (!is.null(hint) && !is.null(detailed)) {
    hint <- paste0(hint, " [", tolower(detailed), "]")
  }

  hint
}


#' Generate Minimal OFX Content from Plaid Transactions
#'
#' Synthesizes OFX 1.x SGML content that can be passed through
#' \code{gc_import_ofx_feed()} for import + FITID dedup.
#'
#' @param txn_tibble Tibble from \code{.plaid_to_ofx_transactions()}.
#' @return OFX content string.
#' @keywords internal
.plaid_to_ofx_content <- function(txn_tibble) {
  if (nrow(txn_tibble) == 0) return("")

  currency <- txn_tibble$currency[1]

  txn_lines <- vapply(seq_len(nrow(txn_tibble)), function(i) {
    row <- txn_tibble[i, ]
    # OFX date format: YYYYMMDD
    dt <- format(row$date, "%Y%m%d")
    # Determine transaction type
    trntype <- if (!is.na(row$transaction_type)) row$transaction_type else "OTHER"
    # Format amount with 2 decimal places
    amt <- sprintf("%.2f", row$amount)
    # FITID
    fitid <- if (!is.na(row$external_id)) row$external_id else ""
    # Name/description
    name <- if (!is.na(row$description)) row$description else ""
    # Memo
    memo_str <- if (!is.na(row$memo)) {
      paste0("<MEMO>", .ofx_escape(row$memo))
    } else {
      ""
    }

    paste0(
      "<STMTTRN>",
      "<TRNTYPE>", trntype,
      "<DTPOSTED>", dt,
      "<TRNAMT>", amt,
      "<FITID>", fitid,
      "<NAME>", .ofx_escape(name),
      memo_str,
      "</STMTTRN>"
    )
  }, character(1))

  paste0(
    "OFXHEADER:100\n",
    "DATA:OFXSGML\n",
    "VERSION:102\n\n",
    "<OFX><BANKMSGSRSV1><STMTTRNRS><STMTRS>",
    "<CURDEF>", currency,
    "<BANKTRANLIST>",
    paste(txn_lines, collapse = "\n"),
    "</BANKTRANLIST>",
    "</STMTRS></STMTTRNRS></BANKMSGSRSV1></OFX>"
  )
}


#' Escape OFX Special Characters
#' @param x Character string.
#' @return Escaped string safe for OFX SGML.
#' @keywords internal
.ofx_escape <- function(x) {
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  x
}


#' Map Plaid Accounts to Summary
#'
#' Converts Plaid account objects to a summary tibble.
#'
#' @param plaid_accounts List of Plaid account objects.
#' @return Tibble with account_id, name, type, subtype, mask, balance fields.
#' @keywords internal
.plaid_accounts_summary <- function(plaid_accounts) {
  if (length(plaid_accounts) == 0) {
    return(tibble::tibble(
      account_id = character(),
      name = character(),
      type = character(),
      subtype = character(),
      mask = character(),
      balance_current = numeric(),
      balance_available = numeric()
    ))
  }

  tibble::tibble(
    account_id = vapply(plaid_accounts, function(a) a$account_id %||% NA_character_, character(1)),
    name = vapply(plaid_accounts, function(a) {
      a$official_name %||% a$name %||% NA_character_
    }, character(1)),
    type = vapply(plaid_accounts, function(a) a$type %||% NA_character_, character(1)),
    subtype = vapply(plaid_accounts, function(a) a$subtype %||% NA_character_, character(1)),
    mask = vapply(plaid_accounts, function(a) a$mask %||% NA_character_, character(1)),
    balance_current = vapply(plaid_accounts, function(a) {
      a$balances$current %||% NA_real_
    }, numeric(1)),
    balance_available = vapply(plaid_accounts, function(a) {
      a$balances$available %||% NA_real_
    }, numeric(1))
  )
}
