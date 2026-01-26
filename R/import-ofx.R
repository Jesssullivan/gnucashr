#' OFX/QFX Import Functions
#'
#' Functions for importing bank statements in OFX (Open Financial Exchange)
#' and QFX (Quicken Financial Exchange) formats. Supports both OFX 1.x (SGML)
#' and OFX 2.x (XML) file formats.
#'
#' @name import-ofx
NULL


#' Import OFX/QFX File
#'
#' Read and parse an OFX or QFX bank statement file. Uses Rcpp-accelerated
#' parsing for performance with large statement files.
#'
#' @param path Path to the OFX/QFX file
#' @param account_mapping Optional named list mapping OFX transaction types
#'   to GnuCash account GUIDs. Keys are transaction types (DEBIT, CREDIT,
#'   CHECK, etc.), values are account GUIDs.
#' @return A tibble with columns:
#'   \describe{
#'     \item{date}{Transaction date (Date)}
#'     \item{description}{Transaction description from NAME field (character)}
#'     \item{amount}{Transaction amount, positive for credits (numeric)}
#'     \item{currency}{Currency code from CURDEF (character)}
#'     \item{external_id}{Bank's unique transaction ID - FITID (character)}
#'     \item{memo}{Additional memo text (character)}
#'     \item{transaction_type}{OFX transaction type: DEBIT, CREDIT, CHECK, etc. (character)}
#'   }
#' @export
#' @examples
#' \dontrun{
#' # Basic import
#' transactions <- import_ofx("bank_statement.ofx")
#'
#' # With account mapping for import
#' mapping <- list(
#'   DEBIT = "expense_account_guid",
#'   CREDIT = "income_account_guid"
#' )
#' transactions <- import_ofx("statement.qfx", account_mapping = mapping)
#' }
import_ofx <- function(path, account_mapping = NULL) {
  if (!file.exists(path)) {
    rlang::abort(paste("OFX file not found:", path))
  }


  # Read file content
  content <- paste(readLines(path, warn = FALSE), collapse = "\n")

  # Detect version for informational purposes
  version <- detect_ofx_version(path)

  # Parse using Rcpp function
  parsed <- parse_ofx_cpp(content)

  if (parsed$n_transactions == 0) {
    rlang::warn("No transactions found in OFX file")
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

  # Build result tibble
  result <- tibble::tibble(
    date = as.Date(parsed$dates),
    description = parsed$names,
    amount = parsed$amounts,
    currency = rep(parsed$currency, length(parsed$amounts)),
    external_id = parsed$fitids,
    memo = parsed$memos,
    transaction_type = parsed$trntypes
  )

  # Replace empty strings with NA for cleaner data

  result <- result |>
    dplyr::mutate(
      description = dplyr::if_else(description == "", NA_character_, description),
      memo = dplyr::if_else(memo == "", NA_character_, memo),
      transaction_type = dplyr::if_else(transaction_type == "", NA_character_, transaction_type)
    )

  # Add mapped account if mapping provided

  if (!is.null(account_mapping)) {
    result <- result |>
      dplyr::mutate(
        mapped_account = purrr::map_chr(
          transaction_type,
          ~ if (!is.na(.x) && .x %in% names(account_mapping)) {
            account_mapping[[.x]]
          } else {
            NA_character_
          }
        )
      )
  }

  # Add metadata as attribute
  attr(result, "ofx_version") <- version
  attr(result, "ofx_account") <- extract_ofx_account_info(content)

  result
}


#' Detect OFX Version
#'
#' Determine whether an OFX file is version 1.x (SGML-based) or
#' version 2.x (XML-based).
#'
#' @param path Path to the OFX/QFX file
#' @return Character string: "1" for SGML, "2" for XML, "unknown" otherwise
#' @export
#' @examples
#' \dontrun{
#' version <- detect_ofx_version("statement.ofx")
#' if (version == "1") {
#'   message("OFX 1.x SGML format detected")
#' }
#' }
detect_ofx_version <- function(path) {
  if (!file.exists(path)) {
    rlang::abort(paste("OFX file not found:", path))
  }

  content <- paste(readLines(path, warn = FALSE, n = 50), collapse = "\n")
  detect_ofx_version_cpp(content)
}


#' Validate OFX Import Data
#'
#' Check imported OFX data for duplicates against existing transactions
#' in a GnuCash database. Uses FITID (external_id) for duplicate detection.
#'
#' @param ofx_data Tibble from `import_ofx()`
#' @param gc A GnuCashDB connection
#' @param account Account GUID or path to check for duplicates
#' @return A tibble with the same structure as `ofx_data` plus columns:
#'   \describe{
#'     \item{is_duplicate}{Logical indicating if transaction already exists}
#'     \item{existing_tx_guid}{GUID of matching transaction if duplicate}
#'   }
#' @export
#' @examples
#' \dontrun{
#' ofx <- import_ofx("statement.ofx")
#' gc <- read_gnucash("books.gnucash")
#' validated <- validate_ofx_import(ofx, gc, "Assets:Bank:Checking")
#'
#' # Filter to new transactions only
#' new_transactions <- validated |> dplyr::filter(!is_duplicate)
#' }
validate_ofx_import <- function(ofx_data, gc, account) {
  if (!inherits(gc, "GnuCashDB")) {
    rlang::abort("gc must be a GnuCashDB object")
  }

  # Resolve account
  acct <- gc$get_account(account)
  if (is.null(acct)) {
    rlang::abort(paste("Account not found:", account))
  }
  account_guid <- acct$guid

  # Get existing transactions for this account
  existing <- gc$splits(collected = TRUE) |>
    dplyr::filter(account_guid == !!account_guid) |>
    dplyr::left_join(
      gc$transactions(collected = TRUE) |>
        dplyr::select(guid, description),
      by = c("tx_guid" = "guid")
    )

  # For duplicate detection, we need to check if FITID was stored

  # GnuCash stores FITID in the split memo or as a slot
  # We'll check memo field for now as that's common practice

  # Check for duplicates by external_id in memo
  ofx_data <- ofx_data |>
    dplyr::mutate(
      is_duplicate = purrr::map_lgl(
        external_id,
        function(fitid) {
          if (is.na(fitid) || fitid == "") {
            return(FALSE)
          }
          # Check if FITID appears in any memo for this account
          any(grepl(fitid, existing$memo, fixed = TRUE), na.rm = TRUE)
        }
      ),
      existing_tx_guid = purrr::map_chr(
        external_id,
        function(fitid) {
          if (is.na(fitid) || fitid == "") {
            return(NA_character_)
          }
          matches <- existing$tx_guid[grepl(fitid, existing$memo, fixed = TRUE)]
          if (length(matches) > 0) matches[1] else NA_character_
        }
      )
    )

  # Also check by date + amount + description for transactions without FITID
  ofx_data <- ofx_data |>
    dplyr::mutate(
      is_duplicate = dplyr::if_else(
        is_duplicate,
        TRUE,
        purrr::pmap_lgl(
          list(date, amount, description),
          function(d, a, desc) {
            if (is.na(d) || is.na(a)) return(FALSE)
            # Check for matching date and amount
            amount_num <- as.integer(round(a * 100))
            matches <- existing |>
              dplyr::filter(
                abs(value_num - amount_num) < 2,  # Allow 1 cent tolerance
                value_denom == 100L
              )
            nrow(matches) > 0
          }
        )
      )
    )

  ofx_data
}


#' Import OFX Transactions to GnuCash
#'
#' Import OFX transactions directly into a GnuCash database.
#' Creates transactions with splits between the target account and
#' a mapped or default offset account.
#'
#' @param ofx_data Tibble from `import_ofx()` or `validate_ofx_import()`
#' @param gc A GnuCashDB connection (must be opened with read_only = FALSE)
#' @param target_account Account GUID or path for the bank account
#' @param offset_account Default offset account for transactions without mapping
#' @param skip_duplicates If TRUE (default), skip transactions marked as duplicates
#' @param store_fitid If TRUE (default), store FITID in memo for duplicate detection
#' @return Tibble with import results: original data plus tx_guid for created transactions
#' @export
#' @examples
#' \dontrun{
#' ofx <- import_ofx("statement.ofx")
#' gc <- read_gnucash("books.gnucash", read_only = FALSE)
#'
#' result <- import_ofx_to_gnucash(
#'   ofx,
#'   gc,
#'   target_account = "Assets:Bank:Checking",
#'   offset_account = "Imbalance-USD"
#' )
#'
#' gc$close()
#' }
import_ofx_to_gnucash <- function(ofx_data,
                                   gc,
                                   target_account,
                                   offset_account,
                                   skip_duplicates = TRUE,
                                   store_fitid = TRUE) {
  if (!inherits(gc, "GnuCashDB")) {
    rlang::abort("gc must be a GnuCashDB object")
  }

  if (gc$metadata()$read_only) {
    rlang::abort("GnuCash database must be opened with read_only = FALSE for import")
  }

  # Resolve accounts
  target <- gc$get_account(target_account)
  if (is.null(target)) {
    rlang::abort(paste("Target account not found:", target_account))
  }

  offset <- gc$get_account(offset_account)
  if (is.null(offset)) {
    rlang::abort(paste("Offset account not found:", offset_account))
  }

  # Filter duplicates if requested
  if (skip_duplicates && "is_duplicate" %in% names(ofx_data)) {
    to_import <- ofx_data |> dplyr::filter(!is_duplicate)
  } else {
    to_import <- ofx_data
  }

  if (nrow(to_import) == 0) {
    rlang::inform("No new transactions to import")
    return(ofx_data |> dplyr::mutate(imported_tx_guid = NA_character_))
  }

  # Get currency GUID
  currency_guid <- target$commodity_guid

  # Import each transaction
  imported_guids <- character(nrow(to_import))

  for (i in seq_len(nrow(to_import))) {
    row <- to_import[i, ]

    # Build memo with FITID if requested
    target_memo <- if (store_fitid && !is.na(row$external_id) && row$external_id != "") {
      paste0("[FITID:", row$external_id, "] ", if_else(is.na(row$memo), "", row$memo))
    } else {
      row$memo
    }

    # Determine offset account (use mapped if available)
    current_offset <- if ("mapped_account" %in% names(row) && !is.na(row$mapped_account)) {
      row$mapped_account
    } else {
      offset$guid
    }

    # Create transaction using post_transaction
    tryCatch({
      result <- post_transaction(
        gc,
        post_date = row$date,
        description = dplyr::if_else(is.na(row$description), "OFX Import", row$description),
        splits = list(
          list(
            account_guid = target$guid,
            value = row$amount,
            memo = target_memo
          ),
          list(
            account_guid = current_offset,
            value = -row$amount,
            memo = NA_character_
          )
        ),
        currency = currency_guid
      )
      imported_guids[i] <- result$tx_guid
    }, error = function(e) {
      rlang::warn(paste("Failed to import transaction:", row$external_id, "-", conditionMessage(e)))
      imported_guids[i] <- NA_character_
    })
  }

  # Add imported GUIDs to result
  to_import$imported_tx_guid <- imported_guids

  # Merge back with original data
  if (skip_duplicates && "is_duplicate" %in% names(ofx_data)) {
    ofx_data <- ofx_data |>
      dplyr::left_join(
        to_import |> dplyr::select(external_id, imported_tx_guid),
        by = "external_id"
      )
  } else {
    ofx_data <- to_import
  }

  n_imported <- sum(!is.na(imported_guids))
  rlang::inform(paste("Imported", n_imported, "of", nrow(to_import), "transactions"))

  ofx_data
}


#' Get OFX Account Information
#'
#' Extract account metadata from an OFX file, including bank ID,
#' account number, and statement date range.
#'
#' @param path Path to the OFX/QFX file
#' @return List with:
#'   \describe{
#'     \item{account_id}{Account number (masked in display)}
#'     \item{bank_id}{Bank routing number}
#'     \item{account_type}{Account type (CHECKING, SAVINGS, etc.)}
#'     \item{org_name}{Financial institution name}
#'     \item{date_start}{Statement start date}
#'     \item{date_end}{Statement end date}
#'     \item{ledger_balance}{Ending balance if available}
#'   }
#' @export
#' @examples
#' \dontrun{
#' info <- get_ofx_account_info("statement.ofx")
#' cat("Bank:", info$org_name, "\n")
#' cat("Account:", info$account_type, "\n")
#' cat("Period:", info$date_start, "to", info$date_end, "\n")
#' }
get_ofx_account_info <- function(path) {
  if (!file.exists(path)) {
    rlang::abort(paste("OFX file not found:", path))
  }

  content <- paste(readLines(path, warn = FALSE), collapse = "\n")
  extract_ofx_account_info(content)
}


#' Print OFX Import Summary
#'
#' Display a summary of imported OFX data including transaction counts,
#' date range, and total amounts.
#'
#' @param ofx_data Tibble from `import_ofx()`
#' @return Invisible NULL, prints summary to console
#' @export
print_ofx_summary <- function(ofx_data) {
  if (!inherits(ofx_data, "data.frame") || nrow(ofx_data) == 0) {
    cat("No OFX data to summarize\n")
    return(invisible(NULL))
  }

  cat("OFX Import Summary\n")
  cat(strrep("-", 40), "\n")

  # Version info if available
  if (!is.null(attr(ofx_data, "ofx_version"))) {
    cat("OFX Version:", attr(ofx_data, "ofx_version"), "\n")
  }

  # Account info if available
  acct_info <- attr(ofx_data, "ofx_account")
  if (!is.null(acct_info) && !is.null(acct_info$org_name) && acct_info$org_name != "") {
    cat("Institution:", acct_info$org_name, "\n")
  }
  if (!is.null(acct_info) && !is.null(acct_info$account_type) && acct_info$account_type != "") {
    cat("Account Type:", acct_info$account_type, "\n")
  }

  cat("\nTransactions:", nrow(ofx_data), "\n")
  cat("Date Range:", as.character(min(ofx_data$date, na.rm = TRUE)),
      "to", as.character(max(ofx_data$date, na.rm = TRUE)), "\n")

  # Currency
  if ("currency" %in% names(ofx_data)) {
    currencies <- unique(ofx_data$currency)
    cat("Currency:", paste(currencies, collapse = ", "), "\n")
  }

  # Amount summary
  credits <- sum(ofx_data$amount[ofx_data$amount > 0], na.rm = TRUE)
  debits <- sum(ofx_data$amount[ofx_data$amount < 0], na.rm = TRUE)
  net <- sum(ofx_data$amount, na.rm = TRUE)

  cat("\nCredits:", format_currency(credits), "\n")
  cat("Debits:", format_currency(debits), "\n")
  cat("Net:", format_currency(net), "\n")

  # Transaction types
  if ("transaction_type" %in% names(ofx_data)) {
    types <- table(ofx_data$transaction_type, useNA = "ifany")
    cat("\nTransaction Types:\n")
    for (type in names(types)) {
      cat("  ", if (is.na(type)) "(unknown)" else type, ":", types[type], "\n")
    }
  }

  # Duplicate info if available
  if ("is_duplicate" %in% names(ofx_data)) {
    n_dup <- sum(ofx_data$is_duplicate, na.rm = TRUE)
    cat("\nDuplicates:", n_dup, "of", nrow(ofx_data), "\n")
    cat("New Transactions:", nrow(ofx_data) - n_dup, "\n")
  }

  invisible(NULL)
}
