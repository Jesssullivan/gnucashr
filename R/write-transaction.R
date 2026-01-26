#' Transaction Write Operations
#'
#' Functions for posting and modifying transactions in GnuCash.
#' All write operations require a writable GnuCashDB connection.
#'
#' @name write-transaction
NULL

#' Validate Transaction Splits
#'
#' Check that splits balance to zero and reference valid accounts.
#'
#' @param splits List of split data (account, value, memo, etc.)
#' @param gc GnuCashDB for validation context
#' @return Result with validated splits or error
#' @noRd
validate_splits <- function(splits, gc) {
  if (length(splits) < 2) {
    return(err("Transaction must have at least 2 splits"))
  }

  errors <- character()
  total <- 0

  for (i in seq_along(splits)) {
    split <- splits[[i]]

    # Check required fields
    if (is.null(split$account)) {
      errors <- c(errors, sprintf("Split %d: account is required", i))
      next
    }

    if (is.null(split$value)) {
      errors <- c(errors, sprintf("Split %d: value is required", i))
      next
    }

    # Validate account exists
    account <- gc$get_account(split$account)
    if (is.null(account)) {
      errors <- c(errors, sprintf("Split %d: account not found: %s", i, split$account))
      next
    }

    # Update split with resolved GUID
    splits[[i]]$account_guid <- account$guid

    # Accumulate total
    total <- total + split$value
  }

  # Check balance
  if (abs(total) > 0.001) {
    errors <- c(errors, sprintf("Splits do not balance: sum = %.4f", total))
  }

  if (length(errors) > 0) {
    return(err(paste(errors, collapse = "; ")))
  }

  ok(splits)
}

#' Post a Transaction
#'
#' Create a new transaction with splits in GnuCash.
#'
#' @param gc GnuCashDB object (must be opened with read_only = FALSE)
#' @param description Transaction description
#' @param splits List of splits, each containing:
#'   - account: Account GUID or path
#'   - value: Numeric value (positive = debit, negative = credit)
#'   - memo: Optional memo
#'   - action: Optional action
#' @param post_date Transaction date (default: today)
#' @param num Optional transaction number/reference
#' @param currency Currency mnemonic (default: book default)
#' @return Result with new transaction GUID or error
#' @export
#' @examples
#' \dontrun{
#' # Simple two-split transaction
#' post_transaction(gc, "Office supplies",
#'   splits = list(
#'     list(account = "Expenses:Office", value = 50.00),
#'     list(account = "Assets:Checking", value = -50.00)
#'   )
#' )
#'
#' # Multi-split transaction
#' post_transaction(gc, "Payroll",
#'   splits = list(
#'     list(account = "Expenses:Salary", value = 5000),
#'     list(account = "Expenses:Payroll Tax", value = 382.50),
#'     list(account = "Liabilities:Payroll Tax", value = -382.50),
#'     list(account = "Assets:Checking", value = -5000)
#'   )
#' )
#' }
post_transaction <- function(
    gc,
    description,
    splits,
    post_date = Sys.Date(),
    num = NULL,
    currency = NULL
) {
  # Check writable
  meta <- gc$metadata()
  if (meta$read_only) {
    return(err("GnuCash file opened in read-only mode. Use read_gnucash(path, read_only = FALSE)"))
  }

  if (meta$format != "sqlite") {
    return(err("Write operations only supported for SQLite format. Convert XML to SQLite first."))
  }

  # Validate description
  if (is.null(description) || nchar(trimws(description)) == 0) {
    return(err("Transaction description is required"))
  }

  # Validate splits
  validation <- validate_splits(splits, gc)
  if (is_err(validation)) {
    return(validation)
  }
  validated_splits <- unwrap(validation)

  # Resolve currency
  if (is.null(currency)) {
    currency <- meta$default_currency
  }

  currency_record <- gc$commodities(collected = TRUE) |>
    dplyr::filter(mnemonic == currency)

  if (nrow(currency_record) == 0) {
    return(err(paste("Currency not found:", currency)))
  }

  currency_guid <- currency_record$guid[1]

  # Generate GUIDs
  tx_guid <- generate_guid()
  post_date <- as.Date(post_date)
  enter_date <- Sys.time()

  # Format dates for GnuCash
  # GnuCash stores dates as strings like "2024-01-15 00:00:00"
  post_date_str <- format(as.POSIXct(post_date), "%Y-%m-%d 00:00:00")
  enter_date_str <- format(enter_date, "%Y-%m-%d %H:%M:%S")

  with_db_transaction(gc, function(con) {
    # Insert transaction
    DBI::dbExecute(con, "
      INSERT INTO transactions
      (guid, currency_guid, num, post_date, enter_date, description)
      VALUES (?, ?, ?, ?, ?, ?)
    ", params = list(
      tx_guid,
      currency_guid,
      num %||% "",
      post_date_str,
      enter_date_str,
      description
    ))

    # Insert splits
    for (split in validated_splits) {
      split_guid <- generate_guid()

      # Convert value to fraction (cents)
      value_num <- as.integer(round(split$value * 100))
      value_denom <- 100L

      DBI::dbExecute(con, "
        INSERT INTO splits
        (guid, tx_guid, account_guid, memo, action, reconcile_state,
         reconcile_date, value_num, value_denom, quantity_num, quantity_denom, lot_guid)
        VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
      ", params = list(
        split_guid,
        tx_guid,
        split$account_guid,
        split$memo %||% "",
        split$action %||% "",
        "n",  # not reconciled
        NA_character_,  # reconcile_date - use NA instead of NULL
        value_num,
        value_denom,
        value_num,  # quantity = value for same currency
        value_denom,
        NA_character_  # lot_guid - use NA instead of NULL
      ))
    }

    tx_guid
  })
}

#' Post Simple Transfer
#'
#' Convenience function for posting a simple two-account transfer.
#'
#' @param gc GnuCashDB object
#' @param from_account Source account (will be credited/reduced)
#' @param to_account Destination account (will be debited/increased)
#' @param amount Amount to transfer (positive number)
#' @param description Transaction description
#' @param post_date Transaction date (default: today)
#' @param num Optional reference number
#' @return Result with transaction GUID or error
#' @export
#' @examples
#' \dontrun{
#' # Transfer $100 from checking to savings
#' post_transfer(gc, "Assets:Checking", "Assets:Savings", 100, "Monthly savings")
#'
#' # Pay an expense
#' post_transfer(gc, "Assets:Checking", "Expenses:Utilities", 150, "Electric bill")
#' }
post_transfer <- function(
    gc,
    from_account,
    to_account,
    amount,
    description,
    post_date = Sys.Date(),
    num = NULL
) {
  if (amount <= 0) {
    return(err("Amount must be positive"))
  }

  post_transaction(
    gc,
    description = description,
    splits = list(
      list(account = to_account, value = amount),
      list(account = from_account, value = -amount)
    ),
    post_date = post_date,
    num = num
  )
}

#' Delete Transaction
#'
#' Delete a transaction and all its splits.
#'
#' @param gc GnuCashDB object
#' @param tx_guid Transaction GUID to delete
#' @return Result with deleted GUID or error
#' @export
delete_transaction <- function(gc, tx_guid) {
  meta <- gc$metadata()
  if (meta$read_only) {
    return(err("GnuCash file opened in read-only mode"))
  }

  if (meta$format != "sqlite") {
    return(err("Write operations only supported for SQLite format"))
  }

  # Verify transaction exists
  tx <- gc$get_transaction(tx_guid)
  if (is.null(tx)) {
    return(err(paste("Transaction not found:", tx_guid)))
  }

  with_db_transaction(gc, function(con) {
    # Delete splits first (foreign key)
    DBI::dbExecute(con,
                   "DELETE FROM splits WHERE tx_guid = ?",
                   params = list(tx_guid))

    # Delete transaction
    DBI::dbExecute(con,
                   "DELETE FROM transactions WHERE guid = ?",
                   params = list(tx_guid))

    tx_guid
  })
}

#' Void Transaction
#'
#' Mark a transaction as void (preserves history but zeroes amounts).
#'
#' @param gc GnuCashDB object
#' @param tx_guid Transaction GUID to void
#' @param reason Reason for voiding
#' @return Result with voided GUID or error
#' @export
void_transaction <- function(gc, tx_guid, reason = "Voided") {
  meta <- gc$metadata()
  if (meta$read_only) {
    return(err("GnuCash file opened in read-only mode"))
  }

  if (meta$format != "sqlite") {
    return(err("Write operations only supported for SQLite format"))
  }

  # Verify transaction exists
  tx <- gc$get_transaction(tx_guid)
  if (is.null(tx)) {
    return(err(paste("Transaction not found:", tx_guid)))
  }

  with_db_transaction(gc, function(con) {
    # Prepend "VOID:" to description
    new_desc <- paste0("VOID: ", tx$transaction$description, " (", reason, ")")

    # Update transaction description
    DBI::dbExecute(con,
                   "UPDATE transactions SET description = ? WHERE guid = ?",
                   params = list(new_desc, tx_guid))

    # Zero out all splits
    DBI::dbExecute(con,
                   "UPDATE splits SET value_num = 0, quantity_num = 0 WHERE tx_guid = ?",
                   params = list(tx_guid))

    tx_guid
  })
}

#' Safe Post Transaction
#'
#' Post transaction with automatic backup and audit logging.
#'
#' @inheritParams post_transaction
#' @param ... Additional arguments passed to post_transaction
#' @return Logged result with transaction GUID or error
#' @export
safe_post_transaction <- function(gc, description, splits, post_date = Sys.Date(), ...) {
  result <- logged(NULL) |>
    log_append(sprintf("Posting transaction: %s", description)) |>
    log_append(sprintf("Splits: %d", length(splits))) |>
    log_append(sprintf("Date: %s", post_date))

  # Create backup first
  backup_result <- create_backup(gc)
  if (is_err(backup_result)) {
    result <- log_append(result, paste("Backup failed:", unwrap_err(backup_result)))
    return(logged(backup_result, result$log))
  }

  result <- log_append(result, sprintf("Backup created: %s",
                                       unwrap(backup_result)$backup_path))

  # Post the transaction
  post_result <- post_transaction(gc, description, splits, post_date, ...)

  if (is_ok(post_result)) {
    tx_guid <- unwrap(post_result)
    result <- log_append(result, sprintf("Transaction posted: %s", tx_guid))

    # Log split details
    for (i in seq_along(splits)) {
      s <- splits[[i]]
      result <- log_append(result, sprintf("  Split %d: %s = %.2f",
                                           i, s$account, s$value))
    }

    logged(post_result, result$log)
  } else {
    result <- log_append(result, sprintf("Post failed: %s", unwrap_err(post_result)))
    logged(post_result, result$log)
  }
}

#' Batch Post Transactions
#'
#' Post multiple transactions in a single database transaction.
#'
#' @param gc GnuCashDB object
#' @param transactions List of transaction specifications, each with:
#'   - description: Transaction description
#'   - splits: List of splits
#'   - post_date: Optional date (default: today)
#'   - num: Optional reference
#' @return Result with list of GUIDs or error (all or nothing)
#' @export
batch_post_transactions <- function(gc, transactions) {
  meta <- gc$metadata()
  if (meta$read_only) {
    return(err("GnuCash file opened in read-only mode"))
  }

  if (meta$format != "sqlite") {
    return(err("Write operations only supported for SQLite format"))
  }

  if (length(transactions) == 0) {
    return(ok(character()))
  }

  # Pre-validate all transactions
  for (i in seq_along(transactions)) {
    tx <- transactions[[i]]
    validation <- validate_splits(tx$splits, gc)
    if (is_err(validation)) {
      return(err(sprintf("Transaction %d validation failed: %s",
                         i, unwrap_err(validation))))
    }
    # Store validated splits back
    transactions[[i]]$validated_splits <- unwrap(validation)
  }

  # Get currency
  currency_record <- gc$commodities(collected = TRUE) |>
    dplyr::filter(mnemonic == meta$default_currency)

  if (nrow(currency_record) == 0) {
    return(err("Default currency not found"))
  }

  currency_guid <- currency_record$guid[1]

  # Post all in single transaction
  with_db_transaction(gc, function(con) {
    guids <- character(length(transactions))

    for (i in seq_along(transactions)) {
      tx <- transactions[[i]]
      tx_guid <- generate_guid()
      guids[i] <- tx_guid

      post_date <- tx$post_date %||% Sys.Date()
      post_date_str <- format(as.POSIXct(as.Date(post_date)), "%Y-%m-%d 00:00:00")
      enter_date_str <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")

      # Insert transaction
      DBI::dbExecute(con, "
        INSERT INTO transactions
        (guid, currency_guid, num, post_date, enter_date, description)
        VALUES (?, ?, ?, ?, ?, ?)
      ", params = list(
        tx_guid,
        currency_guid,
        tx$num %||% "",
        post_date_str,
        enter_date_str,
        tx$description
      ))

      # Insert splits
      for (split in tx$validated_splits) {
        split_guid <- generate_guid()
        value_num <- as.integer(round(split$value * 100))

        DBI::dbExecute(con, "
          INSERT INTO splits
          (guid, tx_guid, account_guid, memo, action, reconcile_state,
           reconcile_date, value_num, value_denom, quantity_num, quantity_denom, lot_guid)
          VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
        ", params = list(
          split_guid,
          tx_guid,
          split$account_guid,
          split$memo %||% "",
          split$action %||% "",
          "n",
          NA_character_,  # reconcile_date
          value_num,
          100L,
          value_num,
          100L,
          NA_character_   # lot_guid
        ))
      }
    }

    guids
  })
}
