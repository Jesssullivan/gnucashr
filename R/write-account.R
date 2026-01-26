#' Account Write Operations
#'
#' Functions for creating and modifying accounts in GnuCash.
#' All write operations require a writable GnuCashDB connection.
#'
#' @name write-account
NULL

#' Validate Account Data
#'
#' Check that account data is valid before creation.
#'
#' @param name Account name
#' @param account_type GnuCash account type
#' @param parent_guid Parent account GUID
#' @param commodity_guid Currency/commodity GUID
#' @param gc GnuCashDB for validation context
#' @return Result with validated data or error
#' @noRd
validate_account_data <- function(name, account_type, parent_guid, commodity_guid, gc) {
  errors <- character()

  # Name validation
  if (is.null(name) || nchar(trimws(name)) == 0) {
    errors <- c(errors, "Account name is required")
  }

  if (grepl(":", name)) {
    errors <- c(errors, "Account name cannot contain colons (use parent_guid for hierarchy)")
  }

  # Account type validation
  valid_types <- c(
    "NONE", "BANK", "CASH", "CREDIT", "ASSET", "LIABILITY",
    "STOCK", "MUTUAL", "CURRENCY", "INCOME", "EXPENSE",
    "EQUITY", "RECEIVABLE", "PAYABLE", "ROOT", "TRADING"
  )

  if (!account_type %in% valid_types) {
    errors <- c(errors, paste("Invalid account type. Must be one of:",
                              paste(valid_types, collapse = ", ")))
  }

  # Parent validation
  if (!is.null(parent_guid) && !is.na(parent_guid)) {
    parent <- gc$get_account(parent_guid)
    if (is.null(parent)) {
      errors <- c(errors, paste("Parent account not found:", parent_guid))
    }
  }

  # Check for duplicate names under same parent
  existing <- gc$accounts(collected = TRUE) |>
    dplyr::filter(
      name == !!name,
      (is.na(parent_guid) & is.na(!!parent_guid)) |
        (!is.na(parent_guid) & parent_guid == !!parent_guid)
    )

  if (nrow(existing) > 0) {
    errors <- c(errors, paste("Account already exists:", name))
  }

  if (length(errors) > 0) {
    return(err(paste(errors, collapse = "; ")))
  }

  ok(list(
    name = trimws(name),
    account_type = account_type,
    parent_guid = parent_guid,
    commodity_guid = commodity_guid
  ))
}

#' Create a New Account
#'
#' Create a new account in the GnuCash book.
#'
#' @param gc GnuCashDB object (must be opened with read_only = FALSE)
#' @param name Account name
#' @param account_type Account type (BANK, CASH, ASSET, LIABILITY, etc.)
#' @param parent Parent account GUID or path (default: ROOT)
#' @param commodity_guid Currency GUID (default: book default currency)
#' @param description Optional account description
#' @param code Optional account code
#' @param placeholder Is this a placeholder account? (default: FALSE)
#' @param hidden Is this account hidden? (default: FALSE)
#' @return Result with new account GUID or error
#' @export
create_account <- function(
    gc,
    name,
    account_type,
    parent = NULL,
    commodity_guid = NULL,
    description = NULL,
    code = NULL,
    placeholder = FALSE,
    hidden = FALSE
) {
  # Check writable
  meta <- gc$metadata()
  if (meta$read_only) {
    return(err("GnuCash file opened in read-only mode. Use read_gnucash(path, read_only = FALSE)"))
  }

  # Only SQLite format supports writes
  if (meta$format != "sqlite") {
    return(err("Write operations only supported for SQLite format. Convert XML to SQLite first."))
  }

  # Resolve parent
  parent_guid <- NULL
  if (is.null(parent)) {
    # Use root account
    parent_guid <- meta$root_account_guid
  } else if (grepl("^[a-f0-9]{32}$", parent, ignore.case = TRUE)) {
    parent_guid <- parent
  } else {
    # Treat as path
    parent_account <- gc$get_account(parent)
    if (is.null(parent_account)) {
      return(err(paste("Parent account not found:", parent)))
    }
    parent_guid <- parent_account$guid
  }

  # Default commodity to book currency
  if (is.null(commodity_guid)) {
    commodities <- gc$commodities(collected = TRUE) |>
      dplyr::filter(mnemonic == meta$default_currency)
    if (nrow(commodities) > 0) {
      commodity_guid <- commodities$guid[1]
    }
  }

  # Validate
  validation <- validate_account_data(name, account_type, parent_guid, commodity_guid, gc)
  if (is_err(validation)) {
    return(validation)
  }

  # Generate GUID
  new_guid <- generate_guid()

  # Build account record
  account_record <- list(
    guid = new_guid,
    name = name,
    account_type = account_type,
    commodity_guid = commodity_guid,
    commodity_scu = 100L,  # Standard currency units
    non_std_scu = 0L,
    parent_guid = parent_guid,
    code = code %||% "",
    description = description %||% "",
    hidden = as.integer(hidden),
    placeholder = as.integer(placeholder)
  )

  # Insert with transaction safety
  result <- with_db_transaction(gc, function(con) {
    DBI::dbExecute(con, "
      INSERT INTO accounts
      (guid, name, account_type, commodity_guid, commodity_scu, non_std_scu,
       parent_guid, code, description, hidden, placeholder)
      VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
    ", params = list(
      account_record$guid,
      account_record$name,
      account_record$account_type,
      account_record$commodity_guid,
      account_record$commodity_scu,
      account_record$non_std_scu,
      account_record$parent_guid,
      account_record$code,
      account_record$description,
      account_record$hidden,
      account_record$placeholder
    ))
    new_guid
  })

  result
}

#' Create Account from Path
#'
#' Create an account and any missing parent accounts from a path.
#'
#' @param gc GnuCashDB object
#' @param path Account path (e.g., "Assets:Current:Checking")
#' @param account_type Type for the leaf account
#' @param parent_type Type for auto-created parent accounts (default: same as leaf)
#' @param ... Additional arguments passed to create_account
#' @return Result with leaf account GUID or error
#' @export
create_account_path <- function(gc, path, account_type, parent_type = NULL, ...) {
  if (is.null(parent_type)) {
    parent_type <- account_type
  }

  parts <- strsplit(path, ":")[[1]]

  if (length(parts) == 0) {
    return(err("Empty account path"))
  }

  # Start from root
  current_parent <- gc$metadata()$root_account_guid
  current_path <- ""

  for (i in seq_along(parts)) {
    part <- parts[i]
    current_path <- if (current_path == "") part else paste(current_path, part, sep = ":")
    is_leaf <- i == length(parts)

    # Check if account exists
    existing <- gc$get_account(current_path)

    if (!is.null(existing)) {
      current_parent <- existing$guid
      next
    }

    # Create the account
    type <- if (is_leaf) account_type else parent_type
    result <- create_account(
      gc,
      name = part,
      account_type = type,
      parent = current_parent,
      placeholder = !is_leaf,
      ...
    )

    if (is_err(result)) {
      return(err(paste("Failed to create", current_path, ":", unwrap_err(result))))
    }

    current_parent <- unwrap(result)
  }

  ok(current_parent)
}

#' Update Account
#'
#' Update an existing account's properties.
#'
#' @param gc GnuCashDB object
#' @param account_guid Account GUID to update
#' @param name New name (optional)
#' @param description New description (optional)
#' @param code New code (optional)
#' @param hidden New hidden status (optional)
#' @param placeholder New placeholder status (optional)
#' @return Result with updated GUID or error
#' @export
update_account <- function(
    gc,
    account_guid,
    name = NULL,
    description = NULL,
    code = NULL,
    hidden = NULL,
    placeholder = NULL
) {
  meta <- gc$metadata()
  if (meta$read_only) {
    return(err("GnuCash file opened in read-only mode"))
  }

  if (meta$format != "sqlite") {
    return(err("Write operations only supported for SQLite format"))
  }

  # Verify account exists
  existing <- gc$get_account(account_guid)
  if (is.null(existing)) {
    return(err(paste("Account not found:", account_guid)))
  }

  # Build update query
  updates <- character()
  params <- list()

  if (!is.null(name)) {
    updates <- c(updates, "name = ?")
    params <- c(params, list(name))
  }
  if (!is.null(description)) {
    updates <- c(updates, "description = ?")
    params <- c(params, list(description))
  }
  if (!is.null(code)) {
    updates <- c(updates, "code = ?")
    params <- c(params, list(code))
  }
  if (!is.null(hidden)) {
    updates <- c(updates, "hidden = ?")
    params <- c(params, list(as.integer(hidden)))
  }
  if (!is.null(placeholder)) {
    updates <- c(updates, "placeholder = ?")
    params <- c(params, list(as.integer(placeholder)))
  }

  if (length(updates) == 0) {
    return(ok(account_guid))  # Nothing to update
  }

  params <- c(params, list(account_guid))

  query <- paste0(
    "UPDATE accounts SET ",
    paste(updates, collapse = ", "),
    " WHERE guid = ?"
  )

  with_db_transaction(gc, function(con) {
    DBI::dbExecute(con, query, params = params)
    account_guid
  })
}

#' Delete Account
#'
#' Delete an account (must have no transactions and no children).
#'
#' @param gc GnuCashDB object
#' @param account_guid Account GUID to delete
#' @param force Force delete even with children (will reparent children to grandparent)
#' @return Result with deleted GUID or error
#' @export
delete_account <- function(gc, account_guid, force = FALSE) {
  meta <- gc$metadata()
  if (meta$read_only) {
    return(err("GnuCash file opened in read-only mode"))
  }

  if (meta$format != "sqlite") {
    return(err("Write operations only supported for SQLite format"))
  }

  # Verify account exists
  existing <- gc$get_account(account_guid)
  if (is.null(existing)) {
    return(err(paste("Account not found:", account_guid)))
  }

  # Check for transactions
  splits <- gc$splits(collected = TRUE) |>
    dplyr::filter(account_guid == !!account_guid)

  if (nrow(splits) > 0) {
    return(err(paste("Account has", nrow(splits),
                     "transaction splits. Move or delete transactions first.")))
  }

  # Check for children
  children <- gc$accounts(collected = TRUE) |>
    dplyr::filter(parent_guid == !!account_guid)

  if (nrow(children) > 0 && !force) {
    return(err(paste("Account has", nrow(children),
                     "child accounts. Use force=TRUE to reparent them.")))
  }

  with_db_transaction(gc, function(con) {
    # Reparent children if force=TRUE
    if (nrow(children) > 0) {
      grandparent <- existing$parent_guid
      DBI::dbExecute(con,
                     "UPDATE accounts SET parent_guid = ? WHERE parent_guid = ?",
                     params = list(grandparent, account_guid))
    }

    # Delete the account
    DBI::dbExecute(con,
                   "DELETE FROM accounts WHERE guid = ?",
                   params = list(account_guid))

    account_guid
  })
}

#' Execute with Database Transaction
#'
#' Run a function within a database transaction.
#'
#' @param gc GnuCashDB object
#' @param fn Function to execute (receives connection)
#' @return Result with fn's return value or error
#' @noRd
with_db_transaction <- function(gc, fn) {
  # Access the private connection
  con <- gc$.__enclos_env__$private$con

  if (is.null(con) || !DBI::dbIsValid(con)) {
    return(err("Database connection is not valid"))
  }

  tryCatch({
    DBI::dbBegin(con)
    result <- fn(con)
    DBI::dbCommit(con)
    # Refresh cache after successful write
    gc$refresh()
    ok(result)
  }, error = function(e) {
    try(DBI::dbRollback(con), silent = TRUE)
    err(conditionMessage(e))
  })
}

#' Safe Create Account
#'
#' Create account with automatic backup.
#'
#' @inheritParams create_account
#' @param ... Additional arguments passed to create_account
#' @return Logged result with account GUID or error
#' @export
safe_create_account <- function(gc, name, account_type, parent = NULL, ...) {
  result <- logged(NULL) |>
    log_append(sprintf("Creating account: %s (%s)", name, account_type))

  # Create backup first
  backup_result <- create_backup(gc)
  if (is_err(backup_result)) {
    result <- log_append(result, paste("Backup failed:", unwrap_err(backup_result)))
    return(logged(backup_result, result$log))
  }

  result <- log_append(result, "Backup created successfully")

  # Create the account
  create_result <- create_account(gc, name, account_type, parent, ...)

  if (is_ok(create_result)) {
    result <- log_append(result, sprintf("Account created: %s", unwrap(create_result)))
    logged(create_result, result$log)
  } else {
    result <- log_append(result, sprintf("Creation failed: %s", unwrap_err(create_result)))
    logged(create_result, result$log)
  }
}
