#' CSV Import Functions for gnucashr
#'
#' Functions for importing transaction data from various payment platforms
#' and accounting systems into a standardized gnucashr format.
#'
#' @name csv-import
#' @keywords internal
NULL

# =============================================================================
# Standard Output Format
# =============================================================================

#' Create a gnucashr CSV Import Object
#'
#' Internal constructor for standardized import results.
#'
#' @param data A tibble with the standard import columns
#' @param source_type Character identifying the source (e.g., "paypal", "stripe")
#' @param import_metadata List of metadata about the import
#' @return A tibble with class `gnucashr_csv_import`
#' @noRd
new_csv_import <- function(data, source_type, import_metadata = list()) {
  if (!tibble::is_tibble(data)) {
    data <- tibble::as_tibble(data)
  }

  # Ensure required columns exist

required_cols <- c("date", "description", "amount", "currency",
                     "external_id", "account", "memo", "category")

  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    rlang::abort(paste(
      "Missing required columns:",
      paste(missing_cols, collapse = ", ")
    ))
  }

  # Ensure column types
  data <- data |>
    dplyr::mutate(
      date = as.Date(date),
      description = as.character(description),
      amount = as.numeric(amount),
      currency = as.character(currency),
      external_id = as.character(external_id),
      account = as.character(account),
      memo = as.character(memo),
      category = as.character(category)
    )

  attr(data, "source_type") <- source_type
  attr(data, "import_metadata") <- import_metadata
  attr(data, "import_time") <- Sys.time()

  class(data) <- c("gnucashr_csv_import", class(data))
  data
}


#' Check if Object is a gnucashr CSV Import
#'
#' @param x Object to check
#' @return Logical TRUE if x is a gnucashr_csv_import
#' @export
#' @examples
#' \dontrun{
#' import <- import_paypal_csv("paypal_export.csv")
#' is_csv_import(import)  # TRUE
#' }
is_csv_import <- function(x) {
  inherits(x, "gnucashr_csv_import")
}


#' Print Method for gnucashr CSV Import
#'
#' @param x A gnucashr_csv_import object
#' @param ... Additional arguments (ignored)
#' @export
print.gnucashr_csv_import <- function(x, ...) {
  cat("<gnucashr_csv_import>\n")
  cat("  Source:", attr(x, "source_type"), "\n")
  cat("  Rows:", nrow(x), "\n")
  cat("  Date range:",
      format(min(x$date, na.rm = TRUE), "%Y-%m-%d"), "to",
      format(max(x$date, na.rm = TRUE), "%Y-%m-%d"), "\n")

  currencies <- unique(x$currency)
  cat("  Currencies:", paste(currencies, collapse = ", "), "\n")

  categories <- unique(x$category)
  cat("  Categories:", paste(categories[1:min(5, length(categories))], collapse = ", "))
  if (length(categories) > 5) cat(", ...")
  cat("\n")

  if (!is.null(attr(x, "import_time"))) {
    cat("  Imported:", format(attr(x, "import_time"), "%Y-%m-%d %H:%M:%S"), "\n")
  }

  invisible(x)
}


# =============================================================================
# PayPal Importer
# =============================================================================

#' Import PayPal Activity CSV Export
#'
#' Imports transaction data from a PayPal activity export CSV file.
#' PayPal exports can have different column layouts; this function
#' auto-detects the format based on header columns.
#'
#' @param path Path to the PayPal CSV file
#' @param include_pending Include transactions with "Pending" status (default FALSE)
#' @param timezone Timezone for date conversion (default "UTC")
#' @return A tibble with class `gnucashr_csv_import` containing standardized columns
#' @export
#' @examples
#' \dontrun{
#' paypal_data <- import_paypal_csv("paypal_activity.csv")
#' print(paypal_data)
#' }
import_paypal_csv <- function(path, include_pending = FALSE, timezone = "UTC") {
  if (!file.exists(path)) {
    rlang::abort(paste("PayPal CSV file not found:", path))
  }

  # Read raw data to detect format
  raw_data <- readr::read_csv(
    path,
    col_types = readr::cols(.default = readr::col_character()),
    show_col_types = FALSE
  )

  if (nrow(raw_data) == 0) {
    rlang::abort("PayPal CSV file is empty")
  }

  # Detect PayPal format based on columns
  format_info <- detect_paypal_format(names(raw_data))

  # Parse based on detected format
  result <- parse_paypal_data(raw_data, format_info, include_pending, timezone)

  # Create import object
  metadata <- list(
    path = normalizePath(path),
    format = format_info$format_name,
    original_rows = nrow(raw_data),
    imported_rows = nrow(result)
  )

  new_csv_import(result, "paypal", metadata)
}


#' Detect PayPal CSV Format
#'
#' @param col_names Vector of column names from CSV
#' @return List with format info and column mappings
#' @noRd
detect_paypal_format <- function(col_names) {
  col_names_lower <- tolower(col_names)

  # Standard PayPal download format (most common)
  if (all(c("date", "time", "name", "type", "status", "gross") %in% col_names_lower)) {
    return(list(
      format_name = "standard",
      date_col = "Date",
      time_col = "Time",
      tz_col = "TimeZone",
      name_col = "Name",
      type_col = "Type",
      status_col = "Status",
      currency_col = "Currency",
      gross_col = "Gross",
      fee_col = "Fee",
      net_col = "Net",
      balance_col = "Balance",
      txn_id_col = "Transaction ID",
      item_col = "Item Title",
      note_col = "Note"
    ))
  }

  # Alternative format with "Subject" instead of "Item Title"
  if (all(c("date", "gross", "currency", "transaction id") %in% col_names_lower)) {
    # Find actual column names (case-sensitive)
    actual_cols <- sapply(c("date", "gross", "currency", "transaction id", "name",
                            "type", "status", "fee", "net"),
                          function(x) col_names[tolower(col_names) == x][1],
                          USE.NAMES = TRUE)
    return(list(
      format_name = "alternative",
      date_col = actual_cols["date"],
      time_col = NULL,
      tz_col = NULL,
      name_col = actual_cols["name"],
      type_col = actual_cols["type"],
      status_col = actual_cols["status"],
      currency_col = actual_cols["currency"],
      gross_col = actual_cols["gross"],
      fee_col = actual_cols["fee"],
      net_col = actual_cols["net"],
      balance_col = NULL,
      txn_id_col = actual_cols["transaction id"],
      item_col = NULL,
      note_col = NULL
    ))
  }

  rlang::abort(paste(
    "Unrecognized PayPal CSV format. Expected columns include:",
    "Date, Time, Name, Type, Status, Currency, Gross, Fee, Net, Transaction ID"
  ))
}


#' Parse PayPal Data Based on Format
#'
#' @param data Raw tibble from CSV
#' @param format_info Format detection result
#' @param include_pending Include pending transactions
#' @param timezone Timezone for conversion
#' @return Tibble in standard format
#' @noRd
parse_paypal_data <- function(data, format_info, include_pending, timezone) {
  # Get column values safely
  get_col <- function(col_name) {
    if (is.null(col_name) || !col_name %in% names(data)) {
      return(rep(NA_character_, nrow(data)))
    }
    data[[col_name]]
  }

  # Parse date
  date_str <- get_col(format_info$date_col)
  if (!is.null(format_info$time_col)) {
    time_str <- get_col(format_info$time_col)
    time_str <- dplyr::if_else(is.na(time_str), "00:00:00", time_str)
    datetime_str <- paste(date_str, time_str)
  } else {
    datetime_str <- date_str
  }

  # Try multiple date formats
  parsed_date <- suppressWarnings(lubridate::mdy_hms(datetime_str, tz = timezone))
  if (all(is.na(parsed_date))) {
    parsed_date <- suppressWarnings(lubridate::ymd_hms(datetime_str, tz = timezone))
  }
  if (all(is.na(parsed_date))) {
    parsed_date <- suppressWarnings(lubridate::dmy_hms(datetime_str, tz = timezone))
  }
  if (all(is.na(parsed_date))) {
    parsed_date <- suppressWarnings(lubridate::mdy(date_str, tz = timezone))
  }
  if (all(is.na(parsed_date))) {
    parsed_date <- suppressWarnings(lubridate::ymd(date_str, tz = timezone))
  }

  # Parse amounts (remove currency symbols and commas)
  parse_amount <- function(x) {
    x <- gsub("[^0-9.,-]", "", x)
    x <- gsub(",", "", x)
    as.numeric(x)
  }

  gross <- parse_amount(get_col(format_info$gross_col))
  fee <- parse_amount(get_col(format_info$fee_col))
  fee <- dplyr::if_else(is.na(fee), 0, fee)

  # Build result
  result <- tibble::tibble(
    date = as.Date(parsed_date),
    description = dplyr::coalesce(
      get_col(format_info$name_col),
      get_col(format_info$item_col),
      "PayPal Transaction"
    ),
    amount = gross,
    currency = toupper(gsub("[^A-Za-z]", "", get_col(format_info$currency_col))),
    external_id = get_col(format_info$txn_id_col),
    account = "PayPal",
    memo = dplyr::coalesce(get_col(format_info$note_col), NA_character_),
    category = categorize_paypal_type(get_col(format_info$type_col)),
    fee = fee,
    status = get_col(format_info$status_col)
  )

  # Filter by status if needed
  if (!include_pending) {
    result <- result |>
      dplyr::filter(
        is.na(status) |
        tolower(status) %in% c("completed", "cleared", "processed")
      )
  }

  result |>
    dplyr::select(-status) |>
    dplyr::filter(!is.na(date))
}


#' Categorize PayPal Transaction Type
#'
#' @param type_vec Vector of PayPal transaction types
#' @return Vector of standardized categories
#' @noRd
categorize_paypal_type <- function(type_vec) {
  type_lower <- tolower(type_vec)

  dplyr::case_when(
    grepl("payment|purchase", type_lower) ~ "payment",
    grepl("refund", type_lower) ~ "refund",
    grepl("fee", type_lower) ~ "fee",
    grepl("transfer|withdraw", type_lower) ~ "transfer",
    grepl("currency|conversion", type_lower) ~ "conversion",
    grepl("hold", type_lower) ~ "hold",
    grepl("reversal", type_lower) ~ "reversal",
    TRUE ~ "other"
  )
}


# =============================================================================
# Stripe Importer
# =============================================================================

#' Import Stripe Balance Transactions CSV
#'
#' Imports transaction data from a Stripe balance transactions export.
#'
#' @param path Path to the Stripe CSV file
#' @param include_fees If TRUE, include fee transactions separately (default TRUE)
#' @param timezone Timezone for date conversion (default "UTC")
#' @return A tibble with class `gnucashr_csv_import` containing standardized columns
#' @export
#' @examples
#' \dontrun{
#' stripe_data <- import_stripe_csv("stripe_balance.csv")
#' print(stripe_data)
#' }
import_stripe_csv <- function(path, include_fees = TRUE, timezone = "UTC") {
  if (!file.exists(path)) {
    rlang::abort(paste("Stripe CSV file not found:", path))
  }

  raw_data <- readr::read_csv(
    path,
    col_types = readr::cols(.default = readr::col_character()),
    show_col_types = FALSE
  )

  if (nrow(raw_data) == 0) {
    rlang::abort("Stripe CSV file is empty")
  }

  # Detect Stripe format
  format_info <- detect_stripe_format(names(raw_data))

  # Parse data
  result <- parse_stripe_data(raw_data, format_info, include_fees, timezone)

  metadata <- list(
    path = normalizePath(path),
    format = format_info$format_name,
    original_rows = nrow(raw_data),
    imported_rows = nrow(result),
    include_fees = include_fees
  )

  new_csv_import(result, "stripe", metadata)
}


#' Detect Stripe CSV Format
#'
#' @param col_names Vector of column names
#' @return List with format info
#' @noRd
detect_stripe_format <- function(col_names) {
  col_names_lower <- tolower(col_names)

  # Standard Stripe balance export
  if (any(grepl("created", col_names_lower)) &&
      any(grepl("amount", col_names_lower))) {

    # Find actual column names
    find_col <- function(pattern) {
      idx <- grep(pattern, col_names, ignore.case = TRUE)
      if (length(idx) > 0) col_names[idx[1]] else NULL
    }

    return(list(
      format_name = "standard",
      id_col = find_col("^id$"),
      created_col = find_col("created"),
      amount_col = find_col("^amount$"),
      fee_col = find_col("^fee$"),
      net_col = find_col("^net$"),
      currency_col = find_col("currency"),
      description_col = find_col("description"),
      type_col = find_col("^type$"),
      status_col = find_col("status"),
      customer_col = find_col("customer"),
      source_col = find_col("source")
    ))
  }

  # Stripe Payments export (different format)
  if (any(grepl("payment.*id", col_names_lower)) ||
      any(grepl("charge.*id", col_names_lower))) {

    find_col <- function(pattern) {
      idx <- grep(pattern, col_names, ignore.case = TRUE)
      if (length(idx) > 0) col_names[idx[1]] else NULL
    }

    return(list(
      format_name = "payments",
      id_col = find_col("(payment|charge).*id"),
      created_col = find_col("created|date"),
      amount_col = find_col("amount"),
      fee_col = find_col("fee"),
      net_col = find_col("net"),
      currency_col = find_col("currency"),
      description_col = find_col("description"),
      type_col = NULL,
      status_col = find_col("status"),
      customer_col = find_col("customer"),
      source_col = NULL
    ))
  }

  rlang::abort(paste(
    "Unrecognized Stripe CSV format. Expected columns include:",
    "id, Created (UTC), Amount, Fee, Net, Currency, Description"
  ))
}


#' Parse Stripe Data
#'
#' @param data Raw tibble
#' @param format_info Format detection result
#' @param include_fees Include separate fee entries
#' @param timezone Timezone
#' @return Tibble in standard format
#' @noRd
parse_stripe_data <- function(data, format_info, include_fees, timezone) {
  get_col <- function(col_name) {
    if (is.null(col_name) || !col_name %in% names(data)) {
      return(rep(NA_character_, nrow(data)))
    }
    data[[col_name]]
  }

  # Parse date (Stripe uses ISO 8601 format)
  date_str <- get_col(format_info$created_col)
  parsed_date <- suppressWarnings(lubridate::ymd_hms(date_str, tz = timezone))
  if (all(is.na(parsed_date))) {
    parsed_date <- suppressWarnings(lubridate::parse_date_time(
      date_str,
      orders = c("ymd HMS", "ymd HM", "ymd", "mdy HMS", "mdy"),
      tz = timezone
    ))
  }

  # Parse amounts (Stripe amounts are in cents for most currencies)
  parse_amount <- function(x) {
    x <- gsub("[^0-9.,-]", "", x)
    x <- gsub(",", "", x)
    amt <- as.numeric(x)
    # Convert from cents if values look like cents (integers > 100)
    # This is a heuristic - Stripe exports vary
    amt
  }

  amount <- parse_amount(get_col(format_info$amount_col))
  fee <- parse_amount(get_col(format_info$fee_col))
  fee <- dplyr::if_else(is.na(fee), 0, fee)

  # Stripe amounts might be in cents - detect and convert
  # If currency is in the data and amounts look like cents, convert
  currency <- toupper(gsub("[^A-Za-z]", "", get_col(format_info$currency_col)))

  # For USD, EUR, etc. amounts in cents should be divided by 100
  # Check if amounts look like cents (all integers, no decimals in original)
  raw_amount <- get_col(format_info$amount_col)
  is_cents <- all(grepl("^-?\\d+$", na.omit(raw_amount)))
  if (is_cents && median(abs(na.omit(amount)), na.rm = TRUE) > 100) {
    amount <- amount / 100
    fee <- fee / 100
  }

  # Build transaction type/category
  type_raw <- get_col(format_info$type_col)
  category <- categorize_stripe_type(type_raw)

  # Main transactions
  result <- tibble::tibble(
    date = as.Date(parsed_date),
    description = dplyr::coalesce(
      get_col(format_info$description_col),
      paste("Stripe", category)
    ),
    amount = amount,
    currency = currency,
    external_id = get_col(format_info$id_col),
    account = "Stripe",
    memo = dplyr::coalesce(get_col(format_info$customer_col), NA_character_),
    category = category,
    fee = fee
  )

  result <- result |>
    dplyr::filter(!is.na(date))

  result
}


#' Categorize Stripe Transaction Type
#'
#' @param type_vec Vector of Stripe types
#' @return Vector of standardized categories
#' @noRd
categorize_stripe_type <- function(type_vec) {
  type_lower <- tolower(type_vec)

  dplyr::case_when(
    grepl("charge|payment", type_lower) ~ "charge",
    grepl("refund", type_lower) ~ "refund",
    grepl("payout|transfer", type_lower) ~ "payout",
    grepl("fee|stripe_fee", type_lower) ~ "fee",
    grepl("adjustment", type_lower) ~ "adjustment",
    grepl("dispute", type_lower) ~ "dispute",
    is.na(type_lower) ~ "charge",
    TRUE ~ "other"
  )
}


# =============================================================================
# QuickBooks Importer
# =============================================================================

#' Import QuickBooks Transaction CSV
#'
#' Imports transaction data from QuickBooks Online or Desktop export.
#' Auto-detects the format from header columns.
#'
#' @param path Path to the QuickBooks CSV file
#' @param format One of "auto", "qbo" (QuickBooks Online), or "desktop"
#' @param timezone Timezone for date conversion (default from system)
#' @return A tibble with class `gnucashr_csv_import` containing standardized columns
#' @export
#' @examples
#' \dontrun{
#' qb_data <- import_quickbooks_csv("qb_transactions.csv")
#' print(qb_data)
#' }
import_quickbooks_csv <- function(path, format = c("auto", "qbo", "desktop"),
                                   timezone = Sys.timezone()) {
  format <- match.arg(format)

  if (!file.exists(path)) {
    rlang::abort(paste("QuickBooks CSV file not found:", path))
  }

  raw_data <- readr::read_csv(
    path,
    col_types = readr::cols(.default = readr::col_character()),
    show_col_types = FALSE
  )

  if (nrow(raw_data) == 0) {
    rlang::abort("QuickBooks CSV file is empty")
  }

  # Detect or use specified format
  format_info <- detect_quickbooks_format(names(raw_data), format)

  # Parse data
  result <- parse_quickbooks_data(raw_data, format_info, timezone)

  metadata <- list(
    path = normalizePath(path),
    format = format_info$format_name,
    original_rows = nrow(raw_data),
    imported_rows = nrow(result)
  )

  new_csv_import(result, "quickbooks", metadata)
}


#' Detect QuickBooks CSV Format
#'
#' @param col_names Vector of column names
#' @param requested_format User-specified format or "auto"
#' @return List with format info
#' @noRd
detect_quickbooks_format <- function(col_names, requested_format) {
  col_names_lower <- tolower(col_names)

  find_col <- function(patterns) {
    for (pat in patterns) {
      idx <- grep(pat, col_names, ignore.case = TRUE)
      if (length(idx) > 0) return(col_names[idx[1]])
    }
    NULL
  }

  # QuickBooks Online format
  is_qbo <- any(grepl("transaction type", col_names_lower)) ||
    any(grepl("split", col_names_lower))

  # QuickBooks Desktop format (IIF-like CSV)
  is_desktop <- any(grepl("^trans$|^trns$", col_names_lower)) ||
    (any(grepl("^num$", col_names_lower)) && any(grepl("^name$", col_names_lower)))

  detected_format <- if (is_qbo) "qbo" else if (is_desktop) "desktop" else "qbo"

  # Use requested if not auto
  if (requested_format != "auto") {
    detected_format <- requested_format
  }

  if (detected_format == "qbo") {
    return(list(
      format_name = "qbo",
      date_col = find_col(c("^date$", "transaction date", "txn date")),
      type_col = find_col(c("transaction type", "type")),
      num_col = find_col(c("^num$", "number", "ref")),
      name_col = find_col(c("^name$", "payee", "customer")),
      memo_col = find_col(c("memo", "description")),
      account_col = find_col(c("^account$", "category")),
      amount_col = find_col(c("^amount$", "total")),
      debit_col = find_col(c("debit", "charge")),
      credit_col = find_col(c("credit", "payment"))
    ))
  } else {
    return(list(
      format_name = "desktop",
      date_col = find_col(c("^date$", "trans date")),
      type_col = find_col(c("^type$", "trans type")),
      num_col = find_col(c("^num$", "number")),
      name_col = find_col(c("^name$", "payee")),
      memo_col = find_col(c("memo", "description")),
      account_col = find_col(c("^account$", "acct")),
      amount_col = find_col(c("^amount$")),
      debit_col = find_col(c("debit")),
      credit_col = find_col(c("credit"))
    ))
  }
}


#' Parse QuickBooks Data
#'
#' @param data Raw tibble
#' @param format_info Format detection result
#' @param timezone Timezone
#' @return Tibble in standard format
#' @noRd
parse_quickbooks_data <- function(data, format_info, timezone) {
  get_col <- function(col_name) {
    if (is.null(col_name) || !col_name %in% names(data)) {
      return(rep(NA_character_, nrow(data)))
    }
    data[[col_name]]
  }

  # Parse date (QuickBooks uses various formats)
  date_str <- get_col(format_info$date_col)
  parsed_date <- suppressWarnings(lubridate::mdy(date_str, tz = timezone))
  if (all(is.na(parsed_date))) {
    parsed_date <- suppressWarnings(lubridate::ymd(date_str, tz = timezone))
  }
  if (all(is.na(parsed_date))) {
    parsed_date <- suppressWarnings(lubridate::dmy(date_str, tz = timezone))
  }

  # Parse amount - QB may have separate debit/credit or single amount
  parse_amount <- function(x) {
    x <- gsub("[^0-9.,-]", "", x)
    x <- gsub(",", "", x)
    as.numeric(x)
  }

  amount_raw <- get_col(format_info$amount_col)
  debit_raw <- get_col(format_info$debit_col)
  credit_raw <- get_col(format_info$credit_col)

  # Prefer single amount, fall back to debit - credit
  if (!all(is.na(amount_raw))) {
    amount <- parse_amount(amount_raw)
  } else {
    debit <- parse_amount(debit_raw)
    credit <- parse_amount(credit_raw)
    debit <- dplyr::if_else(is.na(debit), 0, debit)
    credit <- dplyr::if_else(is.na(credit), 0, credit)
    amount <- credit - debit
  }

  # Categorize QuickBooks transaction types
  type_raw <- get_col(format_info$type_col)
  category <- categorize_quickbooks_type(type_raw)

  # Generate external_id from row number if no ref number
  num_raw <- get_col(format_info$num_col)
  external_id <- dplyr::if_else(
    is.na(num_raw) | num_raw == "",
    paste0("qb_", seq_len(nrow(data))),
    paste0("qb_", num_raw)
  )

  result <- tibble::tibble(
    date = as.Date(parsed_date),
    description = dplyr::coalesce(
      get_col(format_info$name_col),
      get_col(format_info$memo_col),
      "QuickBooks Transaction"
    ),
    amount = amount,
    currency = "USD",  # QuickBooks doesn't typically include currency in CSV
    external_id = external_id,
    account = dplyr::coalesce(get_col(format_info$account_col), "QuickBooks"),
    memo = get_col(format_info$memo_col),
    category = category
  )

  result |>
    dplyr::filter(!is.na(date))
}


#' Categorize QuickBooks Transaction Type
#'
#' @param type_vec Vector of QuickBooks types
#' @return Vector of standardized categories
#' @noRd
categorize_quickbooks_type <- function(type_vec) {
  type_lower <- tolower(type_vec)

  dplyr::case_when(
    grepl("invoice", type_lower) ~ "invoice",
    grepl("payment", type_lower) ~ "payment",
    grepl("bill|expense", type_lower) ~ "expense",
    grepl("check", type_lower) ~ "check",
    grepl("transfer", type_lower) ~ "transfer",
    grepl("journal", type_lower) ~ "journal",
    grepl("deposit", type_lower) ~ "deposit",
    grepl("refund|credit", type_lower) ~ "refund",
    grepl("sales", type_lower) ~ "sale",
    is.na(type_lower) ~ "other",
    TRUE ~ "other"
  )
}


# =============================================================================
# Utility Functions
# =============================================================================

#' Combine Multiple CSV Imports
#'
#' Merges multiple gnucashr CSV import objects into a single tibble,
#' preserving metadata about the source of each transaction.
#'
#' @param ... Multiple `gnucashr_csv_import` objects
#' @param deduplicate If TRUE, attempt to remove duplicate transactions (default FALSE)
#' @param dedupe_key Columns to use for deduplication (default: date, amount, description)
#' @return A combined tibble with class `gnucashr_csv_import`
#' @export
#' @examples
#' \dontrun{
#' paypal <- import_paypal_csv("paypal.csv")
#' stripe <- import_stripe_csv("stripe.csv")
#' combined <- combine_csv_imports(paypal, stripe)
#' }
combine_csv_imports <- function(..., deduplicate = FALSE,
                                 dedupe_key = c("date", "amount", "description")) {
  imports <- list(...)

  # Validate inputs
  for (i in seq_along(imports)) {
    if (!is_csv_import(imports[[i]])) {
      rlang::abort(sprintf(
        "Argument %d is not a gnucashr_csv_import object", i
      ))
    }
  }

  if (length(imports) == 0) {
    rlang::abort("No imports provided to combine")
  }

  # Add source tracking column if not present
  imports <- lapply(imports, function(x) {
    if (!"source" %in% names(x)) {
      x$source <- attr(x, "source_type")
    }
    # Keep only standard columns plus source
    x |> dplyr::select(
      date, description, amount, currency, external_id, account, memo, category,
      dplyr::any_of(c("source", "fee"))
    )
  })

  # Combine
  combined <- dplyr::bind_rows(imports)

  # Deduplicate if requested
  if (deduplicate && nrow(combined) > 0) {
    valid_keys <- intersect(dedupe_key, names(combined))
    if (length(valid_keys) > 0) {
      combined <- combined |>
        dplyr::distinct(dplyr::across(dplyr::all_of(valid_keys)), .keep_all = TRUE)
    }
  }

  # Sort by date
  combined <- combined |>
    dplyr::arrange(date, description)

  # Create combined metadata
  sources <- sapply(imports, function(x) attr(x, "source_type") %||% "unknown")
  metadata <- list(
    combined_from = unique(sources),
    total_sources = length(imports),
    total_rows = nrow(combined),
    deduplicated = deduplicate
  )

  new_csv_import(combined, "combined", metadata)
}


#' Preview CSV Column Mapping
#'
#' Debug helper to show how columns from a CSV file would be mapped
#' by one of the import functions.
#'
#' @param path Path to CSV file
#' @param import_fn Import function to use (e.g., import_paypal_csv)
#' @return A tibble showing column mappings and sample values
#' @export
#' @examples
#' \dontrun{
#' # Preview how PayPal columns will be mapped
#' preview_csv_mapping("paypal.csv", import_paypal_csv)
#' }
preview_csv_mapping <- function(path, import_fn) {
  if (!file.exists(path)) {
    rlang::abort(paste("CSV file not found:", path))
  }

  # Read first few rows
  raw_data <- readr::read_csv(
    path,
    col_types = readr::cols(.default = readr::col_character()),
    n_max = 5,
    show_col_types = FALSE
  )

  # Determine source type from function name
  fn_name <- as.character(substitute(import_fn))
  if (length(fn_name) > 1) fn_name <- fn_name[length(fn_name)]

  source_type <- dplyr::case_when(
    grepl("paypal", fn_name, ignore.case = TRUE) ~ "paypal",
    grepl("stripe", fn_name, ignore.case = TRUE) ~ "stripe",
    grepl("quickbooks|qbo", fn_name, ignore.case = TRUE) ~ "quickbooks",
    TRUE ~ "unknown"
  )

  # Get format info
  format_info <- tryCatch({
    switch(source_type,
      "paypal" = detect_paypal_format(names(raw_data)),
      "stripe" = detect_stripe_format(names(raw_data)),
      "quickbooks" = detect_quickbooks_format(names(raw_data), "auto"),
      list(format_name = "unknown")
    )
  }, error = function(e) {
    list(format_name = "error", error = conditionMessage(e))
  })

  cat("CSV Preview for:", path, "\n")
  cat("Detected source:", source_type, "\n")
  cat("Detected format:", format_info$format_name, "\n\n")

  cat("Columns found:\n")
  for (col in names(raw_data)) {
    sample_val <- raw_data[[col]][1]
    if (is.na(sample_val)) sample_val <- "(NA)"
    cat(sprintf("  %-25s : %s\n", col, substr(sample_val, 1, 40)))
  }

  cat("\nColumn mappings:\n")
  for (mapping in names(format_info)) {
    if (mapping != "format_name" && !is.null(format_info[[mapping]])) {
      target_col <- gsub("_col$", "", mapping)
      cat(sprintf("  %-20s -> %s\n", format_info[[mapping]], target_col))
    }
  }

  # Try actual import
  cat("\nAttempting import...\n")
  result <- tryCatch({
    import_fn(path)
  }, error = function(e) {
    cat("Import error:", conditionMessage(e), "\n")
    NULL
  })

  if (!is.null(result)) {
    cat("Success! Imported", nrow(result), "rows\n\n")
    cat("First few rows:\n")
    print(utils::head(result, 3))
  }

  invisible(result)
}


#' Validate CSV Import
#'
#' Check a CSV import for common issues like missing required fields,
#' invalid amounts, or suspicious duplicates.
#'
#' @param import A `gnucashr_csv_import` object
#' @param strict If TRUE, return errors instead of warnings
#' @return List with validation results
#' @export
#' @examples
#' \dontrun{
#' import <- import_paypal_csv("paypal.csv")
#' validation <- validate_csv_import(import)
#' }
validate_csv_import <- function(import, strict = FALSE) {
  if (!is_csv_import(import)) {
    rlang::abort("Input must be a gnucashr_csv_import object")
  }

  issues <- list()

  # Check for missing dates
  missing_dates <- sum(is.na(import$date))
  if (missing_dates > 0) {
    issues$missing_dates <- sprintf("%d rows with missing dates", missing_dates)
  }

  # Check for missing amounts
  missing_amounts <- sum(is.na(import$amount))
  if (missing_amounts > 0) {
    issues$missing_amounts <- sprintf("%d rows with missing amounts", missing_amounts)
  }

  # Check for zero amounts (might be intentional but worth flagging)
  zero_amounts <- sum(import$amount == 0, na.rm = TRUE)
  if (zero_amounts > 0) {
    issues$zero_amounts <- sprintf("%d rows with zero amounts", zero_amounts)
  }

  # Check for missing external IDs
  missing_ids <- sum(is.na(import$external_id) | import$external_id == "")
  if (missing_ids > 0) {
    issues$missing_ids <- sprintf("%d rows with missing external IDs", missing_ids)
  }

  # Check for potential duplicates
  dupes <- import |>
    dplyr::group_by(date, amount, description) |>
    dplyr::filter(dplyr::n() > 1) |>
    dplyr::ungroup()

  if (nrow(dupes) > 0) {
    issues$potential_duplicates <- sprintf(
      "%d rows appear to be duplicates", nrow(dupes)
    )
  }

  # Check currency consistency
  currencies <- unique(import$currency)
  if (length(currencies) > 1) {
    issues$multiple_currencies <- sprintf(
      "Multiple currencies found: %s",
      paste(currencies, collapse = ", ")
    )
  }

  # Report
  result <- list(
    valid = length(issues) == 0,
    row_count = nrow(import),
    source_type = attr(import, "source_type"),
    issues = issues
  )

  if (length(issues) > 0) {
    msg <- paste(
      sprintf("CSV import validation found %d issues:", length(issues)),
      paste(sprintf("- %s", unlist(issues)), collapse = "\n"),
      sep = "\n"
    )

    if (strict) {
      rlang::abort(msg)
    } else {
      rlang::warn(msg)
    }
  }

  result
}
