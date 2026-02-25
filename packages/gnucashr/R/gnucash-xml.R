#' GnuCash XML Parser
#'
#' Functions for reading GnuCash XML format files (compressed or uncompressed).
#' GnuCash XML files are typically gzip-compressed by default.
#'
#' @name gnucash-xml
NULL

#' Parse GnuCash XML File
#'
#' Read and parse a GnuCash XML file into tibbles.
#' Handles both compressed (.gnucash) and uncompressed (.gnucash.xml) files.
#'
#' @param path Path to GnuCash XML file
#' @return List containing tibbles: accounts, transactions, splits, commodities, prices
#' @export
parse_gnucash_xml <- function(path) {
  if (!file.exists(path)) {
    rlang::abort(paste("GnuCash file not found:", path))
  }

  # Determine if compressed
  raw_bytes <- readBin(path, "raw", n = 2)
  is_gzip <- identical(raw_bytes, as.raw(c(0x1f, 0x8b)))

  # Read XML content
  if (is_gzip) {
    con <- gzfile(path, "rb")
    on.exit(close(con))
    xml_content <- readLines(con, warn = FALSE)
    xml_text <- paste(xml_content, collapse = "\n")
    doc <- xml2::read_xml(xml_text)
  } else {
    doc <- xml2::read_xml(path)
  }

  # Define namespaces used in GnuCash XML
  ns <- xml2::xml_ns(doc)

  # Parse each entity type
  list(
    book_guid = parse_book_guid(doc, ns),
    accounts = parse_xml_accounts(doc, ns),
    transactions = parse_xml_transactions(doc, ns),
    splits = parse_xml_splits(doc, ns),
    commodities = parse_xml_commodities(doc, ns),
    prices = parse_xml_prices(doc, ns)
  )
}

#' Parse Book GUID from XML
#' @noRd
parse_book_guid <- function(doc, ns) {
  book_node <- xml2::xml_find_first(doc, ".//gnc:book", ns)
  if (is.na(book_node)) return(NA_character_)

  guid_node <- xml2::xml_find_first(book_node, ".//book:id[@type='guid']", ns)
  if (is.na(guid_node)) {
    # Try alternative path
    guid_node <- xml2::xml_find_first(book_node, ".//book:id", ns)
  }
  if (is.na(guid_node)) return(NA_character_)
  xml2::xml_text(guid_node)
}

#' Parse Accounts from XML
#' @noRd
parse_xml_accounts <- function(doc, ns) {
  account_nodes <- xml2::xml_find_all(doc, ".//gnc:account", ns)

  if (length(account_nodes) == 0) {
    return(tibble::tibble(
      guid = character(),
      name = character(),
      account_type = character(),
      commodity_guid = character(),
      parent_guid = character(),
      description = character(),
      code = character(),
      hidden = logical(),
      placeholder = logical()
    ))
  }

  purrr::map_dfr(account_nodes, function(node) {
    tibble::tibble(
      guid = xml_child_text(node, ".//act:id[@type='guid']", ns),
      name = xml_child_text(node, ".//act:name", ns),
      account_type = xml_child_text(node, ".//act:type", ns),
      commodity_guid = xml_child_text(node, ".//act:commodity/cmdty:id", ns),
      commodity_space = xml_child_text(node, ".//act:commodity/cmdty:space", ns),
      parent_guid = xml_child_text(node, ".//act:parent[@type='guid']", ns),
      description = xml_child_text(node, ".//act:description", ns),
      code = xml_child_text(node, ".//act:code", ns),
      hidden = xml_child_text(node, ".//act:hidden", ns) == "true",
      placeholder = xml_child_text(node, ".//act:placeholder", ns) == "true"
    )
  })
}

#' Parse Transactions from XML
#' @noRd
parse_xml_transactions <- function(doc, ns) {
  txn_nodes <- xml2::xml_find_all(doc, ".//gnc:transaction", ns)

  if (length(txn_nodes) == 0) {
    return(tibble::tibble(
      guid = character(),
      currency_guid = character(),
      num = character(),
      post_date = as.POSIXct(character()),
      enter_date = as.POSIXct(character()),
      description = character()
    ))
  }

  purrr::map_dfr(txn_nodes, function(node) {
    post_date_raw <- xml_child_text(node, ".//trn:date-posted/ts:date", ns)
    enter_date_raw <- xml_child_text(node, ".//trn:date-entered/ts:date", ns)

    tibble::tibble(
      guid = xml_child_text(node, ".//trn:id[@type='guid']", ns),
      currency_guid = xml_child_text(node, ".//trn:currency/cmdty:id", ns),
      currency_space = xml_child_text(node, ".//trn:currency/cmdty:space", ns),
      num = xml_child_text(node, ".//trn:num", ns),
      post_date = parse_gnucash_date(post_date_raw),
      enter_date = parse_gnucash_date(enter_date_raw),
      description = xml_child_text(node, ".//trn:description", ns)
    )
  })
}

#' Parse Splits from XML
#' @noRd
parse_xml_splits <- function(doc, ns) {
  txn_nodes <- xml2::xml_find_all(doc, ".//gnc:transaction", ns)

  if (length(txn_nodes) == 0) {
    return(tibble::tibble(
      guid = character(),
      tx_guid = character(),
      account_guid = character(),
      memo = character(),
      action = character(),
      reconcile_state = character(),
      value_num = integer(),
      value_denom = integer(),
      quantity_num = integer(),
      quantity_denom = integer()
    ))
  }

  purrr::map_dfr(txn_nodes, function(txn_node) {
    tx_guid <- xml_child_text(txn_node, ".//trn:id[@type='guid']", ns)
    split_nodes <- xml2::xml_find_all(txn_node, ".//trn:split", ns)

    purrr::map_dfr(split_nodes, function(split_node) {
      value_raw <- xml_child_text(split_node, ".//split:value", ns)
      quantity_raw <- xml_child_text(split_node, ".//split:quantity", ns)

      value_parts <- parse_fraction_string(value_raw)
      quantity_parts <- parse_fraction_string(quantity_raw)

      tibble::tibble(
        guid = xml_child_text(split_node, ".//split:id[@type='guid']", ns),
        tx_guid = tx_guid,
        account_guid = xml_child_text(split_node, ".//split:account[@type='guid']", ns),
        memo = xml_child_text(split_node, ".//split:memo", ns),
        action = xml_child_text(split_node, ".//split:action", ns),
        reconcile_state = xml_child_text(split_node, ".//split:reconciled-state", ns),
        value_num = value_parts$num,
        value_denom = value_parts$denom,
        quantity_num = quantity_parts$num,
        quantity_denom = quantity_parts$denom
      )
    })
  })
}

#' Parse Commodities from XML
#' @noRd
parse_xml_commodities <- function(doc, ns) {
  cmdty_nodes <- xml2::xml_find_all(doc, ".//gnc:commodity", ns)

  if (length(cmdty_nodes) == 0) {
    return(tibble::tibble(
      guid = character(),
      namespace = character(),
      mnemonic = character(),
      fullname = character(),
      cusip = character(),
      fraction = integer()
    ))
  }

  purrr::map_dfr(cmdty_nodes, function(node) {
    # Generate guid from space:id combination
    space <- xml_child_text(node, ".//cmdty:space", ns)
    id <- xml_child_text(node, ".//cmdty:id", ns)
    guid <- paste0(space, ":", id)

    fraction_raw <- xml_child_text(node, ".//cmdty:fraction", ns)

    tibble::tibble(
      guid = guid,
      namespace = space,
      mnemonic = id,
      fullname = xml_child_text(node, ".//cmdty:name", ns),
      cusip = xml_child_text(node, ".//cmdty:xcode", ns),
      fraction = if (!is.na(fraction_raw) && nchar(fraction_raw) > 0) as.integer(fraction_raw) else 100L
    )
  })
}

#' Parse Prices from XML
#' @noRd
parse_xml_prices <- function(doc, ns) {
  price_nodes <- xml2::xml_find_all(doc, ".//price", ns)

  if (length(price_nodes) == 0) {
    return(tibble::tibble(
      guid = character(),
      commodity_guid = character(),
      currency_guid = character(),
      date = as.POSIXct(character()),
      source = character(),
      type = character(),
      value_num = integer(),
      value_denom = integer()
    ))
  }

  purrr::map_dfr(price_nodes, function(node) {
    date_raw <- xml_child_text(node, ".//price:time/ts:date", ns)
    value_raw <- xml_child_text(node, ".//price:value", ns)
    value_parts <- parse_fraction_string(value_raw)

    tibble::tibble(
      guid = xml_child_text(node, ".//price:id[@type='guid']", ns),
      commodity_guid = xml_child_text(node, ".//price:commodity/cmdty:id", ns),
      currency_guid = xml_child_text(node, ".//price:currency/cmdty:id", ns),
      date = parse_gnucash_date(date_raw),
      source = xml_child_text(node, ".//price:source", ns),
      type = xml_child_text(node, ".//price:type", ns),
      value_num = value_parts$num,
      value_denom = value_parts$denom
    )
  })
}

# Helper Functions ----

#' Extract text from XML child node
#' @noRd
xml_child_text <- function(node, xpath, ns) {
  child <- xml2::xml_find_first(node, xpath, ns)
  if (is.na(child)) return(NA_character_)
  text <- xml2::xml_text(child)
  if (nchar(text) == 0) return(NA_character_)
  text
}

#' Parse GnuCash date string
#' @noRd
parse_gnucash_date <- function(date_str) {
  if (is.na(date_str) || nchar(date_str) == 0) {
    return(as.POSIXct(NA))
  }
  # GnuCash format: "2024-01-15 12:00:00 -0500" or "2024-01-15 12:00:00"
  # Remove timezone offset for parsing
  clean_str <- sub(" [+-]\\d{4}$", "", date_str)
  tryCatch(
    as.POSIXct(clean_str, format = "%Y-%m-%d %H:%M:%S"),
    error = function(e) as.POSIXct(NA)
  )
}

#' Parse fraction string (e.g., "12345/100")
#' @noRd
parse_fraction_string <- function(frac_str) {
  if (is.na(frac_str) || nchar(frac_str) == 0) {
    return(list(num = NA_integer_, denom = NA_integer_))
  }
  parts <- strsplit(frac_str, "/")[[1]]
  if (length(parts) == 2) {
    list(
      num = as.integer(parts[1]),
      denom = as.integer(parts[2])
    )
  } else {
    list(num = NA_integer_, denom = NA_integer_)
  }
}

#' Detect GnuCash File Format
#'
#' Determine if a GnuCash file is SQLite or XML format.
#'
#' @param path Path to GnuCash file
#' @return Character: "sqlite", "xml-gz", "xml", or "unknown"
#' @export
detect_gnucash_format <- function(path) {
  if (!file.exists(path)) {
    rlang::abort(paste("File not found:", path))
  }

  # Read first 16 bytes to detect format
  raw_bytes <- readBin(path, "raw", n = 16)

  # Check for SQLite header: "SQLite format 3\0"
  sqlite_magic <- charToRaw("SQLite format 3")
  if (length(raw_bytes) >= 15 && identical(raw_bytes[1:15], sqlite_magic)) {
    return("sqlite")
  }

  # Check for gzip header: 1f 8b
  if (length(raw_bytes) >= 2 && identical(raw_bytes[1:2], as.raw(c(0x1f, 0x8b)))) {
    return("xml-gz")
  }

  # Check for XML header: <?xml
  xml_magic <- charToRaw("<?xml")
  if (length(raw_bytes) >= 5 && identical(raw_bytes[1:5], xml_magic)) {
    return("xml")
  }

  "unknown"
}
