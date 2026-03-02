#' Commodity R6 Class
#'
#' R6 class representing a GnuCash commodity (currency, stock, mutual fund, etc.).
#' Commodities are identified by their namespace and mnemonic (e.g., "CURRENCY:USD").
#'
#' @export
Commodity <- R6::R6Class("Commodity",
  public = list(
    #' @description Create a new Commodity object
    #' @param guid Unique identifier (32-character hex)
    #' @param namespace Commodity namespace (CURRENCY, NASDAQ, NYSE, etc.)
    #' @param mnemonic Short identifier (USD, AAPL, etc.)
    #' @param fullname Full descriptive name
    #' @param cusip CUSIP/ISIN identifier for securities
    #' @param fraction Smallest unit (100 for USD = cents)
    #' @param quote_flag Whether to fetch online quotes
    #' @param quote_source Source for online quotes
    #' @param quote_tz Timezone for quotes
    initialize = function(guid = NULL,
                         namespace = "CURRENCY",
                         mnemonic,
                         fullname = NA_character_,
                         cusip = NA_character_,
                         fraction = 100L,
                         quote_flag = FALSE,
                         quote_source = NA_character_,
                         quote_tz = NA_character_) {
      if (is.null(guid)) {
        guid <- generate_guid()
      }

      if (!validate_guid(guid)) {
        rlang::abort("Invalid GUID format")
      }

      private$.guid <- guid
      private$.namespace <- namespace
      private$.mnemonic <- mnemonic
      private$.fullname <- fullname
      private$.cusip <- cusip
      private$.fraction <- as.integer(fraction)
      private$.quote_flag <- quote_flag
      private$.quote_source <- quote_source
      private$.quote_tz <- quote_tz

      invisible(self)
    },

    #' @description Get commodity identifier (namespace:mnemonic)
    identifier = function() {
      paste0(private$.namespace, ":", private$.mnemonic)
    },

    #' @description Check if this is a currency
    is_currency = function() {
      private$.namespace %in% c("CURRENCY", "ISO4217")
    },

    #' @description Check if this is a security (stock, mutual fund, etc.)
    is_security = function() {
      !self$is_currency() && private$.namespace != "template"
    },

    #' @description Get number of decimal places
    decimal_places = function() {
      floor(log10(private$.fraction))
    },

    #' @description Convert amount to smallest unit (e.g., dollars to cents)
    #' @param amount Numeric amount to convert
    to_smallest_unit = function(amount) {
      as.integer(round(amount * private$.fraction))
    },

    #' @description Convert from smallest unit to decimal
    #' @param amount Integer amount in smallest units
    from_smallest_unit = function(amount) {
      amount / private$.fraction
    },

    #' @description Format amount according to commodity precision
    #' @param amount Numeric amount
    #' @param include_symbol Include currency symbol
    format_amount = function(amount, include_symbol = TRUE) {
      decimals <- self$decimal_places()
      formatted <- formatC(amount, format = "f", digits = decimals, big.mark = ",")

      if (include_symbol && self$is_currency()) {
        # Common currency symbols
        symbols <- c(
          USD = "$", EUR = "\u20AC", GBP = "\u00A3", JPY = "\u00A5",
          CAD = "CA$", AUD = "A$", CHF = "CHF "
        )
        symbol <- symbols[private$.mnemonic]
        if (!is.na(symbol)) {
          return(paste0(symbol, formatted))
        }
      }
      formatted
    },

    #' @description Convert to data frame row
    as_tibble = function() {
      tibble::tibble(
        guid = private$.guid,
        namespace = private$.namespace,
        mnemonic = private$.mnemonic,
        fullname = private$.fullname,
        cusip = private$.cusip,
        fraction = private$.fraction,
        quote_flag = private$.quote_flag,
        quote_source = private$.quote_source,
        quote_tz = private$.quote_tz
      )
    },

    #' @description Print method
    print = function() {
      cat("<Commodity>\n")
      cat("  ID:", self$identifier(), "\n")
      if (!is.na(private$.fullname)) {
        cat("  Name:", private$.fullname, "\n")
      }
      cat("  Type:", ifelse(self$is_currency(), "Currency", "Security"), "\n")
      cat("  Fraction:", private$.fraction, "\n")
      if (private$.quote_flag) {
        cat("  Quotes:", private$.quote_source, "\n")
      }
      invisible(self)
    }
  ),

  active = list(
    #' @field guid Commodity GUID (read-only)
    guid = function() private$.guid,

    #' @field namespace Commodity namespace
    namespace = function(value) {
      if (missing(value)) return(private$.namespace)
      private$.namespace <- value
    },

    #' @field mnemonic Commodity mnemonic
    mnemonic = function(value) {
      if (missing(value)) return(private$.mnemonic)
      private$.mnemonic <- value
    },

    #' @field fullname Full descriptive name
    fullname = function(value) {
      if (missing(value)) return(private$.fullname)
      private$.fullname <- value
    },

    #' @field cusip CUSIP/ISIN identifier
    cusip = function(value) {
      if (missing(value)) return(private$.cusip)
      private$.cusip <- value
    },

    #' @field fraction Smallest unit (100 = cents)
    fraction = function(value) {
      if (missing(value)) return(private$.fraction)
      private$.fraction <- as.integer(value)
    },

    #' @field quote_flag Whether to fetch online quotes
    quote_flag = function(value) {
      if (missing(value)) return(private$.quote_flag)
      private$.quote_flag <- value
    },

    #' @field quote_source Source for online quotes
    quote_source = function(value) {
      if (missing(value)) return(private$.quote_source)
      private$.quote_source <- value
    },

    #' @field quote_tz Timezone for quotes
    quote_tz = function(value) {
      if (missing(value)) return(private$.quote_tz)
      private$.quote_tz <- value
    }
  ),

  private = list(
    .guid = NULL,
    .namespace = NULL,
    .mnemonic = NULL,
    .fullname = NULL,
    .cusip = NULL,
    .fraction = NULL,
    .quote_flag = NULL,
    .quote_source = NULL,
    .quote_tz = NULL
  )
)


#' Create a Currency Commodity
#'
#' Helper function to create a currency commodity with standard settings.
#'
#' @param code ISO 4217 currency code (e.g., "USD", "EUR")
#' @param fullname Optional full name
#' @return A Commodity object
#' @export
#' @examples
#' \dontrun{
#' usd <- currency("USD")
#' eur <- currency("EUR", "Euro")
#' }
currency <- function(code, fullname = NA_character_) {
  # Standard currency fractions
  fractions <- c(
    USD = 100L, EUR = 100L, GBP = 100L, CAD = 100L, AUD = 100L,
    CHF = 100L, CNY = 100L, INR = 100L, MXN = 100L, BRL = 100L,
    JPY = 1L, KRW = 1L  # No decimal places
  )

  fraction <- if (code %in% names(fractions)) fractions[code] else 100L

  Commodity$new(
    namespace = "CURRENCY",
    mnemonic = code,
    fullname = fullname,
    fraction = fraction,
    quote_flag = TRUE,
    quote_source = "currency"
  )
}


#' Create a Stock/Security Commodity
#'
#' Helper function to create a stock or security commodity.
#'
#' @param symbol Ticker symbol (e.g., "AAPL", "GOOGL")
#' @param exchange Exchange namespace (NASDAQ, NYSE, etc.)
#' @param fullname Company/fund name
#' @param cusip CUSIP/ISIN identifier
#' @param fraction Price precision (default 10000 for 4 decimal places)
#' @return A Commodity object
#' @export
#' @examples
#' \dontrun{
#' aapl <- security("AAPL", "NASDAQ", "Apple Inc.")
#' }
security <- function(symbol, exchange = "NYSE", fullname = NA_character_,
                     cusip = NA_character_, fraction = 10000L) {
  Commodity$new(
    namespace = exchange,
    mnemonic = symbol,
    fullname = fullname,
    cusip = cusip,
    fraction = fraction,
    quote_flag = TRUE,
    quote_source = "yahoo"
  )
}


#' Convert Data Frame to Commodity Objects
#'
#' Convert a tibble of commodity data to a list of Commodity objects.
#'
#' @param df Data frame with commodity columns
#' @return Named list of Commodity objects (keyed by identifier)
#' @export
commodities_from_df <- function(df) {
  if (nrow(df) == 0) return(list())

  commodities <- purrr::map(seq_len(nrow(df)), function(i) {
    row <- df[i, ]
    Commodity$new(
      guid = row$guid,
      namespace = row$namespace %||% "CURRENCY",
      mnemonic = row$mnemonic,
      fullname = row$fullname %||% NA_character_,
      cusip = row$cusip %||% NA_character_,
      fraction = row$fraction %||% 100L,
      quote_flag = isTRUE(row$quote_flag),
      quote_source = row$quote_source %||% NA_character_,
      quote_tz = row$quote_tz %||% NA_character_
    )
  })

  # Name by identifier
  names(commodities) <- purrr::map_chr(commodities, ~ .x$identifier())
  commodities
}
