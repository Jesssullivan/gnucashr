#' Price R6 Class
#'
#' R6 class representing a price/exchange rate between two commodities.
#' Used for converting between currencies and tracking security prices.
#'
#' @export
Price <- R6::R6Class("Price",
  public = list(
    #' @description Create a new Price object
    #' @param guid Unique identifier (32-character hex)
    #' @param commodity_guid GUID of the commodity being priced
    #' @param currency_guid GUID of the currency used for pricing
    #' @param date Date/time of the price
    #' @param source Source of the price (user, yahoo, etc.)
    #' @param type Price type (last, bid, ask, nav, etc.)
    #' @param value_num Numerator of price fraction
    #' @param value_denom Denominator of price fraction
    initialize = function(guid = NULL,
                         commodity_guid,
                         currency_guid,
                         date = Sys.time(),
                         source = "user:price",
                         type = "last",
                         value_num,
                         value_denom = 100L) {
      if (is.null(guid)) {
        guid <- generate_guid()
      }

      if (!validate_guid(guid)) {
        rlang::abort("Invalid GUID format")
      }

      private$.guid <- guid
      private$.commodity_guid <- commodity_guid
      private$.currency_guid <- currency_guid
      private$.date <- as.POSIXct(date)
      private$.source <- source
      private$.type <- type
      private$.value_num <- as.integer(value_num)
      private$.value_denom <- as.integer(value_denom)

      invisible(self)
    },

    #' @description Get the price as a decimal value
    value = function() {
      fraction_to_double(private$.value_num, private$.value_denom)
    },

    #' @description Set the price from a decimal value
    #' @param new_value New price value
    #' @param denom Denominator for storage precision
    set_value = function(new_value, denom = 100L) {
      result <- double_to_fraction(new_value, denom)
      private$.value_num <- result[1, 1]
      private$.value_denom <- result[1, 2]
      invisible(self)
    },

    #' @description Convert an amount from commodity to currency
    #' @param commodity_amount Amount in commodity units
    convert_to_currency = function(commodity_amount) {
      commodity_amount * self$value()
    },

    #' @description Convert an amount from currency to commodity
    #' @param currency_amount Amount in currency units
    convert_to_commodity = function(currency_amount) {
      currency_amount / self$value()
    },

    #' @description Convert to data frame row
    as_tibble = function() {
      tibble::tibble(
        guid = private$.guid,
        commodity_guid = private$.commodity_guid,
        currency_guid = private$.currency_guid,
        date = private$.date,
        source = private$.source,
        type = private$.type,
        value_num = private$.value_num,
        value_denom = private$.value_denom,
        value = self$value()
      )
    },

    #' @description Print method
    print = function() {
      cat("<Price>\n")
      cat("  Date:", format(private$.date, "%Y-%m-%d %H:%M"), "\n")
      cat("  Value:", self$value(), "\n")
      cat("  Source:", private$.source, "\n")
      cat("  Type:", private$.type, "\n")
      invisible(self)
    }
  ),

  active = list(
    #' @field guid Price GUID (read-only)
    guid = function() private$.guid,

    #' @field commodity_guid Commodity being priced
    commodity_guid = function(value) {
      if (missing(value)) return(private$.commodity_guid)
      private$.commodity_guid <- value
    },

    #' @field currency_guid Currency for pricing
    currency_guid = function(value) {
      if (missing(value)) return(private$.currency_guid)
      private$.currency_guid <- value
    },

    #' @field date Price date/time
    date = function(value) {
      if (missing(value)) return(private$.date)
      private$.date <- as.POSIXct(value)
    },

    #' @field source Price source
    source = function(value) {
      if (missing(value)) return(private$.source)
      private$.source <- value
    },

    #' @field type Price type (last, bid, ask, nav)
    type = function(value) {
      if (missing(value)) return(private$.type)
      private$.type <- value
    },

    #' @field value_num Price numerator
    value_num = function() private$.value_num,

    #' @field value_denom Price denominator
    value_denom = function() private$.value_denom
  ),

  private = list(
    .guid = NULL,
    .commodity_guid = NULL,
    .currency_guid = NULL,
    .date = NULL,
    .source = NULL,
    .type = NULL,
    .value_num = NULL,
    .value_denom = NULL
  )
)


#' Create a Price from Decimal Value
#'
#' Helper function to create a price entry from a decimal value.
#'
#' @param commodity_guid GUID of the commodity
#' @param currency_guid GUID of the currency
#' @param value Price as decimal value
#' @param date Price date (default: now)
#' @param source Price source (default: "user:price")
#' @param type Price type (default: "last")
#' @param precision Decimal places for storage (default: 4)
#' @return A Price object
#' @export
#' @examples
#' \dontrun{
#' price <- new_price("abc123...", "def456...", 150.25)
#' }
new_price <- function(commodity_guid, currency_guid, value,
                      date = Sys.time(), source = "user:price",
                      type = "last", precision = 4L) {
  denom <- 10L^precision
  num <- as.integer(round(value * denom))

  Price$new(
    commodity_guid = commodity_guid,
    currency_guid = currency_guid,
    date = date,
    source = source,
    type = type,
    value_num = num,
    value_denom = denom
  )
}


#' PriceDB R6 Class
#'
#' R6 class for managing a collection of prices.
#' Provides efficient lookup of prices by commodity and date.
#'
#' @export
PriceDB <- R6::R6Class("PriceDB",
  public = list(
    #' @description Create a new PriceDB
    #' @param prices Optional initial list of Price objects
    initialize = function(prices = list()) {
      private$.prices <- prices
      private$.index <- list()
      if (length(prices) > 0) {
        private$rebuild_index()
      }
      invisible(self)
    },

    #' @description Add a price to the database
    #' @param price A Price object
    add = function(price) {
      if (!inherits(price, "Price")) {
        rlang::abort("Expected a Price object")
      }
      private$.prices[[price$guid]] <- price
      private$add_to_index(price)
      invisible(self)
    },

    #' @description Get all prices for a commodity
    #' @param commodity_guid GUID of the commodity
    #' @param currency_guid Optional: filter by currency
    #' @param start_date Optional: start of date range
    #' @param end_date Optional: end of date range
    get_prices = function(commodity_guid, currency_guid = NULL,
                          start_date = NULL, end_date = NULL) {
      key <- commodity_guid
      if (!is.null(currency_guid)) {
        key <- paste(key, currency_guid, sep = ":")
      }

      guids <- private$.index[[key]]
      if (is.null(guids)) return(list())

      prices <- private$.prices[guids]

      # Filter by date if specified
      if (!is.null(start_date)) {
        prices <- purrr::keep(prices, ~ .x$date >= as.POSIXct(start_date))
      }
      if (!is.null(end_date)) {
        prices <- purrr::keep(prices, ~ .x$date <= as.POSIXct(end_date))
      }

      # Sort by date descending (most recent first)
      dates <- purrr::map_dbl(prices, ~ as.numeric(.x$date))
      prices[order(dates, decreasing = TRUE)]
    },

    #' @description Get the most recent price for a commodity
    #' @param commodity_guid GUID of the commodity
    #' @param currency_guid GUID of the currency
    #' @param as_of Optional: get price as of specific date
    get_price = function(commodity_guid, currency_guid, as_of = NULL) {
      prices <- self$get_prices(commodity_guid, currency_guid,
                                end_date = as_of)
      if (length(prices) == 0) return(NULL)
      prices[[1]]  # Most recent
    },

    #' @description Get exchange rate between two commodities
    #' @param from_guid Source commodity GUID
    #' @param to_guid Target commodity GUID
    #' @param as_of Optional: date for rate
    get_rate = function(from_guid, to_guid, as_of = NULL) {
      # Direct price
      price <- self$get_price(from_guid, to_guid, as_of)
      if (!is.null(price)) return(price$value())

      # Inverse price
      price <- self$get_price(to_guid, from_guid, as_of)
      if (!is.null(price)) return(1 / price$value())

      NA_real_
    },

    #' @description Convert amount between commodities
    #' @param amount Amount to convert
    #' @param from_guid Source commodity GUID
    #' @param to_guid Target commodity GUID
    #' @param as_of Optional: date for rate
    convert = function(amount, from_guid, to_guid, as_of = NULL) {
      if (from_guid == to_guid) return(amount)
      rate <- self$get_rate(from_guid, to_guid, as_of)
      if (is.na(rate)) {
        rlang::warn(paste("No exchange rate found for",
                          from_guid, "->", to_guid))
        return(NA_real_)
      }
      amount * rate
    },

    #' @description Get all prices as a tibble
    as_tibble = function() {
      if (length(private$.prices) == 0) {
        return(tibble::tibble(
          guid = character(), commodity_guid = character(),
          currency_guid = character(), date = as.POSIXct(character()),
          source = character(), type = character(),
          value_num = integer(), value_denom = integer(),
          value = numeric()
        ))
      }
      purrr::map_dfr(private$.prices, ~ .x$as_tibble())
    },

    #' @description Get number of prices
    count = function() {
      length(private$.prices)
    },

    #' @description Print method
    print = function() {
      cat("<PriceDB>\n")
      cat("  Prices:", self$count(), "\n")

      # Show commodities with prices
      commodities <- unique(purrr::map_chr(private$.prices, ~ .x$commodity_guid))
      cat("  Commodities:", length(commodities), "\n")

      invisible(self)
    }
  ),

  private = list(
    .prices = list(),
    .index = list(),  # commodity_guid -> list of price guids

    rebuild_index = function() {
      private$.index <- list()
      purrr::walk(private$.prices, private$add_to_index)
    },

    add_to_index = function(price) {
      # Index by commodity
      key1 <- price$commodity_guid
      if (is.null(private$.index[[key1]])) {
        private$.index[[key1]] <- character()
      }
      private$.index[[key1]] <- unique(c(private$.index[[key1]], price$guid))

      # Index by commodity:currency pair
      key2 <- paste(price$commodity_guid, price$currency_guid, sep = ":")
      if (is.null(private$.index[[key2]])) {
        private$.index[[key2]] <- character()
      }
      private$.index[[key2]] <- unique(c(private$.index[[key2]], price$guid))
    }
  )
)


#' Convert Data Frame to Price Objects
#'
#' Convert a tibble of price data to a list of Price objects.
#'
#' @param df Data frame with price columns
#' @return Named list of Price objects (keyed by guid)
#' @export
prices_from_df <- function(df) {
  if (nrow(df) == 0) return(list())

  prices <- purrr::map(seq_len(nrow(df)), function(i) {
    row <- df[i, ]
    Price$new(
      guid = row$guid,
      commodity_guid = row$commodity_guid,
      currency_guid = row$currency_guid,
      date = row$date,
      source = row$source %||% "user:price",
      type = row$type %||% "last",
      value_num = row$value_num,
      value_denom = row$value_denom
    )
  })

  names(prices) <- purrr::map_chr(prices, ~ .x$guid)
  prices
}


#' Create PriceDB from Data Frame
#'
#' Build a PriceDB from a tibble of price data.
#'
#' @param df Data frame with price columns
#' @return A PriceDB object
#' @export
pricedb_from_df <- function(df) {
  prices <- prices_from_df(df)
  PriceDB$new(prices)
}
