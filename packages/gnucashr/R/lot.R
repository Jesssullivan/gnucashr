#' Lot R6 Class
#'
#' R6 class representing a GnuCash lot for tracking cost basis
#' and capital gains/losses. Lots tie buy and sell transactions together
#' for securities tracking.
#'
#' @export
Lot <- R6::R6Class("Lot",
  public = list(
    #' @description Create a new Lot object
    #' @param guid Unique identifier (32-character hex)
    #' @param account_guid GUID of the account this lot belongs to
    #' @param is_closed Whether the lot is closed (fully sold)
    #' @param title Optional title/name for the lot
    #' @param notes Optional notes
    initialize = function(guid = NULL,
                         account_guid,
                         is_closed = FALSE,
                         title = NA_character_,
                         notes = NA_character_) {
      if (is.null(guid)) {
        guid <- generate_guid()
      }

      if (!validate_guid(guid)) {
        rlang::abort("Invalid GUID format")
      }

      private$.guid <- guid
      private$.account_guid <- account_guid
      private$.is_closed <- is_closed
      private$.title <- title
      private$.notes <- notes
      private$.splits <- list()

      invisible(self)
    },

    #' @description Add a split to this lot
    #' @param split_guid GUID of the split
    #' @param quantity Amount (positive for buy, negative for sell)
    #' @param value Cost/proceeds in account currency
    #' @param date Transaction date
    add_split = function(split_guid, quantity, value, date) {
      private$.splits[[split_guid]] <- list(
        guid = split_guid,
        quantity = quantity,
        value = value,
        date = as.POSIXct(date)
      )
      invisible(self)
    },

    #' @description Get all splits in this lot
    splits = function() {
      private$.splits
    },

    #' @description Calculate total quantity in lot
    quantity = function() {
      if (length(private$.splits) == 0) return(0)
      sum(purrr::map_dbl(private$.splits, ~ .x$quantity))
    },

    #' @description Calculate total cost basis
    cost_basis = function() {
      if (length(private$.splits) == 0) return(0)
      # Sum of positive values (purchases)
      values <- purrr::map_dbl(private$.splits, ~ .x$value)
      quantities <- purrr::map_dbl(private$.splits, ~ .x$quantity)

      # Cost basis = sum of values for buy transactions
      sum(values[quantities > 0])
    },

    #' @description Calculate proceeds from sales
    proceeds = function() {
      if (length(private$.splits) == 0) return(0)
      # Sum of absolute values for sell transactions
      values <- purrr::map_dbl(private$.splits, ~ .x$value)
      quantities <- purrr::map_dbl(private$.splits, ~ .x$quantity)

      abs(sum(values[quantities < 0]))
    },

    #' @description Calculate realized gain/loss
    realized_gain = function() {
      self$proceeds() - self$cost_basis()
    },

    #' @description Calculate average cost per unit
    avg_cost = function() {
      qty <- self$quantity()
      if (qty <= 0) return(NA_real_)
      self$cost_basis() / qty
    },

    #' @description Calculate unrealized gain/loss at a given price
    #' @param current_price Current market price per unit
    unrealized_gain = function(current_price) {
      qty <- self$quantity()
      if (qty <= 0) return(0)
      (current_price * qty) - self$cost_basis()
    },

    #' @description Get opening date (first buy)
    open_date = function() {
      if (length(private$.splits) == 0) return(NA)
      dates <- purrr::map(private$.splits, ~ .x$date)
      quantities <- purrr::map_dbl(private$.splits, ~ .x$quantity)

      buy_dates <- dates[quantities > 0]
      if (length(buy_dates) == 0) return(NA)
      min(do.call(c, buy_dates))
    },

    #' @description Get closing date (last sell that closed the lot)
    close_date = function() {
      if (!private$.is_closed) return(NA)
      if (length(private$.splits) == 0) return(NA)

      dates <- purrr::map(private$.splits, ~ .x$date)
      quantities <- purrr::map_dbl(private$.splits, ~ .x$quantity)

      sell_dates <- dates[quantities < 0]
      if (length(sell_dates) == 0) return(NA)
      max(do.call(c, sell_dates))
    },

    #' @description Calculate holding period in days
    holding_period = function() {
      open <- self$open_date()
      if (is.na(open)) return(NA_integer_)

      close <- if (private$.is_closed) self$close_date() else Sys.time()
      as.integer(difftime(close, open, units = "days"))
    },

    #' @description Check if long-term (held > 1 year)
    is_long_term = function() {
      hp <- self$holding_period()
      if (is.na(hp)) return(NA)
      hp > 365
    },

    #' @description Convert to data frame row
    as_tibble = function() {
      tibble::tibble(
        guid = private$.guid,
        account_guid = private$.account_guid,
        is_closed = private$.is_closed,
        title = private$.title,
        notes = private$.notes,
        quantity = self$quantity(),
        cost_basis = self$cost_basis(),
        avg_cost = self$avg_cost(),
        open_date = self$open_date(),
        holding_days = self$holding_period()
      )
    },

    #' @description Print method
    print = function() {
      cat("<Lot>\n")
      cat("  GUID:", private$.guid, "\n")
      if (!is.na(private$.title)) {
        cat("  Title:", private$.title, "\n")
      }
      cat("  Status:", ifelse(private$.is_closed, "Closed", "Open"), "\n")
      cat("  Quantity:", self$quantity(), "\n")
      cat("  Cost Basis:", self$cost_basis(), "\n")
      cat("  Avg Cost:", round(self$avg_cost(), 4), "\n")
      cat("  Holding Period:", self$holding_period(), "days\n")
      if (private$.is_closed) {
        cat("  Realized Gain:", self$realized_gain(), "\n")
      }
      invisible(self)
    }
  ),

  active = list(
    #' @field guid Lot GUID (read-only)
    guid = function() private$.guid,

    #' @field account_guid Account GUID
    account_guid = function(value) {
      if (missing(value)) return(private$.account_guid)
      private$.account_guid <- value
    },

    #' @field is_closed Whether lot is closed
    is_closed = function(value) {
      if (missing(value)) return(private$.is_closed)
      private$.is_closed <- value
    },

    #' @field title Lot title
    title = function(value) {
      if (missing(value)) return(private$.title)
      private$.title <- value
    },

    #' @field notes Lot notes
    notes = function(value) {
      if (missing(value)) return(private$.notes)
      private$.notes <- value
    }
  ),

  private = list(
    .guid = NULL,
    .account_guid = NULL,
    .is_closed = NULL,
    .title = NULL,
    .notes = NULL,
    .splits = list()
  )
)


#' LotManager R6 Class
#'
#' R6 class for managing lots for an account.
#' Provides FIFO, LIFO, and specific identification methods.
#'
#' @export
LotManager <- R6::R6Class("LotManager",
  public = list(
    #' @description Create a new LotManager
    #' @param account_guid Account GUID
    #' @param method Cost basis method (FIFO, LIFO, SPECIFIC)
    initialize = function(account_guid, method = "FIFO") {
      private$.account_guid <- account_guid
      private$.method <- match.arg(method, c("FIFO", "LIFO", "SPECIFIC", "AVG"))
      private$.lots <- list()

      invisible(self)
    },

    #' @description Add a lot
    #' @param lot A Lot object
    add_lot = function(lot) {
      if (!inherits(lot, "Lot")) {
        rlang::abort("Expected a Lot object")
      }
      private$.lots[[lot$guid]] <- lot
      invisible(self)
    },

    #' @description Get all lots
    #' @param include_closed Include closed lots
    lots = function(include_closed = TRUE) {
      if (include_closed) {
        return(private$.lots)
      }
      purrr::keep(private$.lots, ~ !.x$is_closed)
    },

    #' @description Get open lots sorted by method order
    open_lots_ordered = function() {
      open <- self$lots(include_closed = FALSE)
      if (length(open) == 0) return(list())

      dates <- purrr::map_dbl(open, ~ as.numeric(.x$open_date()))

      if (private$.method == "LIFO") {
        open[order(dates, decreasing = TRUE)]
      } else {
        # FIFO or default
        open[order(dates)]
      }
    },

    #' @description Calculate total quantity across all open lots
    total_quantity = function() {
      lots <- self$lots(include_closed = FALSE)
      sum(purrr::map_dbl(lots, ~ .x$quantity()))
    },

    #' @description Calculate total cost basis across all open lots
    total_cost_basis = function() {
      lots <- self$lots(include_closed = FALSE)
      sum(purrr::map_dbl(lots, ~ .x$cost_basis()))
    },

    #' @description Calculate weighted average cost
    avg_cost = function() {
      qty <- self$total_quantity()
      if (qty <= 0) return(NA_real_)
      self$total_cost_basis() / qty
    },

    #' @description Calculate total unrealized gain
    #' @param current_price Current market price
    unrealized_gain = function(current_price) {
      lots <- self$lots(include_closed = FALSE)
      sum(purrr::map_dbl(lots, ~ .x$unrealized_gain(current_price)))
    },

    #' @description Get lots to use for selling (based on method)
    #' @param quantity Quantity to sell
    select_lots_for_sale = function(quantity) {
      if (quantity <= 0) return(list())

      ordered <- self$open_lots_ordered()
      if (length(ordered) == 0) return(list())

      selected <- list()
      remaining <- quantity

      for (lot in ordered) {
        if (remaining <= 0) break

        lot_qty <- lot$quantity()
        if (lot_qty > 0) {
          use_qty <- min(lot_qty, remaining)
          selected[[lot$guid]] <- list(lot = lot, quantity = use_qty)
          remaining <- remaining - use_qty
        }
      }

      if (remaining > 0) {
        rlang::warn(paste("Insufficient lots. Remaining:", remaining))
      }

      selected
    },

    #' @description Get all lots as a tibble
    as_tibble = function() {
      if (length(private$.lots) == 0) {
        return(tibble::tibble(
          guid = character(), account_guid = character(),
          is_closed = logical(), title = character(),
          notes = character(), quantity = numeric(),
          cost_basis = numeric(), avg_cost = numeric(),
          open_date = as.POSIXct(character()),
          holding_days = integer()
        ))
      }
      purrr::map_dfr(private$.lots, ~ .x$as_tibble())
    },

    #' @description Print method
    print = function() {
      cat("<LotManager>\n")
      cat("  Account:", private$.account_guid, "\n")
      cat("  Method:", private$.method, "\n")
      cat("  Total Lots:", length(private$.lots), "\n")
      cat("  Open Lots:", length(self$lots(include_closed = FALSE)), "\n")
      cat("  Total Quantity:", self$total_quantity(), "\n")
      cat("  Total Cost Basis:", self$total_cost_basis(), "\n")
      invisible(self)
    }
  ),

  active = list(
    #' @field account_guid Account GUID
    account_guid = function() private$.account_guid,

    #' @field method Cost basis method
    method = function(value) {
      if (missing(value)) return(private$.method)
      private$.method <- match.arg(value, c("FIFO", "LIFO", "SPECIFIC", "AVG"))
    }
  ),

  private = list(
    .account_guid = NULL,
    .method = NULL,
    .lots = list()
  )
)


#' Convert Data Frame to Lot Objects
#'
#' Convert a tibble of lot data to a list of Lot objects.
#'
#' @param df Data frame with lot columns
#' @return Named list of Lot objects (keyed by guid)
#' @export
lots_from_df <- function(df) {
  if (nrow(df) == 0) return(list())

  lots <- purrr::map(seq_len(nrow(df)), function(i) {
    row <- df[i, ]
    Lot$new(
      guid = row$guid,
      account_guid = row$account_guid,
      is_closed = isTRUE(row$is_closed) || row$is_closed == 1,
      title = row$title %||% NA_character_,
      notes = row$notes %||% NA_character_
    )
  })

  names(lots) <- purrr::map_chr(lots, ~ .x$guid)
  lots
}
