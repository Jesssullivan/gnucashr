#' ScheduledTransaction R6 Class
#'
#' R6 class representing a GnuCash scheduled transaction.
#' Scheduled transactions define recurring transactions with a template.
#'
#' @export
ScheduledTransaction <- R6::R6Class("ScheduledTransaction",
  public = list(
    #' @description Create a new ScheduledTransaction object
    #' @param guid Unique identifier (32-character hex)
    #' @param name Transaction name/description
    #' @param enabled Whether the scheduled transaction is active
    #' @param auto_create Automatically create transactions
    #' @param auto_notify Notify when transaction created
    #' @param advance_creation_days Days in advance to create
    #' @param advance_notify_days Days in advance to notify
    #' @param start_date First occurrence date
    #' @param end_date Optional end date
    #' @param last_occur Last occurrence date
    #' @param num_occur Total occurrences (0 = unlimited)
    #' @param rem_occur Remaining occurrences
    #' @param instance_count Number of instances created
    #' @param template_account_guid GUID of template account holding splits
    initialize = function(guid = NULL,
                         name,
                         enabled = TRUE,
                         auto_create = FALSE,
                         auto_notify = FALSE,
                         advance_creation_days = 0L,
                         advance_notify_days = 0L,
                         start_date,
                         end_date = NULL,
                         last_occur = NULL,
                         num_occur = 0L,
                         rem_occur = 0L,
                         instance_count = 0L,
                         template_account_guid = NULL) {
      if (is.null(guid)) {
        guid <- generate_guid()
      }

      if (!validate_guid(guid)) {
        rlang::abort("Invalid GUID format")
      }

      private$.guid <- guid
      private$.name <- name
      private$.enabled <- enabled
      private$.auto_create <- auto_create
      private$.auto_notify <- auto_notify
      private$.advance_creation_days <- as.integer(advance_creation_days)
      private$.advance_notify_days <- as.integer(advance_notify_days)
      private$.start_date <- as.Date(start_date)
      private$.end_date <- if (!is.null(end_date)) as.Date(end_date) else NULL
      private$.last_occur <- if (!is.null(last_occur)) as.Date(last_occur) else NULL
      private$.num_occur <- as.integer(num_occur)
      private$.rem_occur <- as.integer(rem_occur)
      private$.instance_count <- as.integer(instance_count)
      private$.template_account_guid <- template_account_guid

      # Recurrence defaults
      private$.recurrence_mult <- 1L
      private$.recurrence_period_type <- "month"
      private$.recurrence_weekend_adj <- "none"

      # Template splits
      private$.template_splits <- list()

      invisible(self)
    },

    #' @description Set recurrence pattern
    #' @param period_type Type: day, week, month, month_relative, year
    #' @param mult Multiplier (e.g., 2 for every 2 weeks)
    #' @param weekend_adj Weekend adjustment (none, forward, back)
    set_recurrence = function(period_type, mult = 1L,
                              weekend_adj = "none") {
      private$.recurrence_mult <- as.integer(mult)
      private$.recurrence_period_type <- period_type
      private$.recurrence_weekend_adj <- weekend_adj
      invisible(self)
    },

    #' @description Add a template split
    #' @param account_guid Target account GUID
    #' @param value_num Value numerator
    #' @param value_denom Value denominator
    #' @param memo Optional memo
    add_template_split = function(account_guid, value_num, value_denom,
                                  memo = NA_character_) {
      split_guid <- generate_guid()
      private$.template_splits[[split_guid]] <- list(
        guid = split_guid,
        account_guid = account_guid,
        value_num = as.integer(value_num),
        value_denom = as.integer(value_denom),
        memo = memo
      )
      invisible(self)
    },

    #' @description Get template splits
    template_splits = function() {
      private$.template_splits
    },

    #' @description Calculate next occurrence date
    next_occurrence = function() {
      if (!private$.enabled) return(NA)
      if (!is.null(private$.end_date) &&
          Sys.Date() > private$.end_date) return(NA)
      if (private$.num_occur > 0 && private$.rem_occur <= 0) return(NA)

      # Start from last occurrence or start date
      base_date <- private$.last_occur %||% private$.start_date

      # Calculate next date based on recurrence
      next_date <- switch(private$.recurrence_period_type,
        "day" = base_date + private$.recurrence_mult,
        "week" = base_date + (7L * private$.recurrence_mult),
        "month" = lubridate::add_with_rollback(
          base_date, months(private$.recurrence_mult)
        ),
        "month_relative" = lubridate::add_with_rollback(
          base_date, months(private$.recurrence_mult)
        ),
        "year" = lubridate::add_with_rollback(
          base_date, lubridate::years(private$.recurrence_mult)
        ),
        base_date + 30  # default to monthly
      )

      # Apply weekend adjustment
      dow <- lubridate::wday(next_date)
      if (dow == 1 && private$.recurrence_weekend_adj == "forward") {
        next_date <- next_date + 1
      } else if (dow == 1 && private$.recurrence_weekend_adj == "back") {
        next_date <- next_date - 2
      } else if (dow == 7 && private$.recurrence_weekend_adj == "forward") {
        next_date <- next_date + 2
      } else if (dow == 7 && private$.recurrence_weekend_adj == "back") {
        next_date <- next_date - 1
      }

      # Check against end date
      if (!is.null(private$.end_date) && next_date > private$.end_date) {
        return(NA)
      }

      next_date
    },

    #' @description Get human-readable recurrence description
    recurrence_description = function() {
      period_names <- c(
        "day" = "day",
        "week" = "week",
        "month" = "month",
        "month_relative" = "month",
        "year" = "year"
      )

      period <- period_names[private$.recurrence_period_type] %||% "period"

      if (private$.recurrence_mult == 1) {
        paste("Every", period)
      } else {
        paste("Every", private$.recurrence_mult, paste0(period, "s"))
      }
    },

    #' @description Check if transaction is due
    #' @param as_of Optional date to check against (default: today)
    is_due = function(as_of = Sys.Date()) {
      next_date <- self$next_occurrence()
      if (is.na(next_date)) return(FALSE)
      next_date <= as_of
    },

    #' @description Get total amount from template splits
    total_amount = function() {
      if (length(private$.template_splits) == 0) return(0)

      values <- purrr::map_dbl(private$.template_splits, function(split) {
        fraction_to_double(split$value_num, split$value_denom)
      })

      # Return absolute value of positive splits (debits)
      sum(values[values > 0])
    },

    #' @description Convert to data frame row
    as_tibble = function() {
      tibble::tibble(
        guid = private$.guid,
        name = private$.name,
        enabled = private$.enabled,
        auto_create = private$.auto_create,
        auto_notify = private$.auto_notify,
        advance_creation_days = private$.advance_creation_days,
        advance_notify_days = private$.advance_notify_days,
        start_date = private$.start_date,
        end_date = private$.end_date,
        last_occur = private$.last_occur,
        num_occur = private$.num_occur,
        rem_occur = private$.rem_occur,
        instance_count = private$.instance_count,
        template_account_guid = private$.template_account_guid,
        recurrence_mult = private$.recurrence_mult,
        recurrence_period_type = private$.recurrence_period_type,
        next_occurrence = self$next_occurrence(),
        amount = self$total_amount()
      )
    },

    #' @description Print method
    print = function() {
      cat("<ScheduledTransaction>\n")
      cat("  Name:", private$.name, "\n")
      cat("  Status:", ifelse(private$.enabled, "Enabled", "Disabled"), "\n")
      cat("  Recurrence:", self$recurrence_description(), "\n")
      cat("  Start:", format(private$.start_date, "%Y-%m-%d"), "\n")
      if (!is.null(private$.end_date)) {
        cat("  End:", format(private$.end_date, "%Y-%m-%d"), "\n")
      }
      if (!is.null(private$.last_occur)) {
        cat("  Last:", format(private$.last_occur, "%Y-%m-%d"), "\n")
      }
      next_occ <- self$next_occurrence()
      if (!is.na(next_occ)) {
        cat("  Next:", format(next_occ, "%Y-%m-%d"), "\n")
      }
      cat("  Amount:", self$total_amount(), "\n")
      cat("  Splits:", length(private$.template_splits), "\n")
      invisible(self)
    }
  ),

  active = list(
    #' @field guid Transaction GUID (read-only)
    guid = function() private$.guid,

    #' @field name Transaction name
    name = function(value) {
      if (missing(value)) return(private$.name)
      private$.name <- value
    },

    #' @field enabled Whether enabled
    enabled = function(value) {
      if (missing(value)) return(private$.enabled)
      private$.enabled <- value
    },

    #' @field auto_create Auto-create flag
    auto_create = function(value) {
      if (missing(value)) return(private$.auto_create)
      private$.auto_create <- value
    },

    #' @field auto_notify Auto-notify flag
    auto_notify = function(value) {
      if (missing(value)) return(private$.auto_notify)
      private$.auto_notify <- value
    },

    #' @field start_date Start date
    start_date = function() private$.start_date,

    #' @field end_date End date
    end_date = function() private$.end_date,

    #' @field last_occur Last occurrence date
    last_occur = function() private$.last_occur,

    #' @field num_occur Total occurrences
    num_occur = function() private$.num_occur,

    #' @field rem_occur Remaining occurrences
    rem_occur = function() private$.rem_occur,

    #' @field instance_count Instance count
    instance_count = function() private$.instance_count,

    #' @field recurrence_mult Recurrence multiplier
    recurrence_mult = function() private$.recurrence_mult,

    #' @field recurrence_period_type Recurrence period type
    recurrence_period_type = function() private$.recurrence_period_type
  ),

  private = list(
    .guid = NULL,
    .name = NULL,
    .enabled = NULL,
    .auto_create = NULL,
    .auto_notify = NULL,
    .advance_creation_days = NULL,
    .advance_notify_days = NULL,
    .start_date = NULL,
    .end_date = NULL,
    .last_occur = NULL,
    .num_occur = NULL,
    .rem_occur = NULL,
    .instance_count = NULL,
    .template_account_guid = NULL,
    .recurrence_mult = NULL,
    .recurrence_period_type = NULL,
    .recurrence_weekend_adj = NULL,
    .template_splits = list()
  )
)


#' Create a Monthly Scheduled Transaction
#'
#' Helper function to create a monthly recurring transaction.
#'
#' @param name Transaction description
#' @param amount Transaction amount
#' @param debit_account_guid Account to debit

#' @param credit_account_guid Account to credit
#' @param start_date First occurrence
#' @param day_of_month Day of month (1-31, or "last")
#' @param auto_create Auto-create transactions
#' @return A ScheduledTransaction object
#' @export
monthly_scheduled <- function(name, amount, debit_account_guid,
                              credit_account_guid, start_date,
                              day_of_month = NULL, auto_create = FALSE) {
  sx <- ScheduledTransaction$new(
    name = name,
    start_date = start_date,
    auto_create = auto_create
  )

  sx$set_recurrence("month", mult = 1L)

  # Add template splits
  result <- double_to_fraction(abs(amount), 100L)
  sx$add_template_split(debit_account_guid, result[1, 1], result[1, 2])
  sx$add_template_split(credit_account_guid, -result[1, 1], result[1, 2])

  sx
}


#' Create a Weekly Scheduled Transaction
#'
#' Helper function to create a weekly recurring transaction.
#'
#' @param name Transaction description
#' @param amount Transaction amount
#' @param debit_account_guid Account to debit
#' @param credit_account_guid Account to credit
#' @param start_date First occurrence
#' @param every_n_weeks Recurrence (1 = every week, 2 = biweekly)
#' @param auto_create Auto-create transactions
#' @return A ScheduledTransaction object
#' @export
weekly_scheduled <- function(name, amount, debit_account_guid,
                             credit_account_guid, start_date,
                             every_n_weeks = 1L, auto_create = FALSE) {
  sx <- ScheduledTransaction$new(
    name = name,
    start_date = start_date,
    auto_create = auto_create
  )

  sx$set_recurrence("week", mult = as.integer(every_n_weeks))

  # Add template splits
  result <- double_to_fraction(abs(amount), 100L)
  sx$add_template_split(debit_account_guid, result[1, 1], result[1, 2])
  sx$add_template_split(credit_account_guid, -result[1, 1], result[1, 2])

  sx
}


#' Create an Annual Scheduled Transaction
#'
#' Helper function to create an annual recurring transaction.
#'
#' @param name Transaction description
#' @param amount Transaction amount
#' @param debit_account_guid Account to debit
#' @param credit_account_guid Account to credit
#' @param start_date First occurrence
#' @param auto_create Auto-create transactions
#' @return A ScheduledTransaction object
#' @export
annual_scheduled <- function(name, amount, debit_account_guid,
                             credit_account_guid, start_date,
                             auto_create = FALSE) {
  sx <- ScheduledTransaction$new(
    name = name,
    start_date = start_date,
    auto_create = auto_create
  )

  sx$set_recurrence("year", mult = 1L)

  # Add template splits
  result <- double_to_fraction(abs(amount), 100L)
  sx$add_template_split(debit_account_guid, result[1, 1], result[1, 2])
  sx$add_template_split(credit_account_guid, -result[1, 1], result[1, 2])

  sx
}


#' ScheduledTransactionManager R6 Class
#'
#' R6 class for managing scheduled transactions.
#'
#' @export
ScheduledTransactionManager <- R6::R6Class("ScheduledTransactionManager",
  public = list(
    #' @description Create a new ScheduledTransactionManager
    initialize = function() {
      private$.scheduled <- list()
      invisible(self)
    },

    #' @description Add a scheduled transaction
    #' @param sx A ScheduledTransaction object
    add = function(sx) {
      if (!inherits(sx, "ScheduledTransaction")) {
        rlang::abort("Expected a ScheduledTransaction object")
      }
      private$.scheduled[[sx$guid]] <- sx
      invisible(self)
    },

    #' @description Get all scheduled transactions
    #' @param enabled_only Only return enabled transactions
    all = function(enabled_only = FALSE) {
      if (!enabled_only) return(private$.scheduled)
      purrr::keep(private$.scheduled, ~ .x$enabled)
    },

    #' @description Get due scheduled transactions
    #' @param as_of Date to check (default: today)
    due = function(as_of = Sys.Date()) {
      purrr::keep(self$all(enabled_only = TRUE), ~ .x$is_due(as_of))
    },

    #' @description Get upcoming scheduled transactions
    #' @param days Number of days to look ahead
    upcoming = function(days = 30L) {
      cutoff <- Sys.Date() + days
      purrr::keep(self$all(enabled_only = TRUE), function(sx) {
        next_occ <- sx$next_occurrence()
        !is.na(next_occ) && next_occ <= cutoff
      })
    },

    #' @description Get all as tibble
    as_tibble = function() {
      if (length(private$.scheduled) == 0) {
        return(tibble::tibble(
          guid = character(), name = character(),
          enabled = logical(), recurrence_period_type = character(),
          next_occurrence = as.Date(character()), amount = numeric()
        ))
      }
      purrr::map_dfr(private$.scheduled, ~ .x$as_tibble())
    },

    #' @description Calculate cash flow forecast from scheduled transactions
    #' @param months Number of months to forecast
    #' @param account_mapping Named list mapping account GUIDs to categories
    forecast_cash_flow = function(months = 12L, account_mapping = list()) {
      end_date <- Sys.Date() + (months * 30)

      # Generate all occurrences
      occurrences <- purrr::map_dfr(self$all(enabled_only = TRUE), function(sx) {
        dates <- list()
        current <- sx$next_occurrence()

        while (!is.na(current) && current <= end_date && length(dates) < 100) {
          dates <- c(dates, list(current))
          # Simulate advancing to next occurrence
          sx_copy <- sx$clone()
          # Note: This is a simplified forecast - real implementation
          # would need to properly advance the recurrence
          current <- current + switch(sx$recurrence_period_type,
            "day" = sx$recurrence_mult,
            "week" = 7L * sx$recurrence_mult,
            "month" = 30L * sx$recurrence_mult,
            "year" = 365L * sx$recurrence_mult,
            30
          )
        }

        if (length(dates) == 0) return(NULL)

        tibble::tibble(
          guid = sx$guid,
          name = sx$name,
          date = do.call(c, dates),
          amount = sx$total_amount()
        )
      })

      occurrences
    },

    #' @description Print method
    print = function() {
      cat("<ScheduledTransactionManager>\n")
      cat("  Total:", length(private$.scheduled), "\n")
      cat("  Enabled:", length(self$all(enabled_only = TRUE)), "\n")
      cat("  Due Today:", length(self$due()), "\n")
      invisible(self)
    }
  ),

  private = list(
    .scheduled = list()
  )
)


#' Convert Data Frame to ScheduledTransaction Objects
#'
#' Convert a tibble of scheduled transaction data to objects.
#'
#' @param df Data frame with schedxactions columns
#' @return Named list of ScheduledTransaction objects
#' @export
scheduled_from_df <- function(df) {
  if (nrow(df) == 0) return(list())

  scheduled <- purrr::map(seq_len(nrow(df)), function(i) {
    row <- df[i, ]

    sx <- ScheduledTransaction$new(
      guid = row$guid,
      name = row$name,
      enabled = isTRUE(row$enabled) || row$enabled == 1,
      auto_create = isTRUE(row$auto_create) || row$auto_create == 1,
      auto_notify = isTRUE(row$auto_notify) || row$auto_notify == 1,
      advance_creation_days = row$adv_creation %||% 0L,
      advance_notify_days = row$adv_notify %||% 0L,
      start_date = row$start_date,
      end_date = row$end_date,
      last_occur = row$last_occur,
      num_occur = row$num_occur %||% 0L,
      rem_occur = row$rem_occur %||% 0L,
      instance_count = row$instance_count %||% 0L,
      template_account_guid = row$template_act_guid
    )

    # Set recurrence if available
    if (!is.null(row$recurrence_period_type)) {
      sx$set_recurrence(
        row$recurrence_period_type,
        row$recurrence_mult %||% 1L
      )
    }

    sx
  })

  names(scheduled) <- purrr::map_chr(scheduled, ~ .x$guid)
  scheduled
}
