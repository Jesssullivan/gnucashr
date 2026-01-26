#' Budget R6 Class
#'
#' R6 class representing a GnuCash budget.
#' Budgets have a defined period structure and amounts per account.
#'
#' @export
Budget <- R6::R6Class("Budget",
  public = list(
    #' @description Create a new Budget object
    #' @param guid Unique identifier (32-character hex)
    #' @param name Budget name
    #' @param description Optional description
    #' @param num_periods Number of budget periods (12 for monthly, 4 for quarterly)
    #' @param recurrence_period_type Period type (month, quarter, year)
    #' @param recurrence_mult Multiplier for period
    #' @param recurrence_start Start date for budget periods
    initialize = function(guid = NULL,
                         name,
                         description = NA_character_,
                         num_periods = 12L,
                         recurrence_period_type = "month",
                         recurrence_mult = 1L,
                         recurrence_start = NULL) {
      if (is.null(guid)) {
        guid <- generate_guid()
      }

      if (!validate_guid(guid)) {
        rlang::abort("Invalid GUID format")
      }

      private$.guid <- guid
      private$.name <- name
      private$.description <- description
      private$.num_periods <- as.integer(num_periods)
      private$.recurrence_period_type <- recurrence_period_type
      private$.recurrence_mult <- as.integer(recurrence_mult)
      private$.recurrence_start <- recurrence_start %||% as.Date(paste0(
        lubridate::year(Sys.Date()), "-01-01"
      ))
      private$.amounts <- list()

      invisible(self)
    },

    #' @description Set budget amount for an account and period
    #' @param account_guid Account GUID
    #' @param period_num Period number (1-indexed)
    #' @param amount Budget amount
    set_amount = function(account_guid, period_num, amount) {
      if (period_num < 1 || period_num > private$.num_periods) {
        rlang::abort(paste("Period must be between 1 and", private$.num_periods))
      }

      key <- paste(account_guid, period_num, sep = ":")
      result <- double_to_fraction(amount, 100L)

      private$.amounts[[key]] <- list(
        account_guid = account_guid,
        period_num = as.integer(period_num),
        amount_num = result[1, 1],
        amount_denom = result[1, 2]
      )

      invisible(self)
    },

    #' @description Get budget amount for an account and period
    #' @param account_guid Account GUID
    #' @param period_num Period number (1-indexed)
    get_amount = function(account_guid, period_num) {
      key <- paste(account_guid, period_num, sep = ":")
      entry <- private$.amounts[[key]]

      if (is.null(entry)) return(0)
      fraction_to_double(entry$amount_num, entry$amount_denom)
    },

    #' @description Get all budget amounts for an account
    #' @param account_guid Account GUID
    get_account_amounts = function(account_guid) {
      purrr::map_dbl(seq_len(private$.num_periods), function(period) {
        self$get_amount(account_guid, period)
      })
    },

    #' @description Get total budget for an account across all periods
    #' @param account_guid Account GUID
    get_account_total = function(account_guid) {
      sum(self$get_account_amounts(account_guid))
    },

    #' @description Get date range for a period
    #' @param period_num Period number (1-indexed)
    get_period_dates = function(period_num) {
      if (period_num < 1 || period_num > private$.num_periods) {
        rlang::abort(paste("Period must be between 1 and", private$.num_periods))
      }

      # Calculate period offset
      offset <- (period_num - 1L) * private$.recurrence_mult

      start <- switch(private$.recurrence_period_type,
        "month" = lubridate::add_with_rollback(
          private$.recurrence_start,
          months(offset)
        ),
        "quarter" = lubridate::add_with_rollback(
          private$.recurrence_start,
          months(offset * 3)
        ),
        "year" = lubridate::add_with_rollback(
          private$.recurrence_start,
          lubridate::years(offset)
        ),
        "week" = private$.recurrence_start + lubridate::weeks(offset),
        private$.recurrence_start  # default
      )

      end_offset <- private$.recurrence_mult
      end <- switch(private$.recurrence_period_type,
        "month" = lubridate::add_with_rollback(start, months(end_offset)) - 1,
        "quarter" = lubridate::add_with_rollback(start, months(end_offset * 3)) - 1,
        "year" = lubridate::add_with_rollback(start, lubridate::years(end_offset)) - 1,
        "week" = start + lubridate::weeks(end_offset) - 1,
        start + 30  # default
      )

      list(start = start, end = end)
    },

    #' @description Get all period labels
    period_labels = function() {
      purrr::map_chr(seq_len(private$.num_periods), function(p) {
        dates <- self$get_period_dates(p)
        format(dates$start, "%b %Y")
      })
    },

    #' @description Get all accounts with budget amounts
    budgeted_accounts = function() {
      unique(purrr::map_chr(private$.amounts, ~ .x$account_guid))
    },

    #' @description Get budget amounts as a tibble
    amounts_as_tibble = function() {
      if (length(private$.amounts) == 0) {
        return(tibble::tibble(
          budget_guid = character(),
          account_guid = character(),
          period_num = integer(),
          amount_num = integer(),
          amount_denom = integer(),
          amount = numeric()
        ))
      }

      purrr::map_dfr(private$.amounts, function(entry) {
        tibble::tibble(
          budget_guid = private$.guid,
          account_guid = entry$account_guid,
          period_num = entry$period_num,
          amount_num = entry$amount_num,
          amount_denom = entry$amount_denom,
          amount = fraction_to_double(entry$amount_num, entry$amount_denom)
        )
      })
    },

    #' @description Convert to data frame row
    as_tibble = function() {
      tibble::tibble(
        guid = private$.guid,
        name = private$.name,
        description = private$.description,
        num_periods = private$.num_periods,
        recurrence_period_type = private$.recurrence_period_type,
        recurrence_mult = private$.recurrence_mult,
        recurrence_start = as.character(private$.recurrence_start)
      )
    },

    #' @description Print method
    print = function() {
      cat("<Budget>\n")
      cat("  Name:", private$.name, "\n")
      if (!is.na(private$.description)) {
        cat("  Description:", private$.description, "\n")
      }
      cat("  Periods:", private$.num_periods, private$.recurrence_period_type, "\n")
      cat("  Start:", format(private$.recurrence_start, "%Y-%m-%d"), "\n")
      cat("  Accounts:", length(self$budgeted_accounts()), "\n")
      invisible(self)
    }
  ),

  active = list(
    #' @field guid Budget GUID (read-only)
    guid = function() private$.guid,

    #' @field name Budget name
    name = function(value) {
      if (missing(value)) return(private$.name)
      private$.name <- value
    },

    #' @field description Budget description
    description = function(value) {
      if (missing(value)) return(private$.description)
      private$.description <- value
    },

    #' @field num_periods Number of periods
    num_periods = function() private$.num_periods,

    #' @field recurrence_period_type Period type
    recurrence_period_type = function() private$.recurrence_period_type,

    #' @field recurrence_mult Period multiplier
    recurrence_mult = function() private$.recurrence_mult,

    #' @field recurrence_start Start date
    recurrence_start = function() private$.recurrence_start
  ),

  private = list(
    .guid = NULL,
    .name = NULL,
    .description = NULL,
    .num_periods = NULL,
    .recurrence_period_type = NULL,
    .recurrence_mult = NULL,
    .recurrence_start = NULL,
    .amounts = list()
  )
)


#' Create a Monthly Budget
#'
#' Helper function to create a budget with monthly periods.
#'
#' @param name Budget name
#' @param year Budget year (default: current year)
#' @param description Optional description
#' @return A Budget object
#' @export
#' @examples
#' \dontrun{
#' budget <- monthly_budget("2024 Operating Budget", 2024)
#' }
monthly_budget <- function(name, year = lubridate::year(Sys.Date()),
                           description = NA_character_) {
  Budget$new(
    name = name,
    description = description,
    num_periods = 12L,
    recurrence_period_type = "month",
    recurrence_mult = 1L,
    recurrence_start = as.Date(paste0(year, "-01-01"))
  )
}


#' Create a Quarterly Budget
#'
#' Helper function to create a budget with quarterly periods.
#'
#' @param name Budget name
#' @param year Budget year (default: current year)
#' @param description Optional description
#' @return A Budget object
#' @export
quarterly_budget <- function(name, year = lubridate::year(Sys.Date()),
                             description = NA_character_) {
  Budget$new(
    name = name,
    description = description,
    num_periods = 4L,
    recurrence_period_type = "month",
    recurrence_mult = 3L,
    recurrence_start = as.Date(paste0(year, "-01-01"))
  )
}


#' Create an Annual Budget
#'
#' Helper function to create a budget with a single annual period.
#'
#' @param name Budget name
#' @param year Budget year (default: current year)
#' @param description Optional description
#' @return A Budget object
#' @export
annual_budget <- function(name, year = lubridate::year(Sys.Date()),
                          description = NA_character_) {
  Budget$new(
    name = name,
    description = description,
    num_periods = 1L,
    recurrence_period_type = "year",
    recurrence_mult = 1L,
    recurrence_start = as.Date(paste0(year, "-01-01"))
  )
}


#' BudgetComparison R6 Class
#'
#' R6 class for comparing budget to actuals.
#'
#' @export
BudgetComparison <- R6::R6Class("BudgetComparison",
  public = list(
    #' @description Create a new BudgetComparison
    #' @param budget A Budget object
    initialize = function(budget) {
      if (!inherits(budget, "Budget")) {
        rlang::abort("Expected a Budget object")
      }
      private$.budget <- budget
      private$.actuals <- list()

      invisible(self)
    },

    #' @description Set actual amount for an account and period
    #' @param account_guid Account GUID
    #' @param period_num Period number
    #' @param amount Actual amount
    set_actual = function(account_guid, period_num, amount) {
      key <- paste(account_guid, period_num, sep = ":")
      private$.actuals[[key]] <- amount
      invisible(self)
    },

    #' @description Get actual amount for an account and period
    #' @param account_guid Account GUID
    #' @param period_num Period number
    get_actual = function(account_guid, period_num) {
      key <- paste(account_guid, period_num, sep = ":")
      private$.actuals[[key]] %||% 0
    },

    #' @description Get variance (actual - budget) for an account and period
    #' @param account_guid Account GUID
    #' @param period_num Period number
    variance = function(account_guid, period_num) {
      actual <- self$get_actual(account_guid, period_num)
      budget <- private$.budget$get_amount(account_guid, period_num)
      actual - budget
    },

    #' @description Get variance percentage for an account and period
    #' @param account_guid Account GUID
    #' @param period_num Period number
    variance_pct = function(account_guid, period_num) {
      budget <- private$.budget$get_amount(account_guid, period_num)
      if (budget == 0) return(NA_real_)
      self$variance(account_guid, period_num) / abs(budget) * 100
    },

    #' @description Get comparison summary for an account
    #' @param account_guid Account GUID
    account_summary = function(account_guid) {
      periods <- seq_len(private$.budget$num_periods)

      tibble::tibble(
        period = periods,
        period_label = private$.budget$period_labels(),
        budget = purrr::map_dbl(periods, ~
          private$.budget$get_amount(account_guid, .x)),
        actual = purrr::map_dbl(periods, ~
          self$get_actual(account_guid, .x)),
        variance = purrr::map_dbl(periods, ~
          self$variance(account_guid, .x)),
        variance_pct = purrr::map_dbl(periods, ~
          self$variance_pct(account_guid, .x))
      )
    },

    #' @description Get full comparison as tibble
    as_tibble = function() {
      accounts <- private$.budget$budgeted_accounts()

      purrr::map_dfr(accounts, function(acct) {
        summary <- self$account_summary(acct)
        summary$account_guid <- acct
        summary
      })
    },

    #' @description Print method
    print = function() {
      cat("<BudgetComparison>\n")
      cat("  Budget:", private$.budget$name, "\n")
      cat("  Accounts:", length(private$.budget$budgeted_accounts()), "\n")
      cat("  Actuals Loaded:", length(private$.actuals), "\n")
      invisible(self)
    }
  ),

  active = list(
    #' @field budget The budget being compared
    budget = function() private$.budget
  ),

  private = list(
    .budget = NULL,
    .actuals = list()
  )
)


#' Convert Data Frame to Budget Object
#'
#' Create a Budget from budget and budget_amounts data frames.
#'
#' @param budget_row Single row from budgets table
#' @param amounts_df Budget amounts data frame
#' @return A Budget object
#' @export
budget_from_df <- function(budget_row, amounts_df = NULL) {
  budget <- Budget$new(
    guid = budget_row$guid,
    name = budget_row$name,
    description = budget_row$description %||% NA_character_,
    num_periods = budget_row$num_periods %||% 12L,
    recurrence_period_type = budget_row$recurrence_period_type %||% "month",
    recurrence_mult = budget_row$recurrence_mult %||% 1L
  )

  # Add amounts if provided
  if (!is.null(amounts_df) && nrow(amounts_df) > 0) {
    for (i in seq_len(nrow(amounts_df))) {
      row <- amounts_df[i, ]
      amount <- if (!is.null(row$amount_num) && !is.null(row$amount_denom)) {
        fraction_to_double(row$amount_num, row$amount_denom)
      } else {
        row$amount %||% 0
      }
      budget$set_amount(row$account_guid, row$period_num, amount)
    }
  }

  budget
}
