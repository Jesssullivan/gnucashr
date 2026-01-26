#' Quarto Dashboard Widgets
#'
#' Functions for creating formatted tables and visualizations
#' for financial dashboards in Quarto.
#'
#' @name quarto-widgets
#' @keywords internal
NULL

#' Format Trial Balance as gt Table
#'
#' Creates a formatted gt table from a trial balance tibble.
#'
#' @param tb Trial balance tibble from trial_balance()
#' @param title Optional table title
#' @param currency Currency symbol (default "$")
#' @return A gt table object
#' @export
#' @examples
#' \dontrun{
#' gc <- read_gnucash("books.gnucash")
#' tb <- trial_balance(gc)
#' gt_trial_balance(tb, title = "Trial Balance")
#' }
gt_trial_balance <- function(tb, title = NULL, currency = "$") {
  if (!requireNamespace("gt", quietly = TRUE)) {
    rlang::abort("Package 'gt' is required for formatted tables")
  }

  tb |>
    dplyr::select(name, account_type, debit, credit, balance) |>
    gt::gt() |>
    gt::tab_header(title = title %||% "Trial Balance") |>
    gt::fmt_currency(
      columns = c(debit, credit, balance),
      currency = currency,
      use_subunits = TRUE
    ) |>
    gt::cols_label(
      name = "Account",
      account_type = "Type",
      debit = "Debit",
      credit = "Credit",
      balance = "Balance"
    ) |>
    gt::tab_options(
      table.font.size = "small",
      row.striping.include_table_body = TRUE
    )
}

#' Format Balance Sheet as gt Table
#'
#' Creates a formatted gt table from a balance sheet.
#'
#' @param bs Balance sheet list from balance_sheet()
#' @param title Optional table title
#' @param currency Currency symbol (default "$")
#' @return A gt table object
#' @export
gt_balance_sheet <- function(bs, title = NULL, currency = "$") {
  if (!requireNamespace("gt", quietly = TRUE)) {
    rlang::abort("Package 'gt' is required for formatted tables")
  }

  # Combine sections
  sections <- dplyr::bind_rows(
    dplyr::mutate(bs$assets, section = "Assets"),
    dplyr::mutate(bs$liabilities, section = "Liabilities"),
    dplyr::mutate(bs$equity, section = "Equity")
  )

  sections |>
    dplyr::select(section, name, balance) |>
    gt::gt(groupname_col = "section") |>
    gt::tab_header(title = title %||% "Balance Sheet") |>
    gt::fmt_currency(
      columns = balance,
      currency = currency,
      use_subunits = TRUE
    ) |>
    gt::cols_label(
      name = "Account",
      balance = "Balance"
    ) |>
    gt::summary_rows(
      groups = TRUE,
      columns = balance,
      fns = list(Total = ~ sum(.))
    ) |>
    gt::tab_options(
      table.font.size = "small",
      row_group.font.weight = "bold"
    )
}

#' Format Income Statement as gt Table
#'
#' Creates a formatted gt table from an income statement.
#'
#' @param is Income statement list from income_statement()
#' @param title Optional table title
#' @param currency Currency symbol (default "$")
#' @return A gt table object
#' @export
gt_income_statement <- function(is, title = NULL, currency = "$") {
  if (!requireNamespace("gt", quietly = TRUE)) {
    rlang::abort("Package 'gt' is required for formatted tables")
  }

  # Combine sections
  sections <- dplyr::bind_rows(
    dplyr::mutate(is$income, section = "Income", amount = -balance),
    dplyr::mutate(is$expenses, section = "Expenses", amount = balance)
  )

  sections |>
    dplyr::select(section, name, amount) |>
    gt::gt(groupname_col = "section") |>
    gt::tab_header(title = title %||% "Income Statement") |>
    gt::fmt_currency(
      columns = amount,
      currency = currency,
      use_subunits = TRUE
    ) |>
    gt::cols_label(
      name = "Account",
      amount = "Amount"
    ) |>
    gt::summary_rows(
      groups = TRUE,
      columns = amount,
      fns = list(Total = ~ sum(.))
    ) |>
    gt::grand_summary_rows(
      columns = amount,
      fns = list(`Net Income` = ~ sum(sections$amount[sections$section == "Income"]) -
                   sum(sections$amount[sections$section == "Expenses"]))
    ) |>
    gt::tab_options(
      table.font.size = "small",
      row_group.font.weight = "bold"
    )
}

#' Create Balance Sheet Bar Chart
#'
#' Creates a ggplot2 bar chart showing balance sheet composition.
#'
#' @param bs Balance sheet list from balance_sheet()
#' @param title Optional plot title
#' @return A ggplot2 object
#' @export
plot_balance_sheet <- function(bs, title = NULL) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    rlang::abort("Package 'ggplot2' is required for plots")
  }
  if (!requireNamespace("scales", quietly = TRUE)) {
    rlang::abort("Package 'scales' is required for formatting")
  }

  # Calculate totals
  totals <- tibble::tibble(
    section = c("Assets", "Liabilities", "Equity"),
    total = c(
      sum(bs$assets$balance, na.rm = TRUE),
      -sum(bs$liabilities$balance, na.rm = TRUE),
      -sum(bs$equity$balance, na.rm = TRUE)
    )
  )

  ggplot2::ggplot(totals, ggplot2::aes(x = section, y = total, fill = section)) +
    ggplot2::geom_col(show.legend = FALSE) +
    ggplot2::scale_y_continuous(labels = scales::dollar_format()) +
    ggplot2::scale_fill_manual(values = c(
      "Assets" = "#2E86AB",
      "Liabilities" = "#E94F37",
      "Equity" = "#59CD90"
    )) +
    ggplot2::labs(
      title = title %||% "Balance Sheet Overview",
      x = NULL,
      y = "Amount"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold"),
      panel.grid.major.x = ggplot2::element_blank()
    )
}

#' Create Monthly Activity Line Chart
#'
#' Creates a ggplot2 line chart showing monthly account activity.
#'
#' @param activity Monthly activity tibble from monthly_activity()
#' @param account_filter Optional vector of account names to include
#' @param title Optional plot title
#' @return A ggplot2 object
#' @export
plot_monthly_activity <- function(activity, account_filter = NULL, title = NULL) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    rlang::abort("Package 'ggplot2' is required for plots")
  }
  if (!requireNamespace("scales", quietly = TRUE)) {
    rlang::abort("Package 'scales' is required for formatting")
  }

  if (!is.null(account_filter)) {
    activity <- dplyr::filter(activity, name %in% account_filter)
  }

  ggplot2::ggplot(activity, ggplot2::aes(x = month, y = total, color = name)) +
    ggplot2::geom_line(linewidth = 1) +
    ggplot2::geom_point(size = 2) +
    ggplot2::scale_y_continuous(labels = scales::dollar_format()) +
    ggplot2::labs(
      title = title %||% "Monthly Activity",
      x = "Month",
      y = "Amount",
      color = "Account"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold"),
      legend.position = "bottom"
    )
}

#' Create Monte Carlo Fan Chart
#'
#' Creates a ggplot2 fan chart showing Monte Carlo simulation results.
#'
#' @param mc_result Monte Carlo result from lf_collect() or quick_monte_carlo()
#' @param title Optional plot title
#' @return A ggplot2 object
#' @export
plot_monte_carlo <- function(mc_result, title = NULL) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    rlang::abort("Package 'ggplot2' is required for plots")
  }
  if (!requireNamespace("scales", quietly = TRUE)) {
    rlang::abort("Package 'scales' is required for formatting")
  }

  # Extract summary data
  summary_df <- mc_result$summary

  # Create period column if needed
  if (!"period" %in% names(summary_df)) {
    summary_df$period <- seq_len(nrow(summary_df))
  }

  ggplot2::ggplot(summary_df, ggplot2::aes(x = period)) +
    # 90% confidence interval
    ggplot2::geom_ribbon(
      ggplot2::aes(ymin = p5, ymax = p95),
      fill = "#2E86AB",
      alpha = 0.2
    ) +
    # 50% confidence interval
    ggplot2::geom_ribbon(
      ggplot2::aes(ymin = p25, ymax = p75),
      fill = "#2E86AB",
      alpha = 0.3
    ) +
    # Median line
    ggplot2::geom_line(
      ggplot2::aes(y = p50),
      color = "#2E86AB",
      linewidth = 1.2
    ) +
    ggplot2::scale_y_continuous(labels = scales::dollar_format()) +
    ggplot2::labs(
      title = title %||% "Monte Carlo Simulation",
      subtitle = "Median with 50% and 90% confidence intervals",
      x = "Period",
      y = "Cumulative Value"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold"),
      plot.subtitle = ggplot2::element_text(color = "gray50")
    )
}

#' Create Sensitivity Heatmap
#'
#' Creates a ggplot2 heatmap showing sensitivity analysis results.
#'
#' @param sensitivity Sensitivity result from lf_collect() or quick_sensitivity()
#' @param title Optional plot title
#' @return A ggplot2 object
#' @export
plot_sensitivity <- function(sensitivity, title = NULL) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    rlang::abort("Package 'ggplot2' is required for plots")
  }
  if (!requireNamespace("scales", quietly = TRUE)) {
    rlang::abort("Package 'scales' is required for formatting")
  }

  # Extract the outcomes matrix
  outcomes <- sensitivity$outcomes
  growth_range <- sensitivity$growth_range
  expense_range <- sensitivity$expense_range

  # Convert to long format
  outcomes_df <- expand.grid(
    growth = growth_range,
    expense = expense_range
  )
  outcomes_df$value <- as.vector(outcomes)

  ggplot2::ggplot(outcomes_df, ggplot2::aes(
    x = growth,
    y = expense,
    fill = value
  )) +
    ggplot2::geom_tile() +
    ggplot2::scale_fill_gradient2(
      low = "#E94F37",
      mid = "#F7F7F7",
      high = "#59CD90",
      midpoint = median(outcomes_df$value),
      labels = scales::dollar_format()
    ) +
    ggplot2::scale_x_continuous(labels = scales::percent_format()) +
    ggplot2::scale_y_continuous(labels = scales::percent_format()) +
    ggplot2::labs(
      title = title %||% "Sensitivity Analysis",
      x = "Growth Rate",
      y = "Expense Rate",
      fill = "Final Value"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold"),
      legend.position = "right"
    )
}

#' Create Entity Comparison Chart
#'
#' Creates a ggplot2 bar chart comparing metrics across entities.
#'
#' @param collection BookCollection object
#' @param metric Metric to compare ("balance", "income", "expense")
#' @param as_of Date for comparison
#' @param title Optional plot title
#' @return A ggplot2 object
#' @export
plot_entity_comparison <- function(collection, metric = "balance", as_of = Sys.Date(), title = NULL) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    rlang::abort("Package 'ggplot2' is required for plots")
  }
  if (!requireNamespace("scales", quietly = TRUE)) {
    rlang::abort("Package 'scales' is required for formatting")
  }

  # Get trial balances for each book
  all_tb <- collection$each(function(gc) {
    trial_balance(gc, as_of = as_of)
  })

  # Calculate metric for each entity
  entity_data <- purrr::imap_dfr(all_tb, function(tb, name) {
    value <- switch(metric,
      "balance" = sum(tb$balance[tb$account_type %in% c("ASSET", "BANK", "CASH")], na.rm = TRUE),
      "income" = sum(abs(tb$balance[tb$account_type == "INCOME"]), na.rm = TRUE),
      "expense" = sum(tb$balance[tb$account_type == "EXPENSE"], na.rm = TRUE),
      sum(tb$balance, na.rm = TRUE)
    )
    tibble::tibble(entity = name, value = value)
  })

  ggplot2::ggplot(entity_data, ggplot2::aes(
    x = stats::reorder(entity, -value),
    y = value,
    fill = entity
  )) +
    ggplot2::geom_col(show.legend = FALSE) +
    ggplot2::scale_y_continuous(labels = scales::dollar_format()) +
    ggplot2::labs(
      title = title %||% paste("Entity Comparison:", tools::toTitleCase(metric)),
      x = NULL,
      y = tools::toTitleCase(metric)
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold"),
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
    )
}

#' Create Value Boxes for Dashboard
#'
#' Returns a list of key metrics formatted for Quarto value boxes.
#'
#' @param gc GnuCashDB object
#' @param as_of Date for metrics calculation
#' @return Named list with metrics (total_assets, total_liabilities, net_worth, etc.)
#' @export
dashboard_metrics <- function(gc, as_of = Sys.Date()) {
  tb <- trial_balance(gc, as_of = as_of)

  list(
    total_assets = sum(tb$balance[tb$account_type %in% c("ASSET", "BANK", "CASH")], na.rm = TRUE),
    total_liabilities = abs(sum(tb$balance[tb$account_type == "LIABILITY"], na.rm = TRUE)),
    net_worth = sum(tb$balance[tb$account_type %in% c("ASSET", "BANK", "CASH")], na.rm = TRUE) +
      sum(tb$balance[tb$account_type == "LIABILITY"], na.rm = TRUE),
    total_income = abs(sum(tb$balance[tb$account_type == "INCOME"], na.rm = TRUE)),
    total_expenses = sum(tb$balance[tb$account_type == "EXPENSE"], na.rm = TRUE),
    net_income = abs(sum(tb$balance[tb$account_type == "INCOME"], na.rm = TRUE)) -
      sum(tb$balance[tb$account_type == "EXPENSE"], na.rm = TRUE),
    account_count = nrow(gc$accounts()),
    transaction_count = nrow(gc$transactions())
  )
}

#' Format Currency for Display
#'
#' Helper function to format currency values for display.
#'
#' @param x Numeric value
#' @param prefix Currency prefix (default "$")
#' @param suffix Currency suffix (default "")
#' @param digits Decimal digits (default 2)
#' @return Formatted character string
#' @export
format_currency <- function(x, prefix = "$", suffix = "", digits = 2) {
  formatted <- formatC(abs(x), format = "f", digits = digits, big.mark = ",")
  sign <- ifelse(x < 0, "-", "")
  paste0(sign, prefix, formatted, suffix)
}

#' Format Percentage for Display
#'
#' Helper function to format percentage values for display.
#'
#' @param x Numeric value (0.05 = 5%)
#' @param digits Decimal digits (default 1)
#' @param suffix Suffix (default "%")
#' @return Formatted character string
#' @export
format_percent <- function(x, digits = 1, suffix = "%") {
  paste0(formatC(x * 100, format = "f", digits = digits), suffix)
}
