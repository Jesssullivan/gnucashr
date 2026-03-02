#' Security Operations
#'
#' Tool classification, authorization checks, rate limiting, and anomaly
#' detection for MCP operations.
#'
#' @name security
NULL

#' Classify MCP Tool Authorization Level
#'
#' Determines whether a tool requires automatic approval, review, or
#' explicit approval based on its operation type.
#'
#' @param tool_name MCP tool name (e.g., \code{"gnucash_post_transaction"}).
#' @return String: \code{"auto"}, \code{"review"}, or \code{"approve"}.
#' @export
classify_tool <- function(tool_name) {
  stopifnot(is.character(tool_name), length(tool_name) == 1)
  gc_classify_tool(tool_name)
}

#' Run Security Check
#'
#' Evaluates whether a tool call should be allowed, queued for review,
#' or require explicit approval.
#'
#' @param policy A list with: \code{enforcement_enabled} (logical),
#'   \code{agent_name} (string), \code{agent_tier} (string: "auto"/"review"/"approve").
#' @param tool_name MCP tool name.
#' @param arguments Named list or JSON string of tool arguments.
#' @return Named list: \code{decision}, \code{reason}, \code{approval_id} (or NULL).
#' @export
security_check <- function(policy, tool_name, arguments = "{}") {
  stopifnot(is.list(policy))
  stopifnot(is.character(tool_name), length(tool_name) == 1)

  if (is.list(arguments)) {
    arguments <- jsonlite::toJSON(arguments, auto_unbox = TRUE)
  }

  gc_security_check(policy, tool_name, as.character(arguments))
}

#' Create Rate Limiter
#'
#' Creates a new rate limiter instance that tracks operation counts
#' per agent per hour.
#'
#' @return External pointer to a rate limiter.
#' @export
rate_limiter_create <- function() {
  gc_rate_limiter_create()
}

#' Check and Record Rate Limit
#'
#' Checks if an operation is within its rate limit and records the attempt.
#'
#' @param limiter External pointer from \code{\link{rate_limiter_create}}.
#' @param agent Agent identifier string.
#' @param operation Operation name string.
#' @param max_per_hour Maximum operations allowed per hour.
#' @return Named list: \code{allowed} (logical), \code{remaining} (integer).
#' @export
rate_limiter_check <- function(limiter, agent, operation, max_per_hour) {
  stopifnot(is.character(agent), is.character(operation))
  stopifnot(is.numeric(max_per_hour), max_per_hour >= 1)
  gc_rate_limiter_check(limiter, agent, operation, as.integer(max_per_hour))
}

#' Check Transaction Anomaly
#'
#' Detects anomalous transaction amounts that exceed a configurable threshold.
#'
#' @param arguments Named list or JSON string of tool arguments.
#' @param amount_threshold Threshold in cents (default 500000 = $5,000).
#' @return Named list: \code{is_anomalous}, \code{reason}, \code{severity}.
#' @export
check_anomaly <- function(arguments = "{}", amount_threshold = 500000) {
  if (is.list(arguments)) {
    arguments <- jsonlite::toJSON(arguments, auto_unbox = TRUE)
  }
  gc_check_anomaly(as.character(arguments), as.numeric(amount_threshold))
}
