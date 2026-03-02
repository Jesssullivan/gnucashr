# identity.R - User identity resolution
#
# Determine the current user for audit trail and provenance tracking.
# Uses CLI flag, GNUCASH_USER env var, or system username fallback.

#' Resolve User Identity
#'
#' Determine the current user identity using a priority chain:
#' CLI flag > GNUCASH_USER env var > system username.
#'
#' @param cli_identity Optional identity override string
#' @return Named list with:
#'   \describe{
#'     \item{user_id}{User identifier string}
#'     \item{display_name}{Display name for audit records}
#'     \item{node_name}{Machine hostname}
#'     \item{source}{How identity was resolved: "cli", "env", or "system"}
#'   }
#' @export
resolve_identity <- function(cli_identity = NULL) {
  gc_resolve_identity(cli_identity)
}

#' Get System Username
#'
#' Return the current system username via C++ backend.
#'
#' @return Username string
#' @export
system_username <- function() {
  gc_system_username()
}
