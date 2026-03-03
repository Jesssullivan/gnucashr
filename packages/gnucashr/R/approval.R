#' Approval Queue Operations
#'
#' Create, query, approve, and reject approval requests for write operations
#' that require human authorization.
#'
#' @name approval
NULL

#' Open Approval Database
#'
#' Opens or creates the approval database for a GnuCash book.
#' The database is stored at \code{<book_path>.approvals.db}.
#'
#' @param book_path Path to the GnuCash book file.
#' @return External pointer to the approval database.
#' @export
approval_open <- function(book_path) {
  stopifnot(is.character(book_path), length(book_path) == 1)
  gc_approval_open(book_path)
}

#' Close Approval Database
#'
#' @param approval_ptr External pointer from \code{\link{approval_open}}.
#' @export
approval_close <- function(approval_ptr) {
  gc_approval_close(approval_ptr)
}

#' Create Approval Request
#'
#' Submits a new request for approval of a tool operation.
#'
#' @param approval_ptr External pointer from \code{\link{approval_open}}.
#' @param agent_name Name of the agent making the request.
#' @param tool_name MCP tool name requiring approval.
#' @param arguments Named list or JSON string of tool arguments.
#' @param requesting_user User identity.
#' @param reason Why approval is needed.
#' @return Request ID (GUID string).
#' @export
approval_create <- function(approval_ptr, agent_name, tool_name,
                             arguments = "{}", requesting_user, reason) {
  if (is.list(arguments)) {
    arguments <- jsonlite::toJSON(arguments, auto_unbox = TRUE)
  }
  gc_approval_create(approval_ptr, agent_name, tool_name,
                      as.character(arguments), requesting_user, reason)
}

#' List Pending Approval Requests
#'
#' @param approval_ptr External pointer from \code{\link{approval_open}}.
#' @param limit Maximum number of requests (default 50).
#' @return Data frame of pending requests.
#' @export
approval_pending <- function(approval_ptr, limit = 50L) {
  gc_approval_pending(approval_ptr, as.integer(limit))
}

#' Approve a Request
#'
#' @param approval_ptr External pointer from \code{\link{approval_open}}.
#' @param id Request ID to approve.
#' @param approver Identity of the approver.
#' @export
approval_approve <- function(approval_ptr, id, approver) {
  stopifnot(is.character(id), is.character(approver))
  gc_approval_approve(approval_ptr, id, approver)
}

#' Reject a Request
#'
#' @param approval_ptr External pointer from \code{\link{approval_open}}.
#' @param id Request ID to reject.
#' @param approver Identity of the approver.
#' @param reason Reason for rejection.
#' @export
approval_reject <- function(approval_ptr, id, approver, reason) {
  stopifnot(is.character(id), is.character(approver), is.character(reason))
  gc_approval_reject(approval_ptr, id, approver, reason)
}

#' Get Approval Request Details
#'
#' @param approval_ptr External pointer from \code{\link{approval_open}}.
#' @param id Request ID.
#' @return Named list with request details, or \code{NULL} if not found.
#' @export
approval_get <- function(approval_ptr, id) {
  stopifnot(is.character(id))
  gc_approval_get(approval_ptr, id)
}
