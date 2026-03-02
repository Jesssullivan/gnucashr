# audit.R - Audit trail operations
#
# Provides an audit trail for all operations on GnuCash data.
# Records are stored in {book_path}.audit.db and are compatible
# with the Tailscale Aperture export format.

#' Open Audit Logger
#'
#' Open or create an audit database for a GnuCash book.
#' The database is stored alongside the book file at
#' \code{<book_path>.audit.db}.
#'
#' @param book_path Path to the GnuCash book file
#' @return External pointer to AuditLogger (use with other audit functions)
#' @export
audit_open <- function(book_path) {
  gc_audit_open(book_path)
}

#' Close Audit Logger
#'
#' @param audit_ptr External pointer from \code{audit_open()}
#' @export
audit_close <- function(audit_ptr) {
  gc_audit_close(audit_ptr)
}

#' Log an Audit Record
#'
#' Record a tool call in the audit log.
#'
#' @param audit_ptr External pointer from \code{audit_open()}
#' @param tool_name Name of the tool/operation
#' @param book_path Path to the GnuCash book
#' @param classification "read" or "write" (default "read")
#' @param result_status "success" or "error" (default "success")
#' @param entity_guid Optional GUID of the affected entity
#' @param error_message Optional error message (for failed operations)
#' @param duration_ms Optional execution time in milliseconds
#' @export
audit_log <- function(audit_ptr, tool_name, book_path,
                      classification = "read", result_status = "success",
                      entity_guid = NULL, error_message = NULL,
                      duration_ms = NULL) {
  gc_audit_log(audit_ptr, tool_name, book_path, classification,
               result_status, entity_guid, error_message,
               if (!is.null(duration_ms)) as.integer(duration_ms) else NULL)
}

#' Query Audit Log
#'
#' Retrieve audit records with optional filters.
#'
#' @param audit_ptr External pointer from \code{audit_open()}
#' @param since Optional start timestamp (ISO 8601)
#' @param until Optional end timestamp (ISO 8601)
#' @param tool_name Optional tool name filter (exact match)
#' @param limit Maximum records to return (default 100)
#' @return Data frame of audit records
#' @export
audit_query <- function(audit_ptr, since = NULL, until = NULL,
                        tool_name = NULL, limit = 100L) {
  gc_audit_query(audit_ptr, since, until, tool_name, as.integer(limit))
}

#' Export Audit Log in Aperture Format
#'
#' Export audit records as JSONL compatible with Tailscale Aperture.
#'
#' @param audit_ptr External pointer from \code{audit_open()}
#' @param since Optional start timestamp (ISO 8601)
#' @param until Optional end timestamp (ISO 8601)
#' @param limit Maximum records (default 100)
#' @return JSONL string
#' @export
audit_export_aperture <- function(audit_ptr, since = NULL, until = NULL,
                                  limit = 100L) {
  gc_audit_export_aperture(audit_ptr, since, until, as.integer(limit))
}
