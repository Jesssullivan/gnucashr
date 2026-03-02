# agent-state.R - Per-agent state database
#
# Each agent gets its own SQLite database for key-value state
# and a review queue for human-in-the-loop categorization.

#' Open Agent State Database
#'
#' Open or create a per-agent state database. The database is stored
#' at \code{<book_path>.agent.<agent_name>.db}.
#'
#' @param book_path Path to the GnuCash book file
#' @param agent_name Agent identifier (e.g. "spend-monitor")
#' @return External pointer to AgentStateDB
#' @export
agent_state_open <- function(book_path, agent_name) {
  gc_agent_state_open(book_path, agent_name)
}

#' Close Agent State Database
#'
#' @param state_ptr External pointer from \code{agent_state_open()}
#' @export
agent_state_close <- function(state_ptr) {
  gc_agent_state_close(state_ptr)
}

#' Set Agent State Value
#'
#' Store a key-value pair in the agent's state database.
#' Creates or updates the entry.
#'
#' @param state_ptr External pointer from \code{agent_state_open()}
#' @param key State key
#' @param value State value (character)
#' @export
agent_state_set <- function(state_ptr, key, value) {
  gc_agent_state_set(state_ptr, key, as.character(value))
}

#' Get Agent State Value
#'
#' Retrieve a value from the agent's state database.
#'
#' @param state_ptr External pointer from \code{agent_state_open()}
#' @param key State key
#' @return Value string, or NULL if key not found
#' @export
agent_state_get <- function(state_ptr, key) {
  gc_agent_state_get(state_ptr, key)
}

#' Enqueue Review Item
#'
#' Add a transaction categorization suggestion to the review queue
#' for human approval.
#'
#' @param state_ptr External pointer from \code{agent_state_open()}
#' @param transaction_guid Transaction GUID
#' @param suggested_category Target account path
#' @param confidence Confidence score 0-1
#' @param reason Reasoning for the suggestion
#' @return Integer ID of the created review item
#' @export
agent_state_enqueue_review <- function(state_ptr, transaction_guid,
                                        suggested_category, confidence,
                                        reason) {
  gc_agent_state_enqueue_review(state_ptr, transaction_guid,
                                 suggested_category,
                                 as.double(confidence),
                                 as.character(reason))
}

#' Get Pending Reviews
#'
#' Retrieve pending review items from the agent's queue.
#'
#' @param state_ptr External pointer from \code{agent_state_open()}
#' @param limit Maximum items to return (default 50)
#' @return Data frame with id, transaction_guid, suggested_category,
#'   confidence, reason, status, created_at
#' @export
agent_state_pending_reviews <- function(state_ptr, limit = 50L) {
  gc_agent_state_pending_reviews(state_ptr, as.integer(limit))
}

#' Update Review Status
#'
#' Approve or reject a review item.
#'
#' @param state_ptr External pointer from \code{agent_state_open()}
#' @param id Review item ID
#' @param status New status: "approved" or "rejected"
#' @export
agent_state_update_review <- function(state_ptr, id, status) {
  gc_agent_state_update_review(state_ptr, as.integer(id), status)
}
