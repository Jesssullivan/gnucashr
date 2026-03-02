# slots.R - GnuCash slots (key-value metadata) operations
#
# Slots store metadata on GnuCash entities: online_id (FITID for dedup),
# notes, custom fields, etc. Uses the GnuCash 5.x slots table schema.

#' Get Slot Value
#'
#' Retrieve a single slot value from the GnuCash slots table.
#'
#' @param book_ptr External pointer from \code{gc_open()}
#' @param obj_guid Entity GUID (split, account, or transaction)
#' @param name Slot name (e.g., "online_id", "notes")
#' @return Named list with slot details, or NULL if not found
#' @export
#' @examples
#' \dontrun{
#' book <- gc_open("myfile.gnucash")
#' slot <- gc_get_slot(book, split_guid, "online_id")
#' if (!is.null(slot)) cat("FITID:", slot$string_val)
#' }
get_slot <- function(book_ptr, obj_guid, name) {
  gc_get_slot(book_ptr, obj_guid, name)
}

#' Set Slot Value
#'
#' Set a string-typed slot on an entity. Creates the slot if it doesn't
#' exist, updates it if it does. Requires a read-write book handle.
#'
#' @param book_ptr External pointer from \code{gc_open(read_only = FALSE)}
#' @param obj_guid Entity GUID
#' @param name Slot name
#' @param value Slot string value
#' @return Invisible NULL
#' @export
set_slot <- function(book_ptr, obj_guid, name, value) {
  gc_set_slot(book_ptr, obj_guid, name, value)
  invisible(NULL)
}

#' Delete Slot
#'
#' Remove a slot from an entity.
#'
#' @param book_ptr External pointer from \code{gc_open(read_only = FALSE)}
#' @param obj_guid Entity GUID
#' @param name Slot name to delete
#' @return Invisible NULL
#' @export
delete_slot <- function(book_ptr, obj_guid, name) {
  gc_delete_slot(book_ptr, obj_guid, name)
  invisible(NULL)
}

#' Get All Slots for Entity
#'
#' Retrieve all slot key-value pairs for a given entity.
#'
#' @param book_ptr External pointer from \code{gc_open()}
#' @param obj_guid Entity GUID
#' @return Data frame with columns: id, name, slot_type, string_val,
#'   int64_val, double_val, guid_val
#' @export
get_all_slots <- function(book_ptr, obj_guid) {
  gc_get_all_slots(book_ptr, obj_guid)
}

#' Find Split by FITID
#'
#' Search for a split with a matching Financial Institution Transaction ID
#' (FITID). This is the core dedup function for bank feed imports.
#'
#' @param book_ptr External pointer from \code{gc_open()}
#' @param account_guid Account GUID to search within
#' @param fitid FITID string to search for
#' @return Split GUID string if found, empty string otherwise
#' @export
find_split_by_fitid <- function(book_ptr, account_guid, fitid) {
  gc_find_split_by_fitid(book_ptr, account_guid, fitid)
}

#' Batch Check FITIDs
#'
#' Check which FITIDs already exist for an account (batch dedup).
#'
#' @param book_ptr External pointer from \code{gc_open()}
#' @param account_guid Account GUID
#' @param fitids Character vector of FITIDs to check
#' @return Data frame with columns: fitid, split_guid (only matches)
#' @export
check_fitids <- function(book_ptr, account_guid, fitids) {
  gc_check_fitids(book_ptr, account_guid, fitids)
}
