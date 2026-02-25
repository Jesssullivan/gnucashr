#' Backup and Restore Functions
#'
#' Functions for backing up and restoring GnuCash files before
#' write operations to ensure data safety.
#'
#' @name write-backup
NULL

#' Create Backup of GnuCash File
#'
#' Create a timestamped backup copy of a GnuCash file.
#'
#' @param gc GnuCashDB object or path to GnuCash file
#' @param backup_dir Directory for backups (default: same as source with .backup suffix)
#' @param max_backups Maximum number of backups to keep (default: 10)
#' @return Result with backup path or error
#' @export
create_backup <- function(gc, backup_dir = NULL, max_backups = 10L) {

  # Get source path
 if (inherits(gc, "GnuCashDB")) {
    source_path <- gc$metadata()$path
  } else if (is.character(gc) && file.exists(gc)) {
    source_path <- normalizePath(gc)
  } else {
    return(err("Invalid GnuCash source: provide GnuCashDB object or file path"))
  }

  # Determine backup directory
  if (is.null(backup_dir)) {
    backup_dir <- paste0(dirname(source_path), "/.gnucash-backups")
  }

  # Create backup directory if needed
  if (!dir.exists(backup_dir)) {
    dir.create(backup_dir, recursive = TRUE)
  }

  # Generate backup filename with timestamp
  base_name <- tools::file_path_sans_ext(basename(source_path))
  ext <- tools::file_ext(source_path)
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  backup_name <- sprintf("%s_%s.%s", base_name, timestamp, ext)
  backup_path <- file.path(backup_dir, backup_name)

  # Copy the file
  copy_result <- try_result(file.copy(source_path, backup_path, overwrite = FALSE))

  if (is_err(copy_result)) {
    return(err(paste("Failed to create backup:", unwrap_err(copy_result))))
  }

  if (!unwrap(copy_result)) {
    return(err("File copy returned FALSE - backup may already exist"))
  }

  # Prune old backups if needed
  prune_result <- prune_backups(backup_dir, base_name, ext, max_backups)

  # Return success with backup info
  ok(list(
    backup_path = backup_path,
    source_path = source_path,
    timestamp = Sys.time(),
    size_bytes = file.info(backup_path)$size,
    pruned = if (is_ok(prune_result)) unwrap(prune_result) else 0L
  ))
}

#' Prune Old Backups
#'
#' Remove old backups beyond the maximum count.
#'
#' @param backup_dir Backup directory
#' @param base_name Base filename
#' @param ext File extension
#' @param max_backups Maximum to keep
#' @return Result with count of pruned files
#' @noRd
prune_backups <- function(backup_dir, base_name, ext, max_backups) {
  pattern <- sprintf("^%s_\\d{8}_\\d{6}\\.%s$", base_name, ext)
  backup_files <- list.files(backup_dir, pattern = pattern, full.names = TRUE)

  if (length(backup_files) <= max_backups) {
    return(ok(0L))
  }

  # Sort by modification time (oldest first)
  file_info <- file.info(backup_files)
  sorted_files <- backup_files[order(file_info$mtime)]

  # Remove oldest files beyond limit
  to_remove <- head(sorted_files, length(sorted_files) - max_backups)
  removed <- sum(file.remove(to_remove))

  ok(as.integer(removed))
}

#' List Available Backups
#'
#' List all available backups for a GnuCash file.
#'
#' @param gc GnuCashDB object or path to GnuCash file
#' @param backup_dir Backup directory (default: auto-detect)
#' @return tibble of available backups
#' @export
list_backups <- function(gc, backup_dir = NULL) {
  # Get source path
  if (inherits(gc, "GnuCashDB")) {
    source_path <- gc$metadata()$path
  } else if (is.character(gc) && file.exists(gc)) {
    source_path <- normalizePath(gc)
  } else {
    rlang::abort("Invalid GnuCash source")
  }

  # Determine backup directory
  if (is.null(backup_dir)) {
    backup_dir <- paste0(dirname(source_path), "/.gnucash-backups")
  }

  if (!dir.exists(backup_dir)) {
    return(tibble::tibble(
      path = character(),
      timestamp = as.POSIXct(character()),
      size_bytes = integer(),
      age_hours = numeric()
    ))
  }

  # Find matching backups
  base_name <- tools::file_path_sans_ext(basename(source_path))
  ext <- tools::file_ext(source_path)
  pattern <- sprintf("^%s_\\d{8}_\\d{6}\\.%s$", base_name, ext)

  backup_files <- list.files(backup_dir, pattern = pattern, full.names = TRUE)

  if (length(backup_files) == 0) {
    return(tibble::tibble(
      path = character(),
      timestamp = as.POSIXct(character()),
      size_bytes = integer(),
      age_hours = numeric()
    ))
  }

  file_info <- file.info(backup_files)

  tibble::tibble(
    path = backup_files,
    timestamp = file_info$mtime,
    size_bytes = as.integer(file_info$size),
    age_hours = as.numeric(difftime(Sys.time(), file_info$mtime, units = "hours"))
  ) |>
    dplyr::arrange(dplyr::desc(timestamp))
}

#' Restore from Backup
#'
#' Restore a GnuCash file from a backup.
#'
#' @param backup_path Path to backup file
#' @param target_path Target path (default: original location)
#' @param create_safety_backup Create backup of current file before restore (default: TRUE)
#' @return Result with restore info or error
#' @export
restore_backup <- function(backup_path, target_path = NULL, create_safety_backup = TRUE) {
  if (!file.exists(backup_path)) {
    return(err(paste("Backup file not found:", backup_path)))
  }

  # Parse backup filename to determine original target
  backup_name <- basename(backup_path)

  # Extract original filename by removing timestamp pattern
  original_name <- sub("_\\d{8}_\\d{6}(\\.[^.]+)$", "\\1", backup_name)

  if (is.null(target_path)) {
    # Default to same directory as backup's parent
    backup_dir <- dirname(backup_path)
    parent_dir <- dirname(backup_dir)
    target_path <- file.path(parent_dir, original_name)
  }

  # Create safety backup of current file if it exists
  safety_backup_path <- NULL
  if (create_safety_backup && file.exists(target_path)) {
    timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
    safety_backup_path <- paste0(target_path, ".pre_restore_", timestamp)

    copy_result <- try_result(file.copy(target_path, safety_backup_path))
    if (is_err(copy_result)) {
      return(err(paste("Failed to create safety backup:", unwrap_err(copy_result))))
    }
  }

  # Restore the backup
  restore_result <- try_result(file.copy(backup_path, target_path, overwrite = TRUE))

  if (is_err(restore_result)) {
    return(err(paste("Failed to restore backup:", unwrap_err(restore_result))))
  }

  ok(list(
    restored_to = target_path,
    restored_from = backup_path,
    safety_backup = safety_backup_path,
    timestamp = Sys.time()
  ))
}

#' Automatic Backup Wrapper
#'
#' Execute a function with automatic backup and rollback on error.
#'
#' @param gc GnuCashDB object
#' @param fn Function to execute (receives gc as argument)
#' @param backup_dir Backup directory
#' @return Result from fn or error with rollback info
#' @export
with_backup <- function(gc, fn, backup_dir = NULL) {
  # Create backup before operation
  backup_result <- create_backup(gc, backup_dir)

  if (is_err(backup_result)) {
    return(err(paste("Pre-operation backup failed:",
                     unwrap_err(backup_result))))
  }

  backup_info <- unwrap(backup_result)

  # Execute the function
  operation_result <- try_result(fn(gc))

  if (is_err(operation_result)) {
    # Operation failed - restore from backup
    restore_result <- restore_backup(
      backup_info$backup_path,
      backup_info$source_path,
      create_safety_backup = FALSE
    )

    error_msg <- unwrap_err(operation_result)
    if (is_ok(restore_result)) {
      return(err(paste("Operation failed and rolled back:",
                       error_msg)))
    } else {
      return(err(paste("Operation failed AND rollback failed:",
                       error_msg, "|", unwrap_err(restore_result))))
    }
  }

  # Success - return result with backup info
  ok(list(
    result = unwrap(operation_result),
    backup = backup_info
  ))
}

#' Clean Old Backups
#'
#' Remove backups older than specified age.
#'
#' @param gc GnuCashDB object or path
#' @param backup_dir Backup directory
#' @param older_than_hours Remove backups older than this (default: 168 = 1 week)
#' @return Result with count of removed backups
#' @export
clean_old_backups <- function(gc, backup_dir = NULL, older_than_hours = 168) {
  backups <- list_backups(gc, backup_dir)

  if (nrow(backups) == 0) {
    return(ok(0L))
  }

  old_backups <- backups |>
    dplyr::filter(age_hours > older_than_hours)

  if (nrow(old_backups) == 0) {
    return(ok(0L))
  }

  removed <- sum(file.remove(old_backups$path))
  ok(as.integer(removed))
}

#' Backup Info Summary
#'
#' Get summary information about backups for a GnuCash file.
#'
#' @param gc GnuCashDB object or path
#' @param backup_dir Backup directory
#' @return List with backup statistics
#' @export
backup_summary <- function(gc, backup_dir = NULL) {
  backups <- list_backups(gc, backup_dir)

  list(
    count = nrow(backups),
    total_size_mb = sum(backups$size_bytes) / 1024^2,
    oldest = if (nrow(backups) > 0) min(backups$timestamp) else NA,
    newest = if (nrow(backups) > 0) max(backups$timestamp) else NA,
    avg_age_hours = if (nrow(backups) > 0) mean(backups$age_hours) else NA
  )
}
