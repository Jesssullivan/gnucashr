#' Account Templates
#'
#' Functions for loading and applying account template structures.
#' Templates define standard chart of accounts for different entity types.
#'
#' @name account-templates
NULL


#' List Available Account Templates
#'
#' Get a list of built-in account templates.
#'
#' @return A tibble with template names and descriptions
#' @export
#' @examples
#' \dontrun{
#' list_templates()
#' }
list_templates <- function() {
  template_dir <- system.file("templates", package = "gnucashr")

  if (template_dir == "") {
    rlang::warn("No template directory found. Package may not be installed.")
    return(tibble::tibble(
      name = character(),
      description = character(),
      file = character()
    ))
  }

  files <- list.files(template_dir, pattern = "\\.json$", full.names = TRUE)

  purrr::map_dfr(files, function(file) {
    template <- jsonlite::fromJSON(file, simplifyVector = FALSE)
    tibble::tibble(
      name = template$name %||% basename(file),
      description = template$description %||% NA_character_,
      currency = template$currency %||% "USD",
      file = basename(file)
    )
  })
}


#' Load an Account Template
#'
#' Load an account template from the built-in templates or a custom file.
#'
#' @param template Template name or path to JSON file
#' @return A nested list structure representing the chart of accounts
#' @export
#' @examples
#' \dontrun{
#' template <- load_template("small-business")
#' template <- load_template("c-corporation")
#' }
load_template <- function(template) {
  # Check if it's a file path
  if (file.exists(template)) {
    return(jsonlite::fromJSON(template, simplifyVector = FALSE))
  }

  # Try to find in built-in templates
  template_dir <- system.file("templates", package = "gnucashr")

  if (template_dir == "") {
    rlang::abort("Template directory not found. Package may not be installed.")
  }

  # Try exact match first
  file <- file.path(template_dir, paste0(template, ".json"))
  if (file.exists(file)) {
    return(jsonlite::fromJSON(file, simplifyVector = FALSE))
  }

  # Try case-insensitive match
  files <- list.files(template_dir, pattern = "\\.json$", full.names = TRUE)
  matching <- files[tolower(basename(files)) ==
                    tolower(paste0(template, ".json"))]

  if (length(matching) > 0) {
    return(jsonlite::fromJSON(matching[1], simplifyVector = FALSE))
  }

  rlang::abort(paste("Template not found:", template))
}


#' Flatten Account Template to Data Frame
#'
#' Convert a hierarchical account template to a flat data frame.
#' Useful for importing into GnuCash.
#'
#' @param template Template list (from load_template) or template name
#' @param include_guids Generate GUIDs for each account
#' @return A tibble with account data
#' @export
#' @examples
#' \dontrun{
#' accounts <- flatten_template("small-business")
#' }
flatten_template <- function(template, include_guids = TRUE) {
  if (is.character(template)) {
    template <- load_template(template)
  }

  accounts <- template$accounts
  if (is.null(accounts)) {
    rlang::abort("Template does not contain 'accounts' field")
  }

  # Recursive function to flatten
  flatten_accounts <- function(account_list, parent_path = "", parent_guid = NA_character_,
                               depth = 0) {
    purrr::map_dfr(account_list, function(acct) {
      path <- if (parent_path == "") acct$name else paste(parent_path, acct$name, sep = ":")
      guid <- if (include_guids) generate_guid() else NA_character_

      row <- tibble::tibble(
        guid = guid,
        name = acct$name,
        full_path = path,
        account_type = acct$type %||% "ASSET",
        parent_guid = parent_guid,
        description = acct$description %||% NA_character_,
        placeholder = acct$placeholder %||% FALSE,
        hidden = acct$hidden %||% FALSE,
        depth = depth
      )

      # Process children recursively
      children <- if (!is.null(acct$children)) {
        flatten_accounts(acct$children, path, guid, depth + 1)
      } else {
        tibble::tibble()
      }

      dplyr::bind_rows(row, children)
    })
  }

  flatten_accounts(accounts)
}


#' Create Account Hierarchy Diagram
#'
#' Generate a text-based visualization of the account hierarchy.
#'
#' @param template Template list (from load_template) or template name
#' @param max_depth Maximum depth to display (NULL for all)
#' @return Character string with hierarchy diagram
#' @export
#' @examples
#' \dontrun{
#' cat(template_diagram("small-business"))
#' }
template_diagram <- function(template, max_depth = NULL) {
  if (is.character(template)) {
    template <- load_template(template)
  }

  accounts <- template$accounts
  lines <- c(paste0("# ", template$name %||% "Account Template"))

  # Recursive function to build diagram
  build_diagram <- function(account_list, prefix = "", depth = 0) {
    if (!is.null(max_depth) && depth > max_depth) return(character())

    purrr::map_chr(seq_along(account_list), function(i) {
      acct <- account_list[[i]]
      is_last <- i == length(account_list)

      # Choose connector
      connector <- if (is_last) "\u2514\u2500\u2500 " else "\u251C\u2500\u2500 "
      next_prefix <- paste0(prefix, if (is_last) "    " else "\u2502   ")

      # Account line with type indicator
      type_badge <- substr(acct$type %||% "ASSET", 1, 3)
      line <- paste0(prefix, connector, acct$name, " [", type_badge, "]")

      # Process children
      child_lines <- if (!is.null(acct$children)) {
        build_diagram(acct$children, next_prefix, depth + 1)
      } else {
        character()
      }

      paste(c(line, child_lines), collapse = "\n")
    }) |> paste(collapse = "\n")
  }

  paste(c(lines, build_diagram(accounts)), collapse = "\n")
}


#' Apply Template to GnuCash Database
#'
#' Create accounts from a template in a GnuCash database.
#' Uses write operations to create the account structure.
#'
#' @param gc A GnuCashDB object opened for writing
#' @param template Template list (from load_template) or template name
#' @param parent_guid Optional parent account GUID (default: root)
#' @param dry_run If TRUE, show what would be created without making changes
#' @return Invisible tibble of created accounts
#' @export
apply_template <- function(gc, template, parent_guid = NULL, dry_run = FALSE) {
  if (!inherits(gc, "GnuCashDB")) {
    rlang::abort("Expected a GnuCashDB object")
  }

  if (is.character(template)) {
    template <- load_template(template)
  }

  # Get root account if parent not specified
  if (is.null(parent_guid)) {
    meta <- gc$metadata()
    parent_guid <- meta$root_account_guid
  }

  # Get commodity GUID for default currency
  commodities <- gc$commodities(collected = TRUE)
  currency_code <- template$currency %||% "USD"
  currency <- commodities |>
    dplyr::filter(mnemonic == currency_code) |>
    dplyr::slice(1)

  if (nrow(currency) == 0) {
    rlang::warn(paste("Currency", currency_code, "not found. Using first available."))
    currency <- commodities |> dplyr::slice(1)
  }

  commodity_guid <- currency$guid[1]

  # Flatten template
  accounts <- flatten_template(template, include_guids = TRUE)

  # Map parent GUIDs (template uses internal parents, need to map top-level to parent_guid)
  top_level_guids <- accounts$guid[is.na(accounts$parent_guid)]
  accounts$parent_guid[is.na(accounts$parent_guid)] <- parent_guid

  if (dry_run) {
    cat("Would create", nrow(accounts), "accounts:\n")
    print(accounts |> dplyr::select(full_path, account_type, placeholder))
    return(invisible(accounts))
  }

  # Check for create_account function
  if (!exists("create_account", mode = "function")) {
    rlang::abort("create_account function not available. Is write support loaded?")
  }

  # Create accounts in order (parents before children)
  created <- list()
  for (i in seq_len(nrow(accounts))) {
    row <- accounts[i, ]

    result <- create_account(
      gc,
      name = row$name,
      account_type = row$account_type,
      parent = row$parent_guid,
      description = row$description,
      placeholder = row$placeholder,
      hidden = row$hidden,
      commodity_guid = commodity_guid
    )

    if (inherits(result, "error")) {
      rlang::warn(paste("Failed to create account:", row$name, "-", result$message))
    } else {
      created[[length(created) + 1]] <- row
    }
  }

  invisible(dplyr::bind_rows(created))
}


#' Compare Template to Existing Accounts
#'
#' Compare a template with existing accounts in a GnuCash file.
#' Identifies missing and extra accounts.
#'
#' @param gc A GnuCashDB object
#' @param template Template list (from load_template) or template name
#' @return A list with missing and extra accounts
#' @export
compare_to_template <- function(gc, template) {
  if (!inherits(gc, "GnuCashDB")) {
    rlang::abort("Expected a GnuCashDB object")
  }

  if (is.character(template)) {
    template <- load_template(template)
  }

  # Get existing accounts
  existing <- gc$account_tree() |>
    dplyr::select(name, full_path, account_type)

  # Flatten template
  template_accounts <- flatten_template(template, include_guids = FALSE) |>
    dplyr::select(name, full_path, account_type)

  # Find missing (in template but not in file)
  missing <- template_accounts |>
    dplyr::anti_join(existing, by = "full_path")

  # Find extra (in file but not in template)
  extra <- existing |>
    dplyr::anti_join(template_accounts, by = "full_path")

  # Find type mismatches
  joined <- dplyr::inner_join(
    existing |> dplyr::select(full_path, file_type = account_type),
    template_accounts |> dplyr::select(full_path, template_type = account_type),
    by = "full_path"
  )
  mismatched <- joined |>
    dplyr::filter(file_type != template_type)

  list(
    missing = missing,
    extra = extra,
    mismatched = mismatched,
    summary = tibble::tibble(
      category = c("Missing", "Extra", "Type Mismatch"),
      count = c(nrow(missing), nrow(extra), nrow(mismatched))
    )
  )
}
