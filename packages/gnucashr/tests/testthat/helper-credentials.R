# helper-credentials.R - Credential management for gnucashr tests
#
# Provides credential handling patterns inspired by KeePassXC workflows.
# In CI environments, uses mock credentials. In local development,
# can optionally load real credentials.

# =============================================================================
# Credential Provider
# =============================================================================

#' Get Test Credentials
#'
#' Returns credentials appropriate for the current environment:
#' - In CI (detected via CI env var): Returns mock credentials
#' - In local development: Can optionally load from keyring/env
#'
#' @param name Credential name (e.g., "gnucash_test_db", "api_key")
#' @param use_real If TRUE, attempt to load real credentials locally
#' @return Named list with credential values, or NULL if not available
#' @examples
#' \dontrun{
#' creds <- test_credentials("gnucash_test_db")
#' if (!is.null(creds)) {
#'   con <- DBI::dbConnect(..., password = creds$password)
#' }
#' }
test_credentials <- function(name, use_real = FALSE) {
  # Check if we're in CI
  in_ci <- nzchar(Sys.getenv("CI")) ||
    nzchar(Sys.getenv("GITHUB_ACTIONS")) ||
    nzchar(Sys.getenv("GITLAB_CI")) ||
    nzchar(Sys.getenv("TRAVIS")) ||
    nzchar(Sys.getenv("JENKINS_URL"))

  if (in_ci || !use_real) {
    return(mock_credentials(name))
  }

  # Attempt to load real credentials
  real_creds <- load_real_credentials(name)
  if (!is.null(real_creds)) {
    return(real_creds)
  }

  # Fallback to mock
  mock_credentials(name)
}


#' Get Mock Credentials for Testing
#'
#' Returns safe mock credentials for use in tests.
#' These should never contain real secrets.
#'
#' @param name Credential name
#' @return Named list with mock credential values
mock_credentials <- function(name) {
  mocks <- list(
    gnucash_test_db = list(
      path = tempfile(fileext = ".gnucash"),
      user = "test_user",
      password = "test_password_not_real"
    ),

    api_key = list(
      key = "mock_api_key_12345",
      secret = "mock_secret_67890"
    ),

    remote_db = list(
      host = "localhost",
      port = 5432L,
      database = "test_db",
      user = "test_user",
      password = "test_password"
    )
  )

  mocks[[name]]
}


#' Load Real Credentials (Local Development Only)
#'
#' Attempts to load credentials from various sources:
#' 1. Environment variables (e.g., GNUCASHR_TEST_DB_PATH)
#' 2. System keyring (if keyring package available)
#' 3. Local credential file (~/.gnucashr/credentials.json)
#'
#' @param name Credential name
#' @return Named list with credentials, or NULL if not found
#' @noRd
load_real_credentials <- function(name) {
  # Try environment variables first
  env_creds <- load_from_env(name)
  if (!is.null(env_creds)) {
    return(env_creds)
  }

  # Try keyring if available
  if (requireNamespace("keyring", quietly = TRUE)) {
    keyring_creds <- load_from_keyring(name)
    if (!is.null(keyring_creds)) {
      return(keyring_creds)
    }
  }

  # Try local credentials file
  creds_file <- file.path(Sys.getenv("HOME"), ".gnucashr", "credentials.json")
  if (file.exists(creds_file)) {
    file_creds <- load_from_file(name, creds_file)
    if (!is.null(file_creds)) {
      return(file_creds)
    }
  }

  NULL
}


#' Load Credentials from Environment Variables
#'
#' @param name Credential name
#' @return Named list or NULL
#' @noRd
load_from_env <- function(name) {
  prefix <- paste0("GNUCASHR_", toupper(gsub("[^A-Za-z0-9]", "_", name)), "_")

  # Look for environment variables with this prefix
  all_env <- Sys.getenv()
  matching <- names(all_env)[startsWith(names(all_env), prefix)]

  if (length(matching) == 0) {
    return(NULL)
  }

  # Build credential list from matching env vars
  creds <- list()
  for (var in matching) {
    key <- tolower(sub(prefix, "", var))
    creds[[key]] <- all_env[[var]]
  }

  if (length(creds) > 0) creds else NULL
}


#' Load Credentials from System Keyring
#'
#' @param name Credential name
#' @return Named list or NULL
#' @noRd
load_from_keyring <- function(name) {
  tryCatch({
    password <- keyring::key_get(
      service = "gnucashr",
      username = name
    )

    # Assume password is JSON-encoded
    if (jsonlite::validate(password)) {
      jsonlite::fromJSON(password)
    } else {
      list(password = password)
    }
  }, error = function(e) {
    NULL
  })
}


#' Load Credentials from JSON File
#'
#' @param name Credential name
#' @param file_path Path to credentials file
#' @return Named list or NULL
#' @noRd
load_from_file <- function(name, file_path) {
  tryCatch({
    all_creds <- jsonlite::fromJSON(file_path)
    if (name %in% names(all_creds)) {
      all_creds[[name]]
    } else {
      NULL
    }
  }, error = function(e) {
    NULL
  })
}


# =============================================================================
# Skip Helpers for Credentials
# =============================================================================

#' Skip Test if Credential Is Not Available
#'
#' Use this to skip tests that require specific credentials.
#'
#' @param name Credential name
#' @param fields Optional character vector of required fields
#' @examples
#' \dontrun{
#' test_that("connects to remote database", {
#'   skip_if_no_credential("remote_db", fields = c("host", "password"))
#'   # ... test code
#' })
#' }
skip_if_no_credential <- function(name, fields = NULL) {
  creds <- test_credentials(name, use_real = TRUE)

  if (is.null(creds)) {
    testthat::skip(sprintf("Credential '%s' not available", name))
  }

  if (!is.null(fields)) {
    missing <- setdiff(fields, names(creds))
    if (length(missing) > 0) {
      testthat::skip(sprintf(
        "Credential '%s' missing required fields: %s",
        name, paste(missing, collapse = ", ")
      ))
    }
  }

  invisible(TRUE)
}


#' Skip Test If Running in CI Environment
#'
#' Some tests should only run locally with real credentials.
skip_if_ci <- function() {
  in_ci <- nzchar(Sys.getenv("CI")) ||
    nzchar(Sys.getenv("GITHUB_ACTIONS")) ||
    nzchar(Sys.getenv("GITLAB_CI"))

  if (in_ci) {
    testthat::skip("Test skipped in CI environment")
  }

  invisible(TRUE)
}


#' Skip Test If Not Running in CI Environment
#'
#' Some tests should only run in CI (e.g., integration tests with services).
skip_if_not_ci <- function() {
  in_ci <- nzchar(Sys.getenv("CI")) ||
    nzchar(Sys.getenv("GITHUB_ACTIONS")) ||
    nzchar(Sys.getenv("GITLAB_CI"))

  if (!in_ci) {
    testthat::skip("Test only runs in CI environment")
  }

  invisible(TRUE)
}
