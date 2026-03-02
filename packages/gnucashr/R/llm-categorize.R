#' LLM-Powered Transaction Categorization
#'
#' Uses large language models as a fallback for uncategorized transactions
#' when pattern-based matching fails or returns low confidence.
#'
#' @name llm-categorize
NULL

#' Categorize transactions using an LLM
#'
#' Sends batches of transaction descriptions to an LLM API and returns
#' suggested account categorizations with confidence scores.
#'
#' @param descriptions Character vector of transaction descriptions to categorize.
#' @param account_tree Character vector of full account paths from the book
#'   (e.g., \code{"Expenses:SaaS:AI"}).
#' @param provider LLM provider: \code{"anthropic"} or \code{"openai"}.
#' @param model Model identifier. Defaults to provider-specific model.
#' @param token API key. If \code{NULL}, resolved from environment variables
#'   or keyring.
#' @param batch_size Number of descriptions per API call (default 20).
#' @param min_confidence Minimum confidence threshold (0-1). Results below
#'   this are marked \code{"unknown"}.
#'
#' @return A data frame with columns: \code{description}, \code{category},
#'   \code{confidence}, \code{reasoning}.
#'
#' @details
#' Requires \pkg{httr2} (in Suggests). Returns an error if \pkg{httr2} is not
#' installed.
#'
#' @export
categorize_with_llm <- function(descriptions,
                                 account_tree,
                                 provider = c("anthropic", "openai"),
                                 model = NULL,
                                 token = NULL,
                                 batch_size = 20L,
                                 min_confidence = 0.5) {
  provider <- match.arg(provider)

  if (!requireNamespace("httr2", quietly = TRUE)) {
    stop("Package 'httr2' is required for LLM categorization. ",
         "Install it with: install.packages('httr2')",
         call. = FALSE)
  }

  stopifnot(is.character(descriptions), length(descriptions) > 0)
  stopifnot(is.character(account_tree), length(account_tree) > 0)
  stopifnot(is.numeric(batch_size), batch_size >= 1)
  stopifnot(is.numeric(min_confidence), min_confidence >= 0, min_confidence <= 1)

  # Resolve API key
  key <- .resolve_token(provider, token)

  # Default models
  if (is.null(model)) {
    model <- if (provider == "anthropic") "claude-sonnet-4-20250514" else "gpt-4o-mini"
  }

  # Process in batches
  n <- length(descriptions)
  results <- vector("list", ceiling(n / batch_size))
  batch_idx <- 1L


  for (start in seq(1, n, by = batch_size)) {
    end <- min(start + batch_size - 1L, n)
    batch <- descriptions[start:end]

    prompt <- .build_categorization_prompt(batch, account_tree)

    response <- tryCatch({
      if (provider == "anthropic") {
        .call_anthropic(prompt, model, key)
      } else {
        .call_openai(prompt, model, key)
      }
    }, error = function(e) {
      warning("LLM API call failed for batch ", batch_idx, ": ", conditionMessage(e),
              call. = FALSE)
      NULL
    })

    if (!is.null(response)) {
      df <- .parse_llm_categorizations(response, batch, min_confidence)
      results[[batch_idx]] <- df
    } else {
      # Return unknowns for failed batches
      results[[batch_idx]] <- data.frame(
        description = batch,
        category = rep("unknown", length(batch)),
        confidence = rep(0, length(batch)),
        reasoning = rep("API call failed", length(batch)),
        stringsAsFactors = FALSE
      )
    }
    batch_idx <- batch_idx + 1L
  }

  do.call(rbind, results)
}

#' Categorize a single transaction with an LLM
#'
#' Convenience wrapper around \code{\link{categorize_with_llm}} for a single
#' transaction description.
#'
#' @param description Single transaction description string.
#' @param account_tree Character vector of full account paths.
#' @param provider LLM provider: \code{"anthropic"} or \code{"openai"}.
#' @param token API key. If \code{NULL}, resolved automatically.
#'
#' @return A named list with \code{category}, \code{confidence}, \code{reasoning}.
#'
#' @export
llm_suggest_category <- function(description,
                                  account_tree,
                                  provider = c("anthropic", "openai"),
                                  token = NULL) {
  provider <- match.arg(provider)
  df <- categorize_with_llm(
    descriptions = description,
    account_tree = account_tree,
    provider = provider,
    token = token,
    batch_size = 1L
  )
  list(
    category = df$category[1],
    confidence = df$confidence[1],
    reasoning = df$reasoning[1]
  )
}
