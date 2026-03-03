#' Internal LLM API helpers
#'
#' @name llm-api
#' @keywords internal
NULL

#' Build a categorization prompt
#'
#' @param descriptions Character vector of transaction descriptions.
#' @param account_paths Character vector of account paths from the book.
#' @return A single prompt string.
#' @keywords internal
.build_categorization_prompt <- function(descriptions, account_paths) {
  accounts_str <- paste(account_paths, collapse = "\n")
  desc_list <- paste0(seq_along(descriptions), ". ", descriptions, collapse = "\n")

  paste0(
    "You are a financial categorization assistant. ",
    "Given transaction descriptions, assign each to the most appropriate account ",
    "from the provided chart of accounts.\n\n",
    "## Available Accounts\n",
    accounts_str, "\n\n",
    "## Transactions to Categorize\n",
    desc_list, "\n\n",
    "## Instructions\n",
    "For each transaction, respond with a JSON array where each element has:\n",
    "- \"index\": the transaction number (1-based)\n",
    "- \"category\": the full account path from the list above\n",
    "- \"confidence\": a number between 0 and 1\n",
    "- \"reasoning\": a brief explanation\n\n",
    "If you cannot determine a category, use \"unknown\" with confidence 0.\n",
    "Respond with ONLY the JSON array, no other text."
  )
}

#' Call Anthropic Messages API
#'
#' @param prompt The categorization prompt.
#' @param model Model identifier (e.g., "claude-sonnet-4-20250514").
#' @param token Anthropic API key.
#' @return The text response from the model.
#' @keywords internal
.call_anthropic <- function(prompt, model, token) {
  req <- httr2::request("https://api.anthropic.com/v1/messages") |>
    httr2::req_headers(
      "x-api-key" = token,
      "anthropic-version" = "2023-06-01",
      "content-type" = "application/json"
    ) |>
    httr2::req_body_json(list(
      model = model,
      max_tokens = 4096L,
      messages = list(
        list(role = "user", content = prompt)
      )
    )) |>
    httr2::req_timeout(60)

  resp <- httr2::req_perform(req)
  body <- httr2::resp_body_json(resp)

  # Extract text from content blocks
  content <- body$content
  if (is.list(content) && length(content) > 0) {
    return(content[[1]]$text)
  }

  stop("Unexpected response format from Anthropic API")
}

#' Call OpenAI Chat Completions API
#'
#' @param prompt The categorization prompt.
#' @param model Model identifier (e.g., "gpt-4o-mini").
#' @param token OpenAI API key.
#' @return The text response from the model.
#' @keywords internal
.call_openai <- function(prompt, model, token) {
  req <- httr2::request("https://api.openai.com/v1/chat/completions") |>
    httr2::req_headers(
      "Authorization" = paste("Bearer", token),
      "Content-Type" = "application/json"
    ) |>
    httr2::req_body_json(list(
      model = model,
      messages = list(
        list(role = "user", content = prompt)
      ),
      temperature = 0.1
    )) |>
    httr2::req_timeout(60)

  resp <- httr2::req_perform(req)
  body <- httr2::resp_body_json(resp)

  # Extract text from choices
  choices <- body$choices
  if (is.list(choices) && length(choices) > 0) {
    return(choices[[1]]$message$content)
  }

  stop("Unexpected response format from OpenAI API")
}

#' Parse LLM categorization response
#'
#' @param response_text JSON text from the LLM.
#' @param descriptions Original descriptions (for fallback).
#' @param min_confidence Minimum confidence threshold.
#' @return A data frame with columns: description, category, confidence, reasoning.
#' @keywords internal
.parse_llm_categorizations <- function(response_text, descriptions, min_confidence) {
  # Strip markdown code fences if present
  cleaned <- gsub("^```json\\s*\n?|^```\\s*\n?|\\s*```$", "", response_text)
  cleaned <- trimws(cleaned)

  parsed <- tryCatch(
    jsonlite::fromJSON(cleaned, simplifyDataFrame = FALSE),
    error = function(e) NULL
  )

  n <- length(descriptions)

  if (is.null(parsed) || !is.list(parsed)) {
    return(data.frame(
      description = descriptions,
      category = rep("unknown", n),
      confidence = rep(0, n),
      reasoning = rep("Failed to parse LLM response", n),
      stringsAsFactors = FALSE
    ))
  }

  # Build result data frame
  category <- rep("unknown", n)
  confidence <- rep(0, n)
  reasoning <- rep("", n)

  for (item in parsed) {
    idx <- as.integer(item$index %||% 0L)
    if (idx >= 1 && idx <= n) {
      conf <- as.numeric(item$confidence %||% 0)
      if (conf >= min_confidence) {
        category[idx] <- as.character(item$category %||% "unknown")
      }
      confidence[idx] <- conf
      reasoning[idx] <- as.character(item$reasoning %||% "")
    }
  }

  data.frame(
    description = descriptions,
    category = category,
    confidence = confidence,
    reasoning = reasoning,
    stringsAsFactors = FALSE
  )
}

#' Resolve API key for a provider
#'
#' Checks: explicit parameter -> environment variable -> keyring.
#'
#' @param provider \code{"anthropic"} or \code{"openai"}.
#' @param token Explicit API key or \code{NULL}.
#' @return The resolved API key string.
#' @keywords internal
.resolve_token <- function(provider, token = NULL) {
  if (!is.null(token) && nzchar(token)) {
    return(token)
  }

  # Environment variable

  env_var <- if (provider == "anthropic") "ANTHROPIC_API_KEY" else "OPENAI_API_KEY"
  key <- Sys.getenv(env_var, "")
  if (nzchar(key)) {
    return(key)
  }

  # Keyring fallback
  if (requireNamespace("keyring", quietly = TRUE)) {
    key <- tryCatch(
      keyring::key_get(service = paste0("gnucashr-", provider)),
      error = function(e) ""
    )
    if (nzchar(key)) {
      return(key)
    }
  }

  stop("No API key found for provider '", provider, "'. ",
       "Set ", env_var, " or pass token= explicitly.",
       call. = FALSE)
}
