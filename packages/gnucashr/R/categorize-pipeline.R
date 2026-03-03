#' Categorization Pipeline
#'
#' Two-stage categorization: C++ vendor pattern matching first,
#' then LLM fallback for unmatched transactions.
#'
#' @name categorize-pipeline
NULL

#' Run the full categorization pipeline
#'
#' Attempts C++ pattern-based categorization first, then falls back to
#' LLM-powered categorization for unmatched transactions.
#'
#' @param book_ptr External pointer to an open GnuCash book.
#' @param vendor_patterns Named list of vendor patterns where names are
#'   regex patterns and values are target account paths.
#' @param llm_provider LLM provider for fallback: \code{"anthropic"} or
#'   \code{"openai"}. Set to \code{NULL} to skip LLM fallback.
#' @param llm_token Access token for the LLM provider. If \code{NULL}, resolved
#'   from environment.
#' @param auto_apply_threshold Confidence threshold for automatic application
#'   (default 0.85). Below this, results are queued for review.
#' @param cache_results If \code{TRUE} (default), cache LLM results in the
#'   agent state DB for the \code{"transaction-categorizer"} agent.
#'
#' @return A list with:
#'   \describe{
#'     \item{total}{Total transactions examined}
#'     \item{pattern_matched}{Count matched by C++ vendor patterns}
#'     \item{llm_matched}{Count matched by LLM}
#'     \item{unmatched}{Count still uncategorized}
#'     \item{results}{Data frame with all categorization results}
#'   }
#'
#' @export
run_categorization_pipeline <- function(book_ptr,
                                         vendor_patterns = list(),
                                         llm_provider = NULL,
                                         llm_token = NULL,
                                         auto_apply_threshold = 0.85,
                                         cache_results = TRUE) {
  # Get transactions
  txns <- gc_get_transactions(book_ptr)

  if (nrow(txns) == 0) {
    return(list(
      total = 0L,
      pattern_matched = 0L,
      llm_matched = 0L,
      unmatched = 0L,
      results = data.frame(
        guid = character(0),
        description = character(0),
        category = character(0),
        confidence = numeric(0),
        source = character(0),
        stringsAsFactors = FALSE
      )
    ))
  }

  # Get account tree for LLM context
  accounts <- gc_account_tree(book_ptr)
  account_paths <- accounts$full_path[nzchar(accounts$full_path)]

  descriptions <- txns$description
  guids <- txns$guid
  n <- length(descriptions)

  # Initialize results
  category <- rep(NA_character_, n)
  confidence <- rep(0, n)
  source <- rep("none", n)

  # Stage 1: Pattern matching
  if (length(vendor_patterns) > 0) {
    for (i in seq_len(n)) {
      desc_lower <- tolower(descriptions[i])
      for (pat_name in names(vendor_patterns)) {
        if (grepl(tolower(pat_name), desc_lower, fixed = TRUE)) {
          category[i] <- vendor_patterns[[pat_name]]
          confidence[i] <- 0.90
          source[i] <- "pattern"
          break
        }
      }
    }
  }

  # Stage 2: LLM fallback for unmatched
  unmatched_idx <- which(is.na(category))
  llm_count <- 0L

  if (length(unmatched_idx) > 0 && !is.null(llm_provider)) {
    if (!requireNamespace("httr2", quietly = TRUE)) {
      warning("httr2 not installed, skipping LLM categorization", call. = FALSE)
    } else {
      llm_results <- tryCatch(
        categorize_with_llm(
          descriptions = descriptions[unmatched_idx],
          account_tree = account_paths,
          provider = llm_provider,
          token = llm_token,
          min_confidence = 0.3
        ),
        error = function(e) {
          warning("LLM categorization failed: ", conditionMessage(e), call. = FALSE)
          NULL
        }
      )

      if (!is.null(llm_results)) {
        for (j in seq_along(unmatched_idx)) {
          idx <- unmatched_idx[j]
          if (llm_results$category[j] != "unknown") {
            category[idx] <- llm_results$category[j]
            confidence[idx] <- llm_results$confidence[j]
            source[idx] <- "llm"
            llm_count <- llm_count + 1L
          }
        }
      }
    }
  }

  # Cache results in agent state DB
  if (cache_results && llm_count > 0) {
    tryCatch({
      book_path <- gc_info(book_ptr)$path
      if (!is.null(book_path) && nzchar(book_path)) {
        state <- agent_state_open(book_path, "transaction-categorizer")
        on.exit(agent_state_close(state), add = TRUE)
        for (i in which(source == "llm")) {
          cache_key <- paste0("llm_cache:", descriptions[i])
          cache_val <- jsonlite::toJSON(list(
            category = category[i],
            confidence = confidence[i]
          ), auto_unbox = TRUE)
          agent_state_set(state, cache_key, as.character(cache_val))
        }
      }
    }, error = function(e) {
      # Caching is best-effort
    })
  }

  # Replace remaining NAs
  category[is.na(category)] <- "unknown"

  results <- data.frame(
    guid = guids,
    description = descriptions,
    category = category,
    confidence = confidence,
    source = source,
    stringsAsFactors = FALSE
  )

  list(
    total = n,
    pattern_matched = sum(source == "pattern"),
    llm_matched = llm_count,
    unmatched = sum(category == "unknown"),
    results = results
  )
}
