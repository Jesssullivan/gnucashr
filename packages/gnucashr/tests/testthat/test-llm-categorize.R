# Tests for LLM categorization (Week 16)

# ========================================================================
# Prompt Building
# ========================================================================

test_that(".build_categorization_prompt includes accounts and descriptions", {
  accounts <- c("Expenses:SaaS:AI", "Expenses:Office", "Income:Salary")
  descs <- c("ANTHROPIC API", "STAPLES OFFICE")

  prompt <- .build_categorization_prompt(descs, accounts)

  expect_type(prompt, "character")
  expect_length(prompt, 1)
  expect_true(grepl("Expenses:SaaS:AI", prompt))
  expect_true(grepl("Expenses:Office", prompt))
  expect_true(grepl("Income:Salary", prompt))
  expect_true(grepl("ANTHROPIC API", prompt))
  expect_true(grepl("STAPLES OFFICE", prompt))
  expect_true(grepl("JSON array", prompt))
})

test_that(".build_categorization_prompt numbers transactions", {
  descs <- c("TX1", "TX2", "TX3")
  prompt <- .build_categorization_prompt(descs, c("Expenses:Other"))

  expect_true(grepl("1\\. TX1", prompt))
  expect_true(grepl("2\\. TX2", prompt))
  expect_true(grepl("3\\. TX3", prompt))
})

# ========================================================================
# Response Parsing
# ========================================================================

test_that(".parse_llm_categorizations parses valid JSON", {
  response <- '[
    {"index": 1, "category": "Expenses:SaaS:AI", "confidence": 0.95, "reasoning": "AI service"},
    {"index": 2, "category": "Expenses:Office", "confidence": 0.80, "reasoning": "Office supplies"}
  ]'
  descs <- c("ANTHROPIC", "STAPLES")

  result <- .parse_llm_categorizations(response, descs, min_confidence = 0.5)

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 2)
  expect_equal(result$category[1], "Expenses:SaaS:AI")
  expect_equal(result$category[2], "Expenses:Office")
  expect_equal(result$confidence[1], 0.95)
  expect_equal(result$confidence[2], 0.80)
})

test_that(".parse_llm_categorizations handles markdown fences", {
  response <- '```json\n[{"index": 1, "category": "Expenses:Food", "confidence": 0.9, "reasoning": "food"}]\n```'
  result <- .parse_llm_categorizations(response, c("PIZZA"), min_confidence = 0.5)
  expect_equal(result$category[1], "Expenses:Food")
})

test_that(".parse_llm_categorizations applies min_confidence", {
  response <- '[{"index": 1, "category": "Expenses:Other", "confidence": 0.3, "reasoning": "guess"}]'
  result <- .parse_llm_categorizations(response, c("MYSTERY"), min_confidence = 0.5)
  expect_equal(result$category[1], "unknown")
  expect_equal(result$confidence[1], 0.3)
})

test_that(".parse_llm_categorizations handles invalid JSON", {
  result <- .parse_llm_categorizations("not json at all", c("A", "B"), min_confidence = 0.5)
  expect_equal(nrow(result), 2)
  expect_true(all(result$category == "unknown"))
  expect_true(all(result$confidence == 0))
})

test_that(".parse_llm_categorizations handles empty response", {
  result <- .parse_llm_categorizations("[]", c("TX1"), min_confidence = 0.5)
  expect_equal(nrow(result), 1)
  expect_equal(result$category[1], "unknown")
})

test_that(".parse_llm_categorizations handles out-of-range index", {
  response <- '[{"index": 99, "category": "Expenses:Bad", "confidence": 0.9, "reasoning": "bad"}]'
  result <- .parse_llm_categorizations(response, c("TX1"), min_confidence = 0.5)
  expect_equal(result$category[1], "unknown")
})

# ========================================================================
# Token Resolution
# ========================================================================

test_that(".resolve_token: explicit key", {
  key <- .resolve_token("anthropic", "sk-test-123")
  expect_equal(key, "sk-test-123")
})

test_that(".resolve_token: environment variable", {
  env <- setNames("sk-env-456", paste0("ANTHROPIC_", "API_KEY"))
  withr::with_envvar(env, {
    key <- .resolve_token("anthropic")
    expect_equal(key, "sk-env-456")
  })
})

test_that(".resolve_token: openai env var", {
  env <- setNames("sk-openai-789", paste0("OPENAI_", "API_KEY"))
  withr::with_envvar(env, {
    key <- .resolve_token("openai")
    expect_equal(key, "sk-openai-789")
  })
})

test_that(".resolve_token: error when missing", {
  env <- setNames("", paste0("ANTHROPIC_", "API_KEY"))
  withr::with_envvar(env, {
    expect_error(.resolve_token("anthropic"), "No .* key found")
  })
})

# ========================================================================
# Main function - error handling
# ========================================================================

test_that("categorize_with_llm errors without httr2", {
  # This test runs if httr2 happens to not be installed
  # If httr2 IS installed, we still test input validation
  expect_error(
    categorize_with_llm(
      descriptions = character(0),
      account_tree = c("Expenses"),
      provider = "anthropic",
      token = "test"
    )
  )
})

test_that("categorize_with_llm validates inputs", {
  skip_if_not_installed("httr2")
  expect_error(
    categorize_with_llm(
      descriptions = 123,
      account_tree = c("Expenses"),
      provider = "anthropic",
      token = "test"
    )
  )
})

# ========================================================================
# Integration tests (require API key)
# ========================================================================

test_that("categorize_with_llm works with Anthropic API", {
  skip_if(Sys.getenv("ANTHROPIC_API_KEY") == "",
          "ANTHROPIC_API_KEY not set")
  skip_if_not_installed("httr2")

  result <- categorize_with_llm(
    descriptions = c("NETFLIX MONTHLY", "WHOLE FOODS MARKET"),
    account_tree = c(
      "Expenses:Entertainment:Streaming",
      "Expenses:Groceries",
      "Expenses:Other"
    ),
    provider = "anthropic"
  )

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 2)
  expect_true(all(c("description", "category", "confidence", "reasoning") %in% names(result)))
})

test_that("llm_suggest_category works with Anthropic API", {
  skip_if(Sys.getenv("ANTHROPIC_API_KEY") == "",
          "ANTHROPIC_API_KEY not set")
  skip_if_not_installed("httr2")

  result <- llm_suggest_category(
    description = "SPOTIFY PREMIUM",
    account_tree = c(
      "Expenses:Entertainment:Streaming",
      "Expenses:Music",
      "Expenses:Other"
    ),
    provider = "anthropic"
  )

  expect_type(result, "list")
  expect_true("category" %in% names(result))
  expect_true("confidence" %in% names(result))
})
