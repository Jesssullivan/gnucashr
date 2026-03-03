# Tests for security module (Week 17)

# ========================================================================
# Tool Classification
# ========================================================================

test_that("classify_tool: read tools are auto", {
  expect_equal(classify_tool("gnucash_get_accounts"), "auto")
  expect_equal(classify_tool("gnucash_get_transactions"), "auto")
  expect_equal(classify_tool("gnucash_get_balance"), "auto")
  expect_equal(classify_tool("gnucash_trial_balance"), "auto")
})

test_that("classify_tool: write tools require approval", {
  expect_equal(classify_tool("gnucash_post_transaction"), "approve")
  expect_equal(classify_tool("gnucash_create_account"), "approve")
  expect_equal(classify_tool("gnucash_delete_transaction"), "approve")
  expect_equal(classify_tool("gnucash_void_transaction"), "approve")
})

test_that("classify_tool: unknown tools default to auto", {
  expect_equal(classify_tool("gnucash_some_new_tool"), "auto")
})

# ========================================================================
# Security Check
# ========================================================================

test_that("security_check: enforcement disabled allows everything", {
  policy <- list(
    enforcement_enabled = FALSE,
    agent_name = "test-agent",
    agent_tier = "auto"
  )
  result <- security_check(policy, "gnucash_post_transaction")
  expect_equal(result$decision, "allow")
  expect_true(grepl("enforcement disabled", result$reason))
})

test_that("security_check: read ops allowed with enforcement", {
  policy <- list(
    enforcement_enabled = TRUE,
    agent_name = "test-agent",
    agent_tier = "auto"
  )
  result <- security_check(policy, "gnucash_get_accounts")
  expect_equal(result$decision, "allow")
})

test_that("security_check: write ops require approval", {
  policy <- list(
    enforcement_enabled = TRUE,
    agent_name = "test-agent",
    agent_tier = "auto"
  )
  result <- security_check(policy, "gnucash_post_transaction")
  expect_equal(result$decision, "require_approval")
})

test_that("security_check: approve-tier agent can write", {
  policy <- list(
    enforcement_enabled = TRUE,
    agent_name = "bill-pay",
    agent_tier = "approve"
  )
  result <- security_check(policy, "gnucash_post_transaction")
  expect_equal(result$decision, "allow")
})

# ========================================================================
# Rate Limiter
# ========================================================================

test_that("rate_limiter: allows within limit", {
  limiter <- rate_limiter_create()
  result <- rate_limiter_check(limiter, "test-agent", "post_transaction", 10L)
  expect_true(result$allowed)
  expect_equal(result$remaining, 9L)
})

test_that("rate_limiter: blocks when exceeded", {
  limiter <- rate_limiter_create()
  # Exhaust the limit
  for (i in 1:5) {
    rate_limiter_check(limiter, "test-agent", "op", 5L)
  }
  result <- rate_limiter_check(limiter, "test-agent", "op", 5L)
  expect_false(result$allowed)
  expect_equal(result$remaining, 0L)
})

# ========================================================================
# Anomaly Detection
# ========================================================================

test_that("check_anomaly: normal amount", {
  args <- '{"splits":[{"value_num":5000,"value_denom":100}]}'
  result <- check_anomaly(args, 500000)
  expect_false(result$is_anomalous)
})

test_that("check_anomaly: large amount flagged", {
  args <- '{"splits":[{"value_num":1000000,"value_denom":100}]}'
  result <- check_anomaly(args, 500000)
  expect_true(result$is_anomalous)
  expect_true(result$severity > 0)
})

test_that("check_anomaly: empty args not anomalous", {
  result <- check_anomaly("{}")
  expect_false(result$is_anomalous)
})
