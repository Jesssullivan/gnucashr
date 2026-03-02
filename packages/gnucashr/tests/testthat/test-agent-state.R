# test-agent-state.R - Tests for agent state database

test_that("gc_agent_state_open creates state database", {
  tmp <- tempfile(fileext = ".gnucash")
  on.exit({
    unlink(tmp)
    unlink(paste0(tmp, ".agent.test-agent.db"))
  }, add = TRUE)
  file.create(tmp)

  state <- gc_agent_state_open(tmp, "test-agent")
  expect_true(!is.null(state))
  gc_agent_state_close(state)

  expect_true(file.exists(paste0(tmp, ".agent.test-agent.db")))
})

test_that("gc_agent_state set/get round-trip", {
  tmp <- tempfile(fileext = ".gnucash")
  on.exit(unlink(c(tmp, paste0(tmp, ".agent.kv-test.db")), force = TRUE),
          add = TRUE)
  file.create(tmp)

  state <- gc_agent_state_open(tmp, "kv-test")
  on.exit(gc_agent_state_close(state), add = TRUE)

  gc_agent_state_set(state, "last_run", "2024-01-15")
  result <- gc_agent_state_get(state, "last_run")
  expect_equal(result, "2024-01-15")
})

test_that("gc_agent_state_get returns NULL for missing key", {
  tmp <- tempfile(fileext = ".gnucash")
  on.exit(unlink(c(tmp, paste0(tmp, ".agent.null-test.db")), force = TRUE),
          add = TRUE)
  file.create(tmp)

  state <- gc_agent_state_open(tmp, "null-test")
  on.exit(gc_agent_state_close(state), add = TRUE)

  result <- gc_agent_state_get(state, "nonexistent_key")
  expect_null(result)
})

test_that("gc_agent_state_set overwrites existing value", {
  tmp <- tempfile(fileext = ".gnucash")
  on.exit(unlink(c(tmp, paste0(tmp, ".agent.overwrite.db")), force = TRUE),
          add = TRUE)
  file.create(tmp)

  state <- gc_agent_state_open(tmp, "overwrite")
  on.exit(gc_agent_state_close(state), add = TRUE)

  gc_agent_state_set(state, "counter", "1")
  gc_agent_state_set(state, "counter", "2")
  result <- gc_agent_state_get(state, "counter")
  expect_equal(result, "2")
})

test_that("gc_agent_state_enqueue_review returns id", {
  tmp <- tempfile(fileext = ".gnucash")
  on.exit(unlink(c(tmp, paste0(tmp, ".agent.review.db")), force = TRUE),
          add = TRUE)
  file.create(tmp)

  state <- gc_agent_state_open(tmp, "review")
  on.exit(gc_agent_state_close(state), add = TRUE)

  id <- gc_agent_state_enqueue_review(state,
    "tx_guid_001", "Expenses:Food", 0.85, "Frequent merchant match")
  expect_type(id, "integer")
  expect_true(id > 0)
})

test_that("gc_agent_state_pending_reviews returns data frame", {
  tmp <- tempfile(fileext = ".gnucash")
  on.exit(unlink(c(tmp, paste0(tmp, ".agent.pending.db")), force = TRUE),
          add = TRUE)
  file.create(tmp)

  state <- gc_agent_state_open(tmp, "pending")
  on.exit(gc_agent_state_close(state), add = TRUE)

  gc_agent_state_enqueue_review(state,
    "tx_001", "Expenses:Food", 0.9, "Pattern match")
  gc_agent_state_enqueue_review(state,
    "tx_002", "Expenses:Transport", 0.7, "Keyword match")

  result <- gc_agent_state_pending_reviews(state)
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 2)
  expect_true(all(c("id", "transaction_guid", "suggested_category",
                     "confidence", "reason", "status") %in% names(result)))
  expect_true(all(result$status == "pending"))
})

test_that("gc_agent_state_update_review changes status", {
  tmp <- tempfile(fileext = ".gnucash")
  on.exit(unlink(c(tmp, paste0(tmp, ".agent.update.db")), force = TRUE),
          add = TRUE)
  file.create(tmp)

  state <- gc_agent_state_open(tmp, "update")
  on.exit(gc_agent_state_close(state), add = TRUE)

  id <- gc_agent_state_enqueue_review(state,
    "tx_003", "Expenses:Utilities", 0.95, "Bill payment")

  gc_agent_state_update_review(state, id, "approved")

  # Should no longer be in pending
  pending <- gc_agent_state_pending_reviews(state)
  expect_equal(nrow(pending), 0)
})

test_that("gc_agent_state_pending_reviews respects limit", {
  tmp <- tempfile(fileext = ".gnucash")
  on.exit(unlink(c(tmp, paste0(tmp, ".agent.limit.db")), force = TRUE),
          add = TRUE)
  file.create(tmp)

  state <- gc_agent_state_open(tmp, "limit")
  on.exit(gc_agent_state_close(state), add = TRUE)

  for (i in 1:5) {
    gc_agent_state_enqueue_review(state,
      paste0("tx_", i), "Expenses:Misc", 0.5, paste("Item", i))
  }

  result <- gc_agent_state_pending_reviews(state, limit = 3L)
  expect_equal(nrow(result), 3)
})

test_that("multiple agents have independent state", {
  tmp <- tempfile(fileext = ".gnucash")
  on.exit(unlink(c(tmp,
    paste0(tmp, ".agent.agent-a.db"),
    paste0(tmp, ".agent.agent-b.db")), force = TRUE), add = TRUE)
  file.create(tmp)

  state_a <- gc_agent_state_open(tmp, "agent-a")
  state_b <- gc_agent_state_open(tmp, "agent-b")
  on.exit({
    gc_agent_state_close(state_a)
    gc_agent_state_close(state_b)
  }, add = TRUE)

  gc_agent_state_set(state_a, "shared_key", "value_a")
  gc_agent_state_set(state_b, "shared_key", "value_b")

  expect_equal(gc_agent_state_get(state_a, "shared_key"), "value_a")
  expect_equal(gc_agent_state_get(state_b, "shared_key"), "value_b")
})

test_that("gc_agent_state_enqueue_review stores confidence correctly", {
  tmp <- tempfile(fileext = ".gnucash")
  on.exit(unlink(c(tmp, paste0(tmp, ".agent.conf.db")), force = TRUE),
          add = TRUE)
  file.create(tmp)

  state <- gc_agent_state_open(tmp, "conf")
  on.exit(gc_agent_state_close(state), add = TRUE)

  gc_agent_state_enqueue_review(state,
    "tx_conf", "Expenses:Test", 0.42, "Low confidence")

  result <- gc_agent_state_pending_reviews(state)
  expect_equal(result$confidence[1], 0.42, tolerance = 0.001)
})
