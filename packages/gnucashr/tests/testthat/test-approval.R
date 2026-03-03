# Tests for approval queue module (Week 17)

test_that("approval: open and close", {
  tmp <- tempfile(fileext = ".gnucash")
  file.create(tmp)
  on.exit(unlink(c(tmp, paste0(tmp, ".approvals.db")),
                  recursive = FALSE), add = TRUE)

  adb <- approval_open(tmp)
  expect_false(is.null(adb))
  approval_close(adb)
})

test_that("approval: create request returns GUID", {
  tmp <- tempfile(fileext = ".gnucash")
  file.create(tmp)
  on.exit(unlink(c(tmp, paste0(tmp, ".approvals.db")),
                  recursive = FALSE), add = TRUE)

  adb <- approval_open(tmp)
  on.exit(approval_close(adb), add = TRUE)

  id <- approval_create(
    adb,
    agent_name = "spend-monitor",
    tool_name = "gnucash_post_transaction",
    arguments = '{"amount": 100}',
    requesting_user = "test@example.com",
    reason = "Large transaction"
  )

  expect_type(id, "character")
  expect_equal(nchar(id), 32)  # GUID is 32 hex chars
})

test_that("approval: pending requests returns data frame", {
  tmp <- tempfile(fileext = ".gnucash")
  file.create(tmp)
  on.exit(unlink(c(tmp, paste0(tmp, ".approvals.db")),
                  recursive = FALSE), add = TRUE)

  adb <- approval_open(tmp)
  on.exit(approval_close(adb), add = TRUE)

  approval_create(adb, "agent1", "tool1", "{}", "user1", "reason1")
  approval_create(adb, "agent2", "tool2", "{}", "user2", "reason2")

  pending <- approval_pending(adb)
  expect_s3_class(pending, "data.frame")
  expect_equal(nrow(pending), 2)
  expect_true("id" %in% names(pending))
  expect_true("agent_name" %in% names(pending))
  expect_true("status" %in% names(pending))
})

test_that("approval: approve request", {
  tmp <- tempfile(fileext = ".gnucash")
  file.create(tmp)
  on.exit(unlink(c(tmp, paste0(tmp, ".approvals.db")),
                  recursive = FALSE), add = TRUE)

  adb <- approval_open(tmp)
  on.exit(approval_close(adb), add = TRUE)

  id <- approval_create(adb, "agent1", "tool1", "{}", "user1", "reason1")
  approval_approve(adb, id, "admin@example.com")

  req <- approval_get(adb, id)
  expect_equal(req$status, "approved")
  expect_equal(req$approver, "admin@example.com")
})

test_that("approval: reject request", {
  tmp <- tempfile(fileext = ".gnucash")
  file.create(tmp)
  on.exit(unlink(c(tmp, paste0(tmp, ".approvals.db")),
                  recursive = FALSE), add = TRUE)

  adb <- approval_open(tmp)
  on.exit(approval_close(adb), add = TRUE)

  id <- approval_create(adb, "agent1", "tool1", "{}", "user1", "reason1")
  approval_reject(adb, id, "admin@example.com", "Not authorized")

  req <- approval_get(adb, id)
  expect_equal(req$status, "rejected")
  expect_equal(req$rejection_reason, "Not authorized")
})

test_that("approval: get nonexistent returns NULL", {
  tmp <- tempfile(fileext = ".gnucash")
  file.create(tmp)
  on.exit(unlink(c(tmp, paste0(tmp, ".approvals.db")),
                  recursive = FALSE), add = TRUE)

  adb <- approval_open(tmp)
  on.exit(approval_close(adb), add = TRUE)

  req <- approval_get(adb, "00000000000000000000000000000000")
  expect_null(req)
})

test_that("approval: approve already resolved fails", {
  tmp <- tempfile(fileext = ".gnucash")
  file.create(tmp)
  on.exit(unlink(c(tmp, paste0(tmp, ".approvals.db")),
                  recursive = FALSE), add = TRUE)

  adb <- approval_open(tmp)
  on.exit(approval_close(adb), add = TRUE)

  id <- approval_create(adb, "agent1", "tool1", "{}", "user1", "reason1")
  approval_approve(adb, id, "admin")

  expect_error(approval_approve(adb, id, "admin2"), "No pending request")
})

test_that("approval: full round-trip workflow", {
  tmp <- tempfile(fileext = ".gnucash")
  file.create(tmp)
  on.exit(unlink(c(tmp, paste0(tmp, ".approvals.db")),
                  recursive = FALSE), add = TRUE)

  adb <- approval_open(tmp)
  on.exit(approval_close(adb), add = TRUE)

  # Create request
  id <- approval_create(
    adb,
    agent_name = "bill-pay",
    tool_name = "gnucash_post_transaction",
    arguments = '{"splits":[{"value_num":50000,"value_denom":100}]}',
    requesting_user = "agent@system",
    reason = "Scheduled bill payment"
  )

  # Verify pending
  pending <- approval_pending(adb)
  expect_equal(nrow(pending), 1)

  # Get details
  req <- approval_get(adb, id)
  expect_equal(req$agent_name, "bill-pay")
  expect_equal(req$tool_name, "gnucash_post_transaction")
  expect_equal(req$status, "pending")

  # Approve
  approval_approve(adb, id, "admin@example.com")

  # Verify resolved
  req <- approval_get(adb, id)
  expect_equal(req$status, "approved")
  expect_false(is.null(req$resolved_at))

  # No more pending
  pending <- approval_pending(adb)
  expect_equal(nrow(pending), 0)
})
