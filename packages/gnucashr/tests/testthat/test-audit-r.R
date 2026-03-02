# test-audit-r.R - Tests for audit trail operations

test_that("gc_audit_open creates audit database", {
  tmp <- tempfile(fileext = ".gnucash")
  on.exit(unlink(c(tmp, paste0(tmp, ".audit.db")), force = TRUE), add = TRUE)
  file.create(tmp)

  audit <- gc_audit_open(tmp)
  expect_true(!is.null(audit))
  gc_audit_close(audit)

  expect_true(file.exists(paste0(tmp, ".audit.db")))
})

test_that("gc_audit_log writes a record", {
  tmp <- tempfile(fileext = ".gnucash")
  on.exit(unlink(c(tmp, paste0(tmp, ".audit.db")), force = TRUE), add = TRUE)
  file.create(tmp)

  audit <- gc_audit_open(tmp)
  on.exit(gc_audit_close(audit), add = TRUE)

  expect_no_error(gc_audit_log(audit, "test_tool", tmp))
})

test_that("gc_audit_query returns logged records", {
  tmp <- tempfile(fileext = ".gnucash")
  on.exit(unlink(c(tmp, paste0(tmp, ".audit.db")), force = TRUE), add = TRUE)
  file.create(tmp)

  audit <- gc_audit_open(tmp)
  on.exit(gc_audit_close(audit), add = TRUE)

  gc_audit_log(audit, "tool_a", tmp, "read", "success")
  gc_audit_log(audit, "tool_b", tmp, "write", "error",
               error_message = "test error")

  result <- gc_audit_query(audit)
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 2)
  expect_true("tool_name" %in% names(result))
  expect_true("classification" %in% names(result))
})

test_that("gc_audit_query filters by tool_name", {
  tmp <- tempfile(fileext = ".gnucash")
  on.exit(unlink(c(tmp, paste0(tmp, ".audit.db")), force = TRUE), add = TRUE)
  file.create(tmp)

  audit <- gc_audit_open(tmp)
  on.exit(gc_audit_close(audit), add = TRUE)

  gc_audit_log(audit, "tool_x", tmp)
  gc_audit_log(audit, "tool_y", tmp)
  gc_audit_log(audit, "tool_x", tmp)

  result <- gc_audit_query(audit, tool_name = "tool_x")
  expect_equal(nrow(result), 2)
  expect_true(all(result$tool_name == "tool_x"))
})

test_that("gc_audit_query limit works", {
  tmp <- tempfile(fileext = ".gnucash")
  on.exit(unlink(c(tmp, paste0(tmp, ".audit.db")), force = TRUE), add = TRUE)
  file.create(tmp)

  audit <- gc_audit_open(tmp)
  on.exit(gc_audit_close(audit), add = TRUE)

  for (i in 1:5) gc_audit_log(audit, "bulk_tool", tmp)

  result <- gc_audit_query(audit, limit = 3L)
  expect_equal(nrow(result), 3)
})

test_that("gc_audit_log records duration_ms", {
  tmp <- tempfile(fileext = ".gnucash")
  on.exit(unlink(c(tmp, paste0(tmp, ".audit.db")), force = TRUE), add = TRUE)
  file.create(tmp)

  audit <- gc_audit_open(tmp)
  on.exit(gc_audit_close(audit), add = TRUE)

  gc_audit_log(audit, "timed_tool", tmp, duration_ms = 42L)

  result <- gc_audit_query(audit)
  expect_equal(result$duration_ms[1], 42L)
})

test_that("gc_audit_export_aperture returns JSONL", {
  tmp <- tempfile(fileext = ".gnucash")
  on.exit(unlink(c(tmp, paste0(tmp, ".audit.db")), force = TRUE), add = TRUE)
  file.create(tmp)

  audit <- gc_audit_open(tmp)
  on.exit(gc_audit_close(audit), add = TRUE)

  gc_audit_log(audit, "export_test", tmp)

  jsonl <- gc_audit_export_aperture(audit)
  expect_type(jsonl, "character")
  expect_true(nchar(jsonl) > 0)
  # Should contain JSON object markers
  expect_true(grepl("\\{", jsonl))
  expect_true(grepl("export_test", jsonl))
})

test_that("gc_audit_query empty result returns empty data frame", {
  tmp <- tempfile(fileext = ".gnucash")
  on.exit(unlink(c(tmp, paste0(tmp, ".audit.db")), force = TRUE), add = TRUE)
  file.create(tmp)

  audit <- gc_audit_open(tmp)
  on.exit(gc_audit_close(audit), add = TRUE)

  result <- gc_audit_query(audit)
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0)
})

test_that("gc_audit_log records classification correctly", {
  tmp <- tempfile(fileext = ".gnucash")
  on.exit(unlink(c(tmp, paste0(tmp, ".audit.db")), force = TRUE), add = TRUE)
  file.create(tmp)

  audit <- gc_audit_open(tmp)
  on.exit(gc_audit_close(audit), add = TRUE)

  gc_audit_log(audit, "read_tool", tmp, "read")
  gc_audit_log(audit, "write_tool", tmp, "write")

  result <- gc_audit_query(audit)
  classes <- result$classification[order(result$tool_name)]
  expect_true("read" %in% classes)
  expect_true("write" %in% classes)
})

test_that("gc_audit_log records entity_guid", {
  tmp <- tempfile(fileext = ".gnucash")
  on.exit(unlink(c(tmp, paste0(tmp, ".audit.db")), force = TRUE), add = TRUE)
  file.create(tmp)

  audit <- gc_audit_open(tmp)
  on.exit(gc_audit_close(audit), add = TRUE)

  gc_audit_log(audit, "entity_tool", tmp,
               entity_guid = "abc123def456")

  result <- gc_audit_query(audit)
  expect_equal(result$entity_guid[1], "abc123def456")
})
