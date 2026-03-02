# test-identity.R - Tests for identity resolution

test_that("gc_resolve_identity returns structure with system source", {
  result <- gc_resolve_identity()
  expect_type(result, "list")
  expect_true("user_id" %in% names(result))
  expect_true("display_name" %in% names(result))
  expect_true("node_name" %in% names(result))
  expect_true("source" %in% names(result))
  expect_true(nchar(result$user_id) > 0)
  expect_true(nchar(result$node_name) > 0)
})

test_that("gc_resolve_identity uses cli override", {
  result <- gc_resolve_identity("test-user")
  expect_equal(result$user_id, "test-user")
  expect_equal(result$display_name, "test-user")
  expect_equal(result$source, "cli")
})

test_that("gc_resolve_identity with env var", {
  # Set env var and test
  old <- Sys.getenv("GNUCASH_USER", unset = NA)
  on.exit({
    if (is.na(old)) Sys.unsetenv("GNUCASH_USER")
    else Sys.setenv(GNUCASH_USER = old)
  }, add = TRUE)

  Sys.setenv(GNUCASH_USER = "env-test-user")
  result <- gc_resolve_identity()
  expect_equal(result$user_id, "env-test-user")
  expect_equal(result$source, "env")
})

test_that("gc_system_username returns non-empty string", {
  username <- gc_system_username()
  expect_type(username, "character")
  expect_true(nchar(username) > 0)
})

test_that("gc_resolve_identity cli takes precedence over env", {
  old <- Sys.getenv("GNUCASH_USER", unset = NA)
  on.exit({
    if (is.na(old)) Sys.unsetenv("GNUCASH_USER")
    else Sys.setenv(GNUCASH_USER = old)
  }, add = TRUE)

  Sys.setenv(GNUCASH_USER = "env-user")
  result <- gc_resolve_identity("cli-user")
  expect_equal(result$user_id, "cli-user")
  expect_equal(result$source, "cli")
})
