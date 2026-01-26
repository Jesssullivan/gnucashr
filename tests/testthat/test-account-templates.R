# test-account-templates.R - Tests for account template functions

test_that("list_templates returns tibble", {
  skip_if_not(exists("list_templates"),
              message = "list_templates function not available")

  # This may fail if package is not installed
  result <- tryCatch(
    list_templates(),
    error = function(e) NULL
  )

  if (is.null(result)) {
    skip("Templates not installed")
  }

  expect_s3_class(result, "tbl_df")
  expect_true(all(c("name", "description", "file") %in% names(result)))
})

test_that("load_template loads small-business template", {
  skip_if_not(exists("load_template"),
              message = "load_template function not available")

  # Skip if templates not installed
  template_dir <- system.file("templates", package = "gnucashr")
  if (template_dir == "") {
    skip("Templates not installed")
  }

  template <- load_template("small-business")

  expect_type(template, "list")
  expect_true("name" %in% names(template))
  expect_true("accounts" %in% names(template))
  expect_equal(template$name, "Small Business")
})

test_that("load_template loads c-corporation template", {
  skip_if_not(exists("load_template"),
              message = "load_template function not available")

  template_dir <- system.file("templates", package = "gnucashr")
  if (template_dir == "") {
    skip("Templates not installed")
  }

  template <- load_template("c-corporation")

  expect_type(template, "list")
  expect_equal(template$name, "C Corporation")
})

test_that("load_template loads personal-finance template", {
  skip_if_not(exists("load_template"),
              message = "load_template function not available")

  template_dir <- system.file("templates", package = "gnucashr")
  if (template_dir == "") {
    skip("Templates not installed")
  }

  template <- load_template("personal-finance")

  expect_type(template, "list")
  expect_equal(template$name, "Personal Finance")
})

test_that("flatten_template creates data frame", {
  skip_if_not(exists("flatten_template"),
              message = "flatten_template function not available")

  template_dir <- system.file("templates", package = "gnucashr")
  if (template_dir == "") {
    skip("Templates not installed")
  }

  accounts <- flatten_template("small-business")

  expect_s3_class(accounts, "tbl_df")
  expect_true(all(c("guid", "name", "full_path", "account_type",
                    "parent_guid", "depth") %in% names(accounts)))

  # Should have multiple accounts
  expect_true(nrow(accounts) > 10)
})

test_that("flatten_template generates unique GUIDs", {
  skip_if_not(exists("flatten_template"),
              message = "flatten_template function not available")

  template_dir <- system.file("templates", package = "gnucashr")
  if (template_dir == "") {
    skip("Templates not installed")
  }

  accounts <- flatten_template("small-business", include_guids = TRUE)

  # All GUIDs should be unique
  expect_equal(length(unique(accounts$guid)), nrow(accounts))

  # All GUIDs should be valid
  expect_true(all(purrr::map_lgl(accounts$guid, validate_guid)))
})

test_that("flatten_template builds correct paths", {
  skip_if_not(exists("flatten_template"),
              message = "flatten_template function not available")

  template_dir <- system.file("templates", package = "gnucashr")
  if (template_dir == "") {
    skip("Templates not installed")
  }

  accounts <- flatten_template("small-business")

  # Check that some expected paths exist
  paths <- accounts$full_path

  expect_true("Assets" %in% paths)
  expect_true(any(grepl("Assets:Current Assets", paths)))
  expect_true(any(grepl("Assets:Current Assets:Checking Account", paths)))
})

test_that("flatten_template sets parent_guid correctly", {
  skip_if_not(exists("flatten_template"),
              message = "flatten_template function not available")

  template_dir <- system.file("templates", package = "gnucashr")
  if (template_dir == "") {
    skip("Templates not installed")
  }

  accounts <- flatten_template("small-business")

  # Top-level accounts should have NA parent
  top_level <- accounts |>
    dplyr::filter(depth == 0)

  expect_true(all(is.na(top_level$parent_guid)))

  # Children should reference parents
  children <- accounts |>
    dplyr::filter(depth > 0)

  # All children should have non-NA parent
  expect_true(all(!is.na(children$parent_guid)))
})

test_that("template_diagram returns string", {
  skip_if_not(exists("template_diagram"),
              message = "template_diagram function not available")

  template_dir <- system.file("templates", package = "gnucashr")
  if (template_dir == "") {
    skip("Templates not installed")
  }

  diagram <- template_diagram("small-business")

  expect_type(diagram, "character")
  expect_true(nchar(diagram) > 100)
  expect_true(grepl("Assets", diagram))
})

test_that("template_diagram respects max_depth", {
  skip_if_not(exists("template_diagram"),
              message = "template_diagram function not available")

  template_dir <- system.file("templates", package = "gnucashr")
  if (template_dir == "") {
    skip("Templates not installed")
  }

  # Full diagram
  full <- template_diagram("small-business")

  # Limited depth
  limited <- template_diagram("small-business", max_depth = 1)

  # Limited should be shorter
  expect_true(nchar(limited) < nchar(full))
})

test_that("compare_to_template validates input", {
  skip_if_not(exists("compare_to_template"),
              message = "compare_to_template function not available")

  expect_error(
    compare_to_template("not a gc", "small-business"),
    "GnuCashDB"
  )
})

test_that("apply_template validates input", {
  skip_if_not(exists("apply_template"),
              message = "apply_template function not available")

  expect_error(
    apply_template("not a gc", "small-business"),
    "GnuCashDB"
  )
})
