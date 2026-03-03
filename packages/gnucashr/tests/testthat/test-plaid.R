# Test Plaid Integration Functions

# =============================================================================
# Mock Data Helpers
# =============================================================================

#' Build a mock Plaid transaction object
mock_plaid_transaction <- function(
    transaction_id = "txn_abc123",
    date = "2026-01-15",
    name = "STARBUCKS",
    merchant_name = "Starbucks",
    amount = 5.50,
    iso_currency_code = "USD",
    payment_channel = "in store",
    personal_finance_category = list(primary = "FOOD_AND_DRINK", detailed = "COFFEE")
) {
  list(
    transaction_id = transaction_id,
    date = date,
    name = name,
    merchant_name = merchant_name,
    amount = amount,
    iso_currency_code = iso_currency_code,
    payment_channel = payment_channel,
    personal_finance_category = personal_finance_category
  )
}

#' Build a mock Plaid account object
mock_plaid_account <- function(
    account_id = "acct_001",
    name = "Checking",
    official_name = "Premier Checking",
    type = "depository",
    subtype = "checking",
    mask = "1234",
    balance_current = 1500.00,
    balance_available = 1450.00
) {
  list(
    account_id = account_id,
    name = name,
    official_name = official_name,
    type = type,
    subtype = subtype,
    mask = mask,
    balances = list(current = balance_current, available = balance_available)
  )
}


# =============================================================================
# plaid_create_client Tests
# =============================================================================

test_that("plaid_create_client: creates client with explicit credentials", {
  skip_if_not_installed("httr2")

  client <- plaid_create_client("test_id", "test_secret", "sandbox")

  expect_s3_class(client, "plaid_client")
  expect_equal(client$client_id, "test_id")
  expect_equal(client$secret, "test_secret")
  expect_equal(client$environment, "sandbox")
  expect_equal(client$base_url, "https://sandbox.plaid.com")
})

test_that("plaid_create_client: environment URLs are correct", {
  skip_if_not_installed("httr2")

  sandbox <- plaid_create_client("id", "s", "sandbox")
  expect_equal(sandbox$base_url, "https://sandbox.plaid.com")

  dev <- plaid_create_client("id", "s", "development")
  expect_equal(dev$base_url, "https://development.plaid.com")

  prod <- plaid_create_client("id", "s", "production")
  expect_equal(prod$base_url, "https://production.plaid.com")
})

test_that("plaid_create_client: environment variable fallback", {
  skip_if_not_installed("httr2")

  env <- setNames(
    c("env_client_id", "env_secret_val"),
    c("PLAID_CLIENT_ID", "PLAID_SECRET")
  )
  withr::with_envvar(env, {
    client <- plaid_create_client()
    expect_equal(client$client_id, "env_client_id")
    expect_equal(client$secret, "env_secret_val")
  })
})

test_that("plaid_create_client: errors without credentials", {
  skip_if_not_installed("httr2")

  withr::with_envvar(c("PLAID_CLIENT_ID" = "", "PLAID_SECRET" = ""), {
    expect_error(plaid_create_client(), "Plaid credentials required")
  })
})

test_that("plaid_create_client: invalid environment errors", {
  skip_if_not_installed("httr2")

  expect_error(
    plaid_create_client("id", "s", "invalid"),
    "'arg' should be one of"
  )
})

test_that("plaid_create_client: print method works", {
  skip_if_not_installed("httr2")

  client <- plaid_create_client("id", "s", "sandbox")
  output <- capture.output(print(client))
  expect_match(output[1], "plaid_client")
  expect_match(output[1], "sandbox")
})


# =============================================================================
# .plaid_fitid Tests
# =============================================================================

test_that(".plaid_fitid: generates prefixed FITID", {
  expect_equal(.plaid_fitid("abc123"), "plaid:abc123")
  expect_equal(.plaid_fitid("txn_xyz"), "plaid:txn_xyz")
})

test_that(".plaid_fitid: handles NULL/NA/empty", {
  expect_equal(.plaid_fitid(NULL), NA_character_)
  expect_equal(.plaid_fitid(NA), NA_character_)
  expect_equal(.plaid_fitid(""), NA_character_)
})


# =============================================================================
# .plaid_category_hint Tests
# =============================================================================

test_that(".plaid_category_hint: maps known categories", {
  expect_match(.plaid_category_hint(list(primary = "FOOD_AND_DRINK")),
               "Expenses:Dining")
  expect_match(.plaid_category_hint(list(primary = "TRANSPORTATION")),
               "Expenses:Transportation")
  expect_match(.plaid_category_hint(list(primary = "INCOME")),
               "Income")
  expect_match(.plaid_category_hint(list(primary = "BANK_FEES")),
               "Expenses:Bank Charges")
})

test_that(".plaid_category_hint: includes detailed category", {
  result <- .plaid_category_hint(list(
    primary = "FOOD_AND_DRINK",
    detailed = "COFFEE"
  ))
  expect_match(result, "Expenses:Dining")
  expect_match(result, "\\[coffee\\]")
})

test_that(".plaid_category_hint: returns NULL for unknown", {
  expect_null(.plaid_category_hint(list(primary = "UNKNOWN_CATEGORY")))
  expect_null(.plaid_category_hint(NULL))
  expect_null(.plaid_category_hint(list()))
})


# =============================================================================
# .plaid_to_ofx_transactions Tests
# =============================================================================

test_that(".plaid_to_ofx_transactions: maps basic fields", {
  txns <- list(
    mock_plaid_transaction(
      transaction_id = "txn_001",
      date = "2026-01-15",
      merchant_name = "Starbucks",
      amount = 5.50,
      iso_currency_code = "USD"
    )
  )

  result <- .plaid_to_ofx_transactions(txns)

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 1)
  expect_equal(result$date, as.Date("2026-01-15"))
  expect_equal(result$description, "Starbucks")
  # Plaid positive = debit; OFX positive = credit; so negated

  expect_equal(result$amount, -5.50)
  expect_equal(result$currency, "USD")
  expect_equal(result$external_id, "plaid:txn_001")
})

test_that(".plaid_to_ofx_transactions: negates Plaid amounts correctly", {
  txns <- list(
    mock_plaid_transaction(amount = 42.00),   # Plaid debit (money out)
    mock_plaid_transaction(
      transaction_id = "txn_002",
      amount = -100.00  # Plaid credit (money in)
    )
  )

  result <- .plaid_to_ofx_transactions(txns)

  expect_equal(result$amount[1], -42.00)   # OFX debit (negative)
  expect_equal(result$amount[2], 100.00)   # OFX credit (positive)
})

test_that(".plaid_to_ofx_transactions: handles empty list", {
  result <- .plaid_to_ofx_transactions(list())

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 0)
  expect_true("date" %in% names(result))
  expect_true("external_id" %in% names(result))
})

test_that(".plaid_to_ofx_transactions: prefers merchant_name over name", {
  txns <- list(
    mock_plaid_transaction(name = "SQ *STARBUCKS", merchant_name = "Starbucks")
  )

  result <- .plaid_to_ofx_transactions(txns)
  expect_equal(result$description, "Starbucks")
})

test_that(".plaid_to_ofx_transactions: falls back to name if no merchant", {
  txns <- list(
    mock_plaid_transaction(name = "TRANSFER TO SAVINGS", merchant_name = NULL)
  )

  result <- .plaid_to_ofx_transactions(txns)
  expect_equal(result$description, "TRANSFER TO SAVINGS")
})

test_that(".plaid_to_ofx_transactions: maps payment_channel to type", {
  txns <- list(
    mock_plaid_transaction(payment_channel = "in store"),
    mock_plaid_transaction(transaction_id = "t2", payment_channel = "online"),
    mock_plaid_transaction(transaction_id = "t3", payment_channel = "other")
  )

  result <- .plaid_to_ofx_transactions(txns)
  expect_equal(result$transaction_type, c("POS", "DIRECTDEBIT", "OTHER"))
})


# =============================================================================
# .plaid_to_ofx_content Tests
# =============================================================================

test_that(".plaid_to_ofx_content: generates valid OFX SGML", {
  txns <- list(
    mock_plaid_transaction(
      transaction_id = "txn_ofx_001",
      date = "2026-02-20",
      merchant_name = "Amazon",
      amount = 29.99
    )
  )

  tibble_data <- .plaid_to_ofx_transactions(txns)
  ofx_content <- .plaid_to_ofx_content(tibble_data)

  expect_type(ofx_content, "character")
  expect_match(ofx_content, "OFXHEADER:100")
  expect_match(ofx_content, "<STMTTRN>")
  expect_match(ofx_content, "<FITID>plaid:txn_ofx_001")
  expect_match(ofx_content, "<CURDEF>USD")
  expect_match(ofx_content, "<NAME>Amazon")
  expect_match(ofx_content, "<DTPOSTED>20260220")
})

test_that(".plaid_to_ofx_content: escapes special characters", {
  txns <- list(
    mock_plaid_transaction(merchant_name = "AT&T Wireless <Corp>")
  )

  tibble_data <- .plaid_to_ofx_transactions(txns)
  ofx_content <- .plaid_to_ofx_content(tibble_data)

  expect_match(ofx_content, "AT&amp;T Wireless &lt;Corp&gt;")
})

test_that(".plaid_to_ofx_content: empty tibble returns empty string", {
  result <- .plaid_to_ofx_content(tibble::tibble(
    date = as.Date(character()),
    description = character(),
    amount = numeric(),
    currency = character(),
    external_id = character(),
    memo = character(),
    transaction_type = character()
  ))

  expect_equal(result, "")
})


# =============================================================================
# .plaid_accounts_summary Tests
# =============================================================================

test_that(".plaid_accounts_summary: maps account fields", {
  accounts <- list(
    mock_plaid_account(
      account_id = "acct_001",
      official_name = "Premier Checking",
      type = "depository",
      subtype = "checking",
      mask = "1234",
      balance_current = 1500.00
    )
  )

  result <- .plaid_accounts_summary(accounts)

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 1)
  expect_equal(result$account_id, "acct_001")
  expect_equal(result$name, "Premier Checking")
  expect_equal(result$type, "depository")
  expect_equal(result$mask, "1234")
  expect_equal(result$balance_current, 1500.00)
})

test_that(".plaid_accounts_summary: handles empty list", {
  result <- .plaid_accounts_summary(list())
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 0)
})


# =============================================================================
# Live Plaid API Tests (skip without credentials)
# =============================================================================

test_that("plaid_sync_transactions: live sandbox test", {
  skip_if_not_installed("httr2")
  skip_if(
    Sys.getenv("PLAID_SECRET") == "",
    "PLAID_SECRET not set; skipping live Plaid tests"
  )
  skip_if(
    Sys.getenv("PLAID_ACCESS_TOKEN") == "",
    "PLAID_ACCESS_TOKEN not set; skipping live sync test"
  )

  client <- plaid_create_client(environment = "sandbox")
  result <- plaid_sync_transactions(
    client,
    access_tok = Sys.getenv("PLAID_ACCESS_TOKEN")
  )

  expect_type(result, "list")
  expect_true("added" %in% names(result))
  expect_true("next_cursor" %in% names(result))
  expect_false(result$has_more)
})

test_that("plaid_get_accounts: live sandbox test", {
  skip_if_not_installed("httr2")
  skip_if(
    Sys.getenv("PLAID_SECRET") == "",
    "PLAID_SECRET not set; skipping live Plaid tests"
  )
  skip_if(
    Sys.getenv("PLAID_ACCESS_TOKEN") == "",
    "PLAID_ACCESS_TOKEN not set; skipping live account test"
  )

  client <- plaid_create_client(environment = "sandbox")
  accounts <- plaid_get_accounts(
    client,
    access_tok = Sys.getenv("PLAID_ACCESS_TOKEN")
  )

  expect_s3_class(accounts, "tbl_df")
  expect_true(nrow(accounts) > 0)
  expect_true("account_id" %in% names(accounts))
  expect_true("balance_current" %in% names(accounts))
})
