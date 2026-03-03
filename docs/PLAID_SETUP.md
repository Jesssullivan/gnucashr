# Plaid Integration Setup

## Overview

gnucashr uses the Plaid API to import bank transactions from real financial institutions. Plaid's `/transactions/sync` endpoint provides cursor-based incremental updates, and transactions are mapped to the existing OFX import pipeline for dedup via FITID.

## Account Setup

### 1. Register for Plaid

- Go to https://dashboard.plaid.com/signup
- Sign up for a free developer account
- Free tier: 200 API calls per month (sufficient for testing)

### 2. Get Sandbox Credentials

After registration:

1. Navigate to **Developers > Keys** in the Plaid dashboard
2. Copy your **client_id** and **Sandbox secret**
3. The sandbox environment uses test data -- no real bank connections needed

### 3. Store Credentials

#### Option A: Environment variables

```bash
export PLAID_CLIENT_ID="your_client_id"
export PLAID_SECRET="your_sandbox_secret"
```

Add to your `.Renviron` (lives in project root, gitignored):

```
PLAID_CLIENT_ID=your_client_id
PLAID_SECRET=your_sandbox_secret
```

#### Option B: KeePassXC (recommended for production)

```r
# If using RemoteJuggler MCP:
# juggler_keys_store("RemoteJuggler/Plaid/CLIENT_ID", "your_client_id")
# juggler_keys_store("RemoteJuggler/Plaid/SECRET", "your_secret")
```

## Quick Start

### Connect a Bank (Sandbox)

```r
library(gnucashr)

# Create client (reads from env vars)
client <- plaid_create_client(environment = "sandbox")

# Run the Link server to connect a sandbox bank
result <- plaid_link_server(client, user_id = "my-user")
# Opens browser -> select bank -> authenticate -> returns access_token

# Store the access token securely!
cat("Access token:", result$access_token, "\n")
cat("Item ID:", result$item_id, "\n")
```

### Sync Transactions

```r
# Sync all available transactions
sync <- plaid_sync_transactions(client, access_tok = result$access_token)
cat("New transactions:", length(sync$added), "\n")

# View accounts
accounts <- plaid_get_accounts(client, access_tok = result$access_token)
print(accounts)
```

### Import into GnuCash

```r
book <- gc_open("finances.gnucash", read_only = FALSE)
state <- agent_state_open("finances.gnucash", "plaid-importer")

import_result <- plaid_import_transactions(
  book, client,
  access_tok = result$access_token,
  target_account_guid = "your_checking_account_guid",
  offset_account_guid = "your_imbalance_account_guid",
  state_ptr = state
)

cat("Imported:", import_result$imported, "transactions\n")

# Cursor is automatically saved -- next sync only gets new transactions
agent_state_close(state)
gc_close(book)
```

## Sandbox Test Banks

In sandbox mode, Plaid provides test institutions:

| Institution | Username | Password |
|-------------|----------|----------|
| First Platypus Bank | `user_good` | `pass_good` |
| Tartan Bank | `user_good` | `pass_good` |
| Tattersall Federal Credit Union | `user_good` | `pass_good` |

These return realistic synthetic transaction data.

## Environments

| Environment | URL | Use Case |
|-------------|-----|----------|
| `sandbox` | sandbox.plaid.com | Development + testing (free) |
| `development` | development.plaid.com | Testing with real banks (100 items) |
| `production` | production.plaid.com | Live deployment |

## API Limits

- **Sandbox**: Unlimited API calls, 100 test items
- **Development**: 100 live items, free
- **Production**: Pay-per-item pricing

The `/transactions/sync` endpoint is cursor-based and incremental:
- First call: returns all historical transactions (may paginate)
- Subsequent calls with cursor: returns only new/modified/removed since last sync
- gnucashr caches the cursor in the agent state DB automatically

## Troubleshooting

### "Plaid credentials required" error

Ensure environment variables are set:
```r
Sys.getenv("PLAID_CLIENT_ID")  # Should not be ""
Sys.getenv("PLAID_SECRET")     # Should not be ""
```

### Link server doesn't open browser

Pass `browse = FALSE` and navigate manually:
```r
result <- plaid_link_server(client, browse = FALSE)
# Navigate to http://127.0.0.1:1410 manually
```

### Duplicate transactions after re-import

Transactions are deduped by FITID (Plaid transaction_id prefixed with `plaid:`). If you see duplicates, check:
```r
# Verify FITID is stored
find_split_by_fitid(book, account_guid, "plaid:txn_abc123")
```
