-- Agent authorization rules
-- Maps financial operations to authorization tiers
--
-- Auto:    Safe, read-only operations
-- Review:  Creates artifacts for human review
-- Approve: Moves money or modifies structure

let AuthorizationLevel = ../types/AuthorizationLevel.dhall
let AgentRule = ../types/AgentRule.dhall

let rules
    : List AgentRule
    = [ -- === Auto tier (no approval needed) ===
        { name = "read_balances"
        , description = "Read account balances and metadata"
        , authorization = AuthorizationLevel.Auto
        , max_amount_cents = None Natural
        , rate_limit_per_hour = 120
        , enabled = True
        }
      , { name = "generate_report"
        , description = "Generate financial reports (P&L, balance sheet, trial balance)"
        , authorization = AuthorizationLevel.Auto
        , max_amount_cents = None Natural
        , rate_limit_per_hour = 30
        , enabled = True
        }
      , { name = "categorize_transaction"
        , description = "Assign category to existing transaction"
        , authorization = AuthorizationLevel.Auto
        , max_amount_cents = None Natural
        , rate_limit_per_hour = 60
        , enabled = True
        }
      , { name = "calculate_tax"
        , description = "Calculate tax estimates from categorized expenses"
        , authorization = AuthorizationLevel.Auto
        , max_amount_cents = None Natural
        , rate_limit_per_hour = 10
        , enabled = True
        }
      , { name = "detect_subscriptions"
        , description = "Identify recurring transactions and subscriptions"
        , authorization = AuthorizationLevel.Auto
        , max_amount_cents = None Natural
        , rate_limit_per_hour = 5
        , enabled = True
        }
      , { name = "search_transactions"
        , description = "Full-text search across transaction descriptions and memos"
        , authorization = AuthorizationLevel.Auto
        , max_amount_cents = None Natural
        , rate_limit_per_hour = 60
        , enabled = True
        }

        -- === Review tier (human reviews before finalization) ===
      , { name = "create_invoice"
        , description = "Draft invoice for review before sending"
        , authorization = AuthorizationLevel.Review
        , max_amount_cents = None Natural
        , rate_limit_per_hour = 10
        , enabled = True
        }
      , { name = "flag_subscription"
        , description = "Flag subscription for cancellation review"
        , authorization = AuthorizationLevel.Review
        , max_amount_cents = None Natural
        , rate_limit_per_hour = 20
        , enabled = True
        }
      , { name = "modify_budget"
        , description = "Propose budget modifications for review"
        , authorization = AuthorizationLevel.Review
        , max_amount_cents = None Natural
        , rate_limit_per_hour = 5
        , enabled = True
        }
      , { name = "import_statement"
        , description = "Import OFX/CSV bank statement for review"
        , authorization = AuthorizationLevel.Review
        , max_amount_cents = None Natural
        , rate_limit_per_hour = 5
        , enabled = True
        }

        -- === Approve tier (explicit approval required) ===
      , { name = "post_transaction"
        , description = "Post new transaction to ledger"
        , authorization = AuthorizationLevel.Approve
        , max_amount_cents = Some 100000
        , rate_limit_per_hour = 20
        , enabled = True
        }
      , { name = "pay_bill"
        , description = "Initiate bill payment"
        , authorization = AuthorizationLevel.Approve
        , max_amount_cents = Some 50000
        , rate_limit_per_hour = 5
        , enabled = True
        }
      , { name = "cancel_subscription"
        , description = "Execute subscription cancellation"
        , authorization = AuthorizationLevel.Approve
        , max_amount_cents = None Natural
        , rate_limit_per_hour = 3
        , enabled = True
        }
      , { name = "create_account"
        , description = "Create new account in chart of accounts"
        , authorization = AuthorizationLevel.Approve
        , max_amount_cents = None Natural
        , rate_limit_per_hour = 10
        , enabled = True
        }
      , { name = "transfer_funds"
        , description = "Transfer between accounts"
        , authorization = AuthorizationLevel.Approve
        , max_amount_cents = Some 500000
        , rate_limit_per_hour = 5
        , enabled = True
        }
      ]

in  rules
