-- transaction-categorizer agent configuration
-- Auto-categorizes transactions using LLM + pattern matching

let Types = ../types.dhall

let SpendMonitor = ./spend-monitor.dhall

in  Types.Agent::{
    , name = "transaction-categorizer"
    , description =
        "Automatically categorize transactions using pattern matching and LLM reasoning. High-confidence matches (>0.9) auto-apply, low-confidence matches queue for human review."
    , tools =
      [ "gnucash_get_accounts"
      , "gnucash_account_tree"
      , "gnucash_get_transactions"
      , "gnucash_get_transaction"
      , "gnucash_post_transaction"     -- For updating transaction categories
      , "gnucash_audit_log"
      ]
    , authorization_level = Types.AuthorizationLevel.Review  -- Human review for low confidence
    , vendor_patterns = SpendMonitor.vendor_patterns          -- Reuse spend-monitor patterns
    , schedule = Some Types.Schedule.OnDemand                 -- Triggered on new transactions
    }
