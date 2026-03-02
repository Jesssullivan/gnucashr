-- invoice-generator agent configuration
-- Generates invoice summaries from receivable accounts

let Types = ../types.dhall

in  Types.Agent::{
    , name = "invoice-generator"
    , description =
        "Generate invoice summaries from accounts receivable. Groups outstanding balances by customer, identifies overdue items, and queues invoices for human review before sending."
    , tools =
      [ "gnucash_get_accounts"
      , "gnucash_account_tree"
      , "gnucash_get_transactions"
      , "gnucash_get_transaction"
      , "gnucash_get_balance"
      , "gnucash_get_splits"
      , "gnucash_audit_log"
      ]
    , authorization_level = Types.AuthorizationLevel.Review
    , schedule = Some Types.Schedule.OnDemand
    }
