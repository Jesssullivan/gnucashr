-- bill-pay agent configuration
-- Schedules and executes bill payments with approval flow

let Types = ../types.dhall

in  Types.Agent::{
    , name = "bill-pay"
    , description =
        "Identify outstanding bills from payable and liability accounts. Create payment requests that require human approval before execution. Track pending, approved, and executed payments."
    , tools =
      [ "gnucash_get_accounts"
      , "gnucash_account_tree"
      , "gnucash_get_transactions"
      , "gnucash_get_balance"
      , "gnucash_get_splits"
      , "gnucash_post_transaction"
      , "gnucash_audit_log"
      ]
    , authorization_level = Types.AuthorizationLevel.Approve
    , schedule = Some (Types.Schedule.Daily 1)
    }
