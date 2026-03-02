-- subscription-manager agent configuration
-- Detects and tracks recurring transactions (subscriptions)

let Types = ../types.dhall

in  Types.Agent::{
    , name = "subscription-manager"
    , description =
        "Detect recurring transactions (subscriptions) by analyzing vendor patterns, amounts, and frequency. Track monthly and annual subscription costs, detect new subscriptions, cancellations, and price changes."
    , tools =
      [ "gnucash_get_accounts"
      , "gnucash_account_tree"
      , "gnucash_get_transactions"
      , "gnucash_get_balance"
      , "gnucash_trial_balance"
      , "gnucash_audit_log"
      ]
    , authorization_level = Types.AuthorizationLevel.Auto
    , schedule = Some (Types.Schedule.Daily 1)
    }
