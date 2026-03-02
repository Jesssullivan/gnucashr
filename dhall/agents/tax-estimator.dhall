-- tax-estimator agent configuration
-- Estimates tax obligations from income and expense data

let Types = ../types.dhall

in  Types.Agent::{
    , name = "tax-estimator"
    , description =
        "Estimate federal tax obligations based on income and expense categories. Applies progressive tax brackets, identifies deductible expenses, and projects quarterly estimated payments."
    , tools =
      [ "gnucash_get_accounts"
      , "gnucash_account_tree"
      , "gnucash_get_balance"
      , "gnucash_trial_balance"
      , "gnucash_get_transactions"
      , "gnucash_audit_log"
      ]
    , authorization_level = Types.AuthorizationLevel.Auto
    , schedule = Some (Types.Schedule.Daily 1)
    }
