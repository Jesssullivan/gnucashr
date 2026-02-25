-- report-generator agent configuration
-- Generates financial reports on schedule or on-demand

let Types = ../types.dhall

in  Types.Agent::{
    , name = "report-generator"
    , description =
        "Generate financial reports: trial balance, income statement, balance sheet. Supports monthly/quarterly/annual periods with comparative analysis."
    , tools =
      [ "gnucash_get_accounts"
      , "gnucash_account_tree"
      , "gnucash_get_transactions"
      , "gnucash_get_balance"
      , "gnucash_trial_balance"
      , "gnucash_get_commodities"
      , "gnucash_get_prices"
      , "gnucash_info"
      ]
    , authorization_level = Types.AuthorizationLevel.Auto
    , schedule = Some (Types.Schedule.Daily 1)
    }
