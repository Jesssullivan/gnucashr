-- reconciler agent configuration
-- Reconciles accounts and matches cross-institution transfers

let Types = ../types.dhall

in  Types.Agent::{
    , name = "reconciler"
    , description =
        "Reconcile bank accounts against statements. Find cross-institution transfer matches between accounts (e.g., KeyBank debit matching Venmo credit). Update split categorization for matched transfers."
    , tools =
      [ "gnucash_reconcile_account"
      , "gnucash_match_imported"
      , "gnucash_get_splits"
      , "gnucash_get_balance"
      , "gnucash_account_tree"
      , "gnucash_bank_feed_status"
      , "gnucash_update_split"
      ]
    , authorization_level = Types.AuthorizationLevel.Approve
    , schedule = Some (Types.Schedule.OnDemand)
    }
