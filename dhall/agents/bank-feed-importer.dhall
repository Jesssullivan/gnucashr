-- bank-feed-importer agent configuration
-- Imports OFX/CSV bank statements with FITID dedup

let Types = ../types.dhall

in  Types.Agent::{
    , name = "bank-feed-importer"
    , description =
        "Import bank statements from OFX and CSV files. Automatically deduplicates using financial institution transaction IDs (FITIDs). Supports KeyBank (OFX), Apple Card (OFX/CSV), Venmo (CSV), PayPal (CSV), Stripe (CSV), Capital One (QFX)."
    , tools =
      [ "gnucash_import_ofx"
      , "gnucash_import_csv"
      , "gnucash_check_duplicates"
      , "gnucash_get_slots"
      , "gnucash_set_slot"
      , "gnucash_account_tree"
      , "gnucash_bank_feed_status"
      ]
    , authorization_level = Types.AuthorizationLevel.Review
    , schedule = Some (Types.Schedule.OnDemand)
    , vendor_patterns =
      [ { pattern = "KEYBANK", category = "Assets:Current Assets:Checking", confidence = 0.95 }
      , { pattern = "APPLE CARD", category = "Liabilities:Credit Card", confidence = 0.95 }
      , { pattern = "VENMO", category = "Assets:Current Assets:Venmo", confidence = 0.90 }
      , { pattern = "PAYPAL", category = "Assets:Current Assets:PayPal", confidence = 0.90 }
      , { pattern = "STRIPE", category = "Assets:Current Assets:Stripe", confidence = 0.90 }
      , { pattern = "CAPITAL ONE", category = "Liabilities:Capital One", confidence = 0.95 }
      , { pattern = "CHIME", category = "Assets:Current Assets:Chime", confidence = 0.90 }
      ]
    }
