-- spend-monitor agent configuration
-- Monitors and categorizes SaaS/compute spending, detects anomalies

let Types = ../types.dhall

in  Types.Agent::{
    , name = "spend-monitor"
    , description =
        "Monitor and categorize spending on SaaS, cloud, and compute services. Detect spending anomalies and generate reports."
    , tools =
      [ "gnucash_get_accounts"
      , "gnucash_account_tree"
      , "gnucash_get_transactions"
      , "gnucash_get_balance"
      , "gnucash_trial_balance"
      , "gnucash_audit_log"
      ]
    , authorization_level = Types.AuthorizationLevel.Auto
    , vendor_patterns =
      [ { pattern = "MODAL LABS"
        , category = "Expenses:SaaS:Compute"
        , confidence = 0.99
        }
      , { pattern = "VERCEL"
        , category = "Expenses:SaaS:Hosting"
        , confidence = 0.95
        }
      , { pattern = "GITHUB"
        , category = "Expenses:SaaS:DevTools"
        , confidence = 0.98
        }
      , { pattern = "ANTHROPIC"
        , category = "Expenses:SaaS:AI"
        , confidence = 0.99
        }
      , { pattern = "AWS"
        , category = "Expenses:Cloud:AWS"
        , confidence = 0.90
        }
      , { pattern = "GOOGLE CLOUD"
        , category = "Expenses:Cloud:GCP"
        , confidence = 0.92
        }
      , { pattern = "MICROSOFT AZURE"
        , category = "Expenses:Cloud:Azure"
        , confidence = 0.93
        }
      , { pattern = "DIGITALOCEAN"
        , category = "Expenses:Cloud:DigitalOcean"
        , confidence = 0.95
        }
      , { pattern = "HEROKU"
        , category = "Expenses:SaaS:Hosting"
        , confidence = 0.94
        }
      , { pattern = "NETLIFY"
        , category = "Expenses:SaaS:Hosting"
        , confidence = 0.96
        }
      ]
    , schedule = Some (Types.Schedule.Hourly 1)
    }
