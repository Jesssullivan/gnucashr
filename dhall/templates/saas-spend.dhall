-- SaaS and compute spend categorization
-- Maps vendor accounts to spend categories for monitoring agents

let AccountMapping = ../types/AccountMapping.dhall
let AccountType = ../types/AccountType.dhall

let mappings
    : List AccountMapping
    = [ -- Compute & Infrastructure
        { gnucash_path = "Expenses:SaaS:Compute"
        , account_type = AccountType.EXPENSE
        , category = "compute"
        , tags = [ "infrastructure", "cloud", "variable-cost" ]
        , auto_categorize = True
        }
      , { gnucash_path = "Expenses:SaaS:Storage"
        , account_type = AccountType.EXPENSE
        , category = "storage"
        , tags = [ "infrastructure", "cloud" ]
        , auto_categorize = True
        }
      , { gnucash_path = "Expenses:SaaS:AI-ML"
        , account_type = AccountType.EXPENSE
        , category = "ai-ml"
        , tags = [ "infrastructure", "ai", "variable-cost" ]
        , auto_categorize = True
        }
      , { gnucash_path = "Expenses:SaaS:Networking"
        , account_type = AccountType.EXPENSE
        , category = "networking"
        , tags = [ "infrastructure", "cloud" ]
        , auto_categorize = True
        }

        -- Developer Tools
      , { gnucash_path = "Expenses:SaaS:Dev-Tools"
        , account_type = AccountType.EXPENSE
        , category = "dev-tools"
        , tags = [ "development", "subscription" ]
        , auto_categorize = True
        }
      , { gnucash_path = "Expenses:SaaS:CI-CD"
        , account_type = AccountType.EXPENSE
        , category = "ci-cd"
        , tags = [ "development", "infrastructure" ]
        , auto_categorize = True
        }
      , { gnucash_path = "Expenses:SaaS:Source-Control"
        , account_type = AccountType.EXPENSE
        , category = "source-control"
        , tags = [ "development", "subscription" ]
        , auto_categorize = True
        }

        -- Productivity & Communication
      , { gnucash_path = "Expenses:SaaS:Productivity"
        , account_type = AccountType.EXPENSE
        , category = "productivity"
        , tags = [ "operations", "subscription" ]
        , auto_categorize = True
        }
      , { gnucash_path = "Expenses:SaaS:Communication"
        , account_type = AccountType.EXPENSE
        , category = "communication"
        , tags = [ "operations", "subscription" ]
        , auto_categorize = True
        }

        -- Security & Compliance
      , { gnucash_path = "Expenses:SaaS:Security"
        , account_type = AccountType.EXPENSE
        , category = "security"
        , tags = [ "security", "compliance", "subscription" ]
        , auto_categorize = True
        }

        -- Observability
      , { gnucash_path = "Expenses:SaaS:Monitoring"
        , account_type = AccountType.EXPENSE
        , category = "monitoring"
        , tags = [ "infrastructure", "observability" ]
        , auto_categorize = True
        }
      ]

in  mappings
