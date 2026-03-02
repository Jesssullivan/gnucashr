-- Income categorization for multi-stream businesses
-- Used by report-generator and tax-estimator agents

let AccountMapping = ../types/AccountMapping.dhall
let AccountType = ../types/AccountType.dhall

let mappings
    : List AccountMapping
    = [ { gnucash_path = "Income:Consulting"
        , account_type = AccountType.INCOME
        , category = "consulting"
        , tags = [ "service", "1099-reportable", "active" ]
        , auto_categorize = True
        }
      , { gnucash_path = "Income:Subscriptions"
        , account_type = AccountType.INCOME
        , category = "subscriptions"
        , tags = [ "recurring", "passive", "mrr" ]
        , auto_categorize = True
        }
      , { gnucash_path = "Income:Product Sales"
        , account_type = AccountType.INCOME
        , category = "product-sales"
        , tags = [ "product", "cogs-applicable" ]
        , auto_categorize = True
        }
      , { gnucash_path = "Income:Interest"
        , account_type = AccountType.INCOME
        , category = "interest-income"
        , tags = [ "passive", "1099-int" ]
        , auto_categorize = True
        }
      , { gnucash_path = "Income:Dividends"
        , account_type = AccountType.INCOME
        , category = "dividend-income"
        , tags = [ "passive", "1099-div" ]
        , auto_categorize = True
        }
      , { gnucash_path = "Income:Grants"
        , account_type = AccountType.INCOME
        , category = "grants"
        , tags = [ "non-recurring", "restricted" ]
        , auto_categorize = False
        }
      ]

in  mappings
