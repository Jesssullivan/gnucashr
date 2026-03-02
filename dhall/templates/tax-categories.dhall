-- Tax category mappings for US business expenses
-- Used by tax-estimator agent to categorize expenses for Schedule C / 1120

let AccountMapping = ../types/AccountMapping.dhall
let AccountType = ../types/AccountType.dhall

let mappings
    : List AccountMapping
    = [ -- Schedule C / Form 1120 categories
        { gnucash_path = "Expenses:Office"
        , account_type = AccountType.EXPENSE
        , category = "office-expenses"
        , tags = [ "deductible", "schedule-c-line-18" ]
        , auto_categorize = True
        }
      , { gnucash_path = "Expenses:Rent"
        , account_type = AccountType.EXPENSE
        , category = "rent"
        , tags = [ "deductible", "schedule-c-line-20b" ]
        , auto_categorize = True
        }
      , { gnucash_path = "Expenses:Utilities"
        , account_type = AccountType.EXPENSE
        , category = "utilities"
        , tags = [ "deductible", "schedule-c-line-25" ]
        , auto_categorize = True
        }
      , { gnucash_path = "Expenses:Insurance"
        , account_type = AccountType.EXPENSE
        , category = "insurance"
        , tags = [ "deductible", "schedule-c-line-15" ]
        , auto_categorize = True
        }
      , { gnucash_path = "Expenses:Professional Services"
        , account_type = AccountType.EXPENSE
        , category = "professional-services"
        , tags = [ "deductible", "schedule-c-line-17" ]
        , auto_categorize = True
        }
      , { gnucash_path = "Expenses:Travel"
        , account_type = AccountType.EXPENSE
        , category = "travel"
        , tags = [ "deductible", "schedule-c-line-24a" ]
        , auto_categorize = True
        }
      , { gnucash_path = "Expenses:Meals"
        , account_type = AccountType.EXPENSE
        , category = "meals"
        , tags = [ "partially-deductible", "schedule-c-line-24b" ]
        , auto_categorize = True
        }
      , { gnucash_path = "Expenses:Vehicle"
        , account_type = AccountType.EXPENSE
        , category = "vehicle"
        , tags = [ "deductible", "schedule-c-line-9" ]
        , auto_categorize = False
        }
      , { gnucash_path = "Expenses:Equipment"
        , account_type = AccountType.EXPENSE
        , category = "equipment"
        , tags = [ "depreciable", "section-179" ]
        , auto_categorize = True
        }
      , { gnucash_path = "Expenses:Home Office"
        , account_type = AccountType.EXPENSE
        , category = "home-office"
        , tags = [ "deductible", "form-8829" ]
        , auto_categorize = False
        }
      , { gnucash_path = "Expenses:Advertising"
        , account_type = AccountType.EXPENSE
        , category = "advertising"
        , tags = [ "deductible", "schedule-c-line-8" ]
        , auto_categorize = True
        }
      , { gnucash_path = "Expenses:Interest"
        , account_type = AccountType.EXPENSE
        , category = "interest"
        , tags = [ "deductible", "schedule-c-line-16" ]
        , auto_categorize = True
        }

        -- Income categories
      , { gnucash_path = "Income:Consulting"
        , account_type = AccountType.INCOME
        , category = "consulting-income"
        , tags = [ "taxable", "1099-reportable" ]
        , auto_categorize = True
        }
      , { gnucash_path = "Income:Product Sales"
        , account_type = AccountType.INCOME
        , category = "product-sales"
        , tags = [ "taxable", "sales-tax-applicable" ]
        , auto_categorize = True
        }
      , { gnucash_path = "Income:Subscriptions"
        , account_type = AccountType.INCOME
        , category = "subscription-income"
        , tags = [ "taxable", "recurring" ]
        , auto_categorize = True
        }
      ]

in  mappings
