-- Account mapping for agent categorization
-- Maps GnuCash account paths to categories and tags for agent consumption

let AccountType = ./AccountType.dhall

let AccountMapping =
      { gnucash_path : Text
      , account_type : AccountType
      , category : Text
      , tags : List Text
      , auto_categorize : Bool
      }

in  AccountMapping
