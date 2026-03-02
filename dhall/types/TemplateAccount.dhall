-- Account template structure for chart-of-accounts provisioning
-- Uses fixed-depth nesting (max 5 levels, matching GnuCash templates)
-- Dhall does not support recursive types, so we define each level explicitly

let AccountType = ./AccountType.dhall

-- Leaf account (no children)
let AccountL0 =
      { name : Text
      , account_type : AccountType
      , placeholder : Bool
      , description : Text
      , hidden : Bool
      }

-- Account with leaf children
let AccountL1 =
      { name : Text
      , account_type : AccountType
      , placeholder : Bool
      , description : Text
      , hidden : Bool
      , children : List AccountL0
      }

-- Account with L1 children
let AccountL2 =
      { name : Text
      , account_type : AccountType
      , placeholder : Bool
      , description : Text
      , hidden : Bool
      , children : List AccountL1
      }

-- Account with L2 children
let AccountL3 =
      { name : Text
      , account_type : AccountType
      , placeholder : Bool
      , description : Text
      , hidden : Bool
      , children : List AccountL2
      }

-- Top-level account (max depth = 4 children deep)
let AccountL4 =
      { name : Text
      , account_type : AccountType
      , placeholder : Bool
      , description : Text
      , hidden : Bool
      , children : List AccountL3
      }

let Template =
      { name : Text
      , description : Text
      , currency : Text
      , accounts : List AccountL4
      }

in  { AccountL0, AccountL1, AccountL2, AccountL3, AccountL4, Template }
