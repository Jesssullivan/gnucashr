-- GnuCash operations that agents can perform via the JSON API bridge
-- Each operation maps to a gnucash-bridge method with validation rules

let AccountType = ./AccountType.dhall

let AuthorizationLevel = ./AuthorizationLevel.dhall

let Split =
      { account_guid : Text
      , value_num : Integer
      , value_denom : Natural
      , memo : Optional Text
      }

let TransactionRequest =
      { description : Text
      , post_date : Text
      , splits : List Split
      , num : Optional Text
      , currency_guid : Optional Text
      }

let AccountRequest =
      { name : Text
      , account_type : AccountType
      , parent_guid : Text
      , description : Optional Text
      , code : Optional Text
      , hidden : Bool
      , placeholder : Bool
      }

let ImportRequest =
      { file_path : Text
      , target_account_guid : Text
      , default_expense_guid : Optional Text
      , skip_duplicates : Bool
      }

let BalanceQuery =
      { account_guid : Text
      , as_of : Optional Text
      }

let TransactionQuery =
      { from_date : Optional Text
      , to_date : Optional Text
      }

let GnuCashOperation =
      -- Read operations (Auto tier)
      < GetAccounts
      | AccountTree
      | GetAccount : { guid : Text }
      | GetAccountByPath : { path : Text }
      | GetTransactions : TransactionQuery
      | GetBalance : BalanceQuery
      | TrialBalance : { as_of : Optional Text }
      | GetCommodities
      | GetPrices
      -- Write operations (Approve tier)
      | PostTransaction : TransactionRequest
      | CreateAccount : AccountRequest
      | DeleteTransaction : { guid : Text }
      | VoidTransaction : { guid : Text, reason : Text }
      -- Import operations (Review tier)
      | ImportOFX : ImportRequest
      >

in  { GnuCashOperation
    , TransactionRequest
    , AccountRequest
    , ImportRequest
    , BalanceQuery
    , TransactionQuery
    , Split
    }
