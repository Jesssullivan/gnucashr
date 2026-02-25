-- GnuCash account types (mirrors gnucashr R package)
-- See: packages/gnucashr/R/write-account.R

let AccountType =
      < NONE
      | BANK
      | CASH
      | CREDIT
      | ASSET
      | LIABILITY
      | STOCK
      | MUTUAL
      | CURRENCY
      | INCOME
      | EXPENSE
      | EQUITY
      | RECEIVABLE
      | PAYABLE
      | ROOT
      | TRADING
      >

in  AccountType
