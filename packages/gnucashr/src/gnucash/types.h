#pragma once
// GnuCash data types matching the SQLite schema
// See: docs/epic/02-GNUCASH-INTERFACE.md

#include "fraction.h"
#include <string>
#include <vector>
#include <optional>

namespace gnucash {

// Account types (mirrors dhall/types/AccountType.dhall)
enum class AccountType {
    NONE, BANK, CASH, CREDIT, ASSET, LIABILITY, STOCK, MUTUAL,
    CURRENCY, INCOME, EXPENSE, EQUITY, RECEIVABLE, PAYABLE, ROOT, TRADING
};

AccountType parse_account_type(const std::string& s);
std::string account_type_to_string(AccountType t);

// Reconcile state
enum class ReconcileState { NOT_RECONCILED, CLEARED, RECONCILED };

struct Commodity {
    std::string guid;
    std::string ns;         // "CURRENCY", "NYSE", etc.
    std::string mnemonic;   // "USD", "AAPL", etc.
    std::string fullname;
    std::string cusip;
    int fraction;           // 100 for USD, 1 for JPY
    bool quote_flag;
    std::string quote_source;
};

struct Account {
    std::string guid;
    std::string name;
    AccountType type;
    std::string commodity_guid;
    int commodity_scu;
    bool non_std_scu;
    std::string parent_guid;
    std::string code;
    std::string description;
    bool hidden;
    bool placeholder;

    // Computed (not stored in DB)
    std::string full_path;
};

struct Split {
    std::string guid;
    std::string tx_guid;
    std::string account_guid;
    std::string memo;
    std::string action;
    ReconcileState reconcile_state;
    std::optional<std::string> reconcile_date;
    Fraction value;     // {value_num, value_denom}
    Fraction quantity;  // {quantity_num, quantity_denom}
    std::optional<std::string> lot_guid;
};

struct Transaction {
    std::string guid;
    std::string currency_guid;
    std::string num;
    std::string post_date;   // "YYYY-MM-DD HH:MM:SS"
    std::string enter_date;
    std::string description;
    std::vector<Split> splits;
};

struct Price {
    std::string guid;
    std::string commodity_guid;
    std::string currency_guid;
    std::string date;
    std::string source;
    std::string type;
    Fraction value;
};

struct TrialBalanceEntry {
    std::string account_guid;
    std::string account_name;
    std::string full_path;
    AccountType account_type;
    double balance;
};

using TrialBalance = std::vector<TrialBalanceEntry>;

} // namespace gnucash
