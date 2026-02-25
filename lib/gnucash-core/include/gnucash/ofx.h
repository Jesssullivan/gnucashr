#pragma once
// OFX/QFX file parser for bank statement imports
// Handles both OFX v1.x (SGML) and v2.x (XML) formats

#include <string>
#include <vector>
#include <optional>

namespace gnucash {

// OFX transaction record
struct OfxTransaction {
    std::string date;       // ISO format: YYYY-MM-DD
    double amount;          // Signed amount
    std::string name;       // Payee name
    std::string fitid;      // Financial institution transaction ID
    std::string memo;
    std::string trntype;    // DEBIT, CREDIT, CHECK, etc.
};

// OFX account information
struct OfxAccountInfo {
    std::string account_id;
    std::string bank_id;
    std::string account_type;   // CHECKING, SAVINGS, CREDITLINE, etc.
    std::string org_name;
    std::string fid;
    std::string date_start;     // ISO format
    std::string date_end;       // ISO format
    std::string ledger_balance;
    std::string currency;       // ISO 4217 code
};

// OFX parse result
struct OfxParseResult {
    OfxAccountInfo account;
    std::vector<OfxTransaction> transactions;
};

// Detect OFX version: "1" (SGML), "2" (XML), or "unknown"
std::string detect_ofx_version(const std::string& content);

// Parse OFX file content, returning account info and transactions
OfxParseResult parse_ofx(const std::string& content);

} // namespace gnucash
