#pragma once
// CSV parser for bank statement imports
// Handles PayPal, Stripe, Venmo, Apple Card, and generic CSV formats

#include "fraction.h"
#include "result.h"
#include <string>
#include <vector>
#include <optional>

namespace gnucash {

struct CsvTransaction {
    std::string date;        // ISO format: YYYY-MM-DD
    Fraction amount;         // Signed amount as exact fraction
    std::string description; // Payee/description
    std::string memo;        // Additional details
    std::string id;          // Transaction ID (for dedup)
    std::string category;    // Category hint (if available)
};

struct CsvFormat {
    std::string name;        // Format name
    char delimiter = ',';
    bool has_header = true;

    // Column indices (0-based)
    int date_col = 0;
    int amount_col = 1;
    int desc_col = 2;
    int memo_col = -1;       // -1 = not present
    int id_col = -1;         // -1 = not present
    int category_col = -1;   // -1 = not present

    // Some formats have separate debit/credit columns
    int debit_col = -1;      // -1 = use amount_col
    int credit_col = -1;     // -1 = use amount_col

    // Date format: "YYYY-MM-DD", "MM/DD/YYYY", "MM/DD/YY", "DD/MM/YYYY"
    std::string date_format = "YYYY-MM-DD";

    // Amount sign convention: true if negative means debit (bank convention)
    bool negate_amount = false;
};

// Predefined format constructors
CsvFormat paypal_format();
CsvFormat stripe_format();
CsvFormat venmo_format();
CsvFormat apple_card_format();
CsvFormat generic_format();

// Parse a CSV line handling quoted fields with commas
std::vector<std::string> parse_csv_line(const std::string& line, char delimiter = ',');

// Auto-detect CSV format from header line
std::optional<CsvFormat> detect_csv_format(const std::string& header_line);

// Parse CSV content to transactions
Result<std::vector<CsvTransaction>> parse_csv(const std::string& content,
                                               const CsvFormat& format);

// Parse a date string in the given format to ISO YYYY-MM-DD
std::string normalize_date(const std::string& date_str, const std::string& format);

} // namespace gnucash
