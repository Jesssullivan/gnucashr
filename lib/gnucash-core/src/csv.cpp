// CSV parser for bank statement imports
// Handles PayPal, Stripe, Venmo, Apple Card, and generic formats

#include "gnucash/csv.h"
#include <sstream>
#include <algorithm>
#include <cctype>

namespace gnucash {

// --- Predefined formats ---

CsvFormat paypal_format() {
    CsvFormat f;
    f.name = "paypal";
    f.date_col = 0;         // "Date"
    f.amount_col = 7;       // "Gross" (column H)
    f.desc_col = 3;         // "Name"
    f.memo_col = 4;         // "Type"
    f.id_col = 12;          // "Transaction ID"
    f.date_format = "MM/DD/YYYY";
    return f;
}

CsvFormat stripe_format() {
    CsvFormat f;
    f.name = "stripe";
    f.date_col = 1;         // "Created (UTC)"
    f.amount_col = 4;       // "Amount"
    f.desc_col = 2;         // "Description"
    f.id_col = 0;           // "id"
    f.date_format = "YYYY-MM-DD";
    return f;
}

CsvFormat venmo_format() {
    CsvFormat f;
    f.name = "venmo";
    f.date_col = 2;         // "Datetime"
    f.amount_col = 8;       // "Amount (total)"
    f.desc_col = 5;         // "Note"
    f.memo_col = 3;         // "Type"
    f.id_col = 1;           // "ID"
    f.date_format = "YYYY-MM-DD";
    return f;
}

CsvFormat apple_card_format() {
    CsvFormat f;
    f.name = "apple_card";
    f.date_col = 0;         // "Transaction Date"
    f.amount_col = 6;       // "Amount (USD)"
    f.desc_col = 2;         // "Description"
    f.memo_col = 3;         // "Merchant"
    f.category_col = 5;     // "Category"
    f.date_format = "MM/DD/YYYY";
    return f;
}

CsvFormat generic_format() {
    CsvFormat f;
    f.name = "generic";
    f.date_col = 0;
    f.amount_col = 1;
    f.desc_col = 2;
    f.date_format = "YYYY-MM-DD";
    return f;
}

// --- CSV line parser ---

std::vector<std::string> parse_csv_line(const std::string& line, char delimiter) {
    std::vector<std::string> fields;
    std::string field;
    bool in_quotes = false;

    for (size_t i = 0; i < line.size(); i++) {
        char c = line[i];
        if (in_quotes) {
            if (c == '"') {
                // Check for escaped quote ""
                if (i + 1 < line.size() && line[i + 1] == '"') {
                    field.push_back('"');
                    i++; // skip next quote
                } else {
                    in_quotes = false;
                }
            } else {
                field.push_back(c);
            }
        } else {
            if (c == '"') {
                in_quotes = true;
            } else if (c == delimiter) {
                fields.push_back(field);
                field.clear();
            } else {
                field.push_back(c);
            }
        }
    }
    fields.push_back(field); // last field
    return fields;
}

// --- Format detection ---

namespace {
    // Trim whitespace from both ends
    std::string trim(const std::string& s) {
        size_t start = s.find_first_not_of(" \t\r\n");
        if (start == std::string::npos) return "";
        size_t end = s.find_last_not_of(" \t\r\n");
        return s.substr(start, end - start + 1);
    }

    std::string to_lower(const std::string& s) {
        std::string r = s;
        std::transform(r.begin(), r.end(), r.begin(),
                       [](unsigned char c) { return std::tolower(c); });
        return r;
    }
} // anon

std::optional<CsvFormat> detect_csv_format(const std::string& header_line) {
    auto fields = parse_csv_line(header_line, ',');
    if (fields.size() < 2) return std::nullopt;

    // Normalize headers for matching
    std::vector<std::string> headers;
    for (auto& f : fields) headers.push_back(to_lower(trim(f)));

    // PayPal: "Date","Time","TimeZone","Name","Type",...
    for (const auto& h : headers) {
        if (h == "transaction id" || h == "transaction_id") {
            // Check if this looks like PayPal (has "gross" column)
            for (const auto& h2 : headers) {
                if (h2 == "gross") return paypal_format();
            }
        }
    }

    // Stripe: "id","Created (UTC)","Amount","Description",...
    if (!headers.empty() && headers[0] == "id") {
        for (const auto& h : headers) {
            if (h.find("created") != std::string::npos) return stripe_format();
        }
    }

    // Venmo: ","ID","Datetime","Type",...
    for (const auto& h : headers) {
        if (h == "amount (total)") return venmo_format();
    }

    // Apple Card: "Transaction Date","Clearing Date","Description","Merchant",...
    for (const auto& h : headers) {
        if (h == "transaction date") {
            for (const auto& h2 : headers) {
                if (h2 == "merchant") return apple_card_format();
            }
        }
    }

    return std::nullopt;
}

// --- Date normalization ---

std::string normalize_date(const std::string& date_str, const std::string& format) {
    std::string d = trim(date_str);
    if (d.empty()) return "";

    // If already ISO format, return as-is
    if (format == "YYYY-MM-DD") {
        // Truncate time portion if present ("2024-01-15T12:00:00" -> "2024-01-15")
        if (d.size() >= 10) return d.substr(0, 10);
        return d;
    }

    // Parse based on format
    int year = 0, month = 0, day = 0;

    if (format == "MM/DD/YYYY" || format == "MM/DD/YY") {
        // 01/15/2024 or 01/15/24
        char sep = '/';
        if (d.find('-') != std::string::npos) sep = '-';
        std::istringstream iss(d);
        std::string part;

        if (std::getline(iss, part, sep)) month = std::stoi(part);
        if (std::getline(iss, part, sep)) day = std::stoi(part);
        if (std::getline(iss, part, sep)) {
            year = std::stoi(part);
            if (year < 100) year += 2000; // 24 -> 2024
        }
    } else if (format == "DD/MM/YYYY") {
        char sep = '/';
        if (d.find('-') != std::string::npos) sep = '-';
        std::istringstream iss(d);
        std::string part;

        if (std::getline(iss, part, sep)) day = std::stoi(part);
        if (std::getline(iss, part, sep)) month = std::stoi(part);
        if (std::getline(iss, part, sep)) year = std::stoi(part);
    } else {
        // Unknown format, return as-is
        return d;
    }

    if (year == 0 || month == 0 || day == 0) return d;

    // Format as YYYY-MM-DD
    char buf[11];
    snprintf(buf, sizeof(buf), "%04d-%02d-%02d", year, month, day);
    return std::string(buf);
}

// --- CSV parser ---

Result<std::vector<CsvTransaction>> parse_csv(const std::string& content,
                                               const CsvFormat& format) {
    std::vector<CsvTransaction> transactions;
    std::istringstream stream(content);
    std::string line;
    int line_num = 0;
    int errors = 0;

    while (std::getline(stream, line)) {
        // Remove trailing \r (Windows line endings)
        if (!line.empty() && line.back() == '\r') line.pop_back();

        line_num++;

        // Skip header
        if (line_num == 1 && format.has_header) continue;

        // Skip empty lines
        if (trim(line).empty()) continue;

        auto fields = parse_csv_line(line, format.delimiter);

        // Need enough fields
        int max_col = std::max({format.date_col, format.amount_col, format.desc_col});
        if (format.memo_col >= 0) max_col = std::max(max_col, format.memo_col);
        if (format.id_col >= 0) max_col = std::max(max_col, format.id_col);
        if (format.category_col >= 0) max_col = std::max(max_col, format.category_col);
        if (format.debit_col >= 0) max_col = std::max(max_col, format.debit_col);
        if (format.credit_col >= 0) max_col = std::max(max_col, format.credit_col);

        if (static_cast<int>(fields.size()) <= max_col) {
            errors++;
            continue; // skip malformed lines
        }

        CsvTransaction txn;

        // Date
        txn.date = normalize_date(trim(fields[format.date_col]), format.date_format);
        if (txn.date.empty()) {
            errors++;
            continue;
        }

        // Amount
        if (format.debit_col >= 0 && format.credit_col >= 0) {
            // Separate debit/credit columns
            std::string debit_str = trim(fields[format.debit_col]);
            std::string credit_str = trim(fields[format.credit_col]);
            if (!debit_str.empty() && debit_str != "0" && debit_str != "0.00") {
                txn.amount = Fraction::from_string(debit_str);
                if (txn.amount.num > 0) txn.amount.num = -txn.amount.num; // debits are negative
            } else if (!credit_str.empty()) {
                txn.amount = Fraction::from_string(credit_str);
                if (txn.amount.num < 0) txn.amount.num = -txn.amount.num; // credits are positive
            } else {
                txn.amount = {0, 100};
            }
        } else {
            std::string amt_str = trim(fields[format.amount_col]);
            if (amt_str.empty()) {
                errors++;
                continue;
            }
            txn.amount = Fraction::from_string(amt_str);
        }

        if (format.negate_amount) {
            txn.amount.num = -txn.amount.num;
        }

        // Description
        txn.description = trim(fields[format.desc_col]);

        // Optional fields
        if (format.memo_col >= 0 && format.memo_col < static_cast<int>(fields.size()))
            txn.memo = trim(fields[format.memo_col]);

        if (format.id_col >= 0 && format.id_col < static_cast<int>(fields.size()))
            txn.id = trim(fields[format.id_col]);

        if (format.category_col >= 0 && format.category_col < static_cast<int>(fields.size()))
            txn.category = trim(fields[format.category_col]);

        // Skip zero-amount transactions (often summary rows)
        if (txn.amount.is_zero() && txn.description.empty()) continue;

        transactions.push_back(std::move(txn));
    }

    if (transactions.empty() && errors > 0) {
        return Result<std::vector<CsvTransaction>>::err(
            "Failed to parse CSV: " + std::to_string(errors) + " malformed lines, 0 valid");
    }

    return Result<std::vector<CsvTransaction>>::ok(std::move(transactions));
}

} // namespace gnucash
