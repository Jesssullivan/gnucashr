// ofx-parser.cpp - High-performance OFX/QFX file parser for bank statement imports
//
// OFX (Open Financial Exchange) files come in two formats:
// - v1.x: SGML-based (more common for bank downloads)
// - v2.x: XML-based
//
// This parser handles both formats, extracting transaction data from STMTTRN blocks.

#include <Rcpp.h>
#include <string>
#include <vector>
#include <regex>
#include <algorithm>
#include <cctype>

using namespace Rcpp;

// Helper function to trim whitespace from string
std::string trim(const std::string& str) {
    size_t first = str.find_first_not_of(" \t\n\r");
    if (first == std::string::npos) return "";
    size_t last = str.find_last_not_of(" \t\n\r");
    return str.substr(first, last - first + 1);
}

// Helper function to extract value between tags (SGML style: <TAG>value or XML style: <TAG>value</TAG>)
std::string extract_tag_value(const std::string& content, const std::string& tag) {
    // Try SGML style first: <TAG>value (no closing tag, value ends at newline or next <)
    std::string open_tag = "<" + tag + ">";
    size_t start = content.find(open_tag);
    if (start == std::string::npos) {
        // Try case-insensitive search
        std::string content_upper = content;
        std::string tag_upper = open_tag;
        std::transform(content_upper.begin(), content_upper.end(), content_upper.begin(), ::toupper);
        std::transform(tag_upper.begin(), tag_upper.end(), tag_upper.begin(), ::toupper);
        start = content_upper.find(tag_upper);
        if (start == std::string::npos) {
            return "";
        }
    }

    start += open_tag.length();

    // Find end of value: either closing tag, newline, or next opening tag
    size_t end = content.length();

    // Check for closing tag (XML style)
    std::string close_tag = "</" + tag + ">";
    size_t close_pos = content.find(close_tag, start);
    if (close_pos != std::string::npos && close_pos < end) {
        end = close_pos;
    }

    // Check for newline (SGML style)
    size_t newline_pos = content.find_first_of("\n\r", start);
    if (newline_pos != std::string::npos && newline_pos < end) {
        end = newline_pos;
    }

    // Check for next opening tag
    size_t next_tag = content.find("<", start);
    if (next_tag != std::string::npos && next_tag < end) {
        end = next_tag;
    }

    return trim(content.substr(start, end - start));
}

// Parse OFX date format (YYYYMMDD or YYYYMMDDHHMMSS[.XXX][TZ])
// Returns date string in ISO format (YYYY-MM-DD)
std::string parse_ofx_date(const std::string& date_str) {
    if (date_str.length() < 8) {
        return "";
    }

    // Extract just YYYYMMDD portion
    std::string year = date_str.substr(0, 4);
    std::string month = date_str.substr(4, 2);
    std::string day = date_str.substr(6, 2);

    return year + "-" + month + "-" + day;
}

// Extract all STMTTRN blocks from OFX content
std::vector<std::string> extract_stmttrn_blocks(const std::string& content) {
    std::vector<std::string> blocks;

    // Pattern matches <STMTTRN>...</STMTTRN> or <STMTTRN>...<STMTTRN> (SGML)
    std::string content_upper = content;
    std::transform(content_upper.begin(), content_upper.end(), content_upper.begin(), ::toupper);

    size_t pos = 0;
    while (pos < content.length()) {
        size_t start = content_upper.find("<STMTTRN>", pos);
        if (start == std::string::npos) break;

        // Find end: either </STMTTRN> or next <STMTTRN> or </STMTTRNRS> or </BANKTRANLIST>
        size_t end = content.length();

        size_t close_tag = content_upper.find("</STMTTRN>", start + 9);
        size_t next_open = content_upper.find("<STMTTRN>", start + 9);
        size_t list_end = content_upper.find("</BANKTRANLIST>", start + 9);
        size_t stmttrnrs_end = content_upper.find("</STMTTRNRS>", start + 9);

        // Take the minimum valid position
        if (close_tag != std::string::npos) end = std::min(end, close_tag);
        if (next_open != std::string::npos) end = std::min(end, next_open);
        if (list_end != std::string::npos) end = std::min(end, list_end);
        if (stmttrnrs_end != std::string::npos) end = std::min(end, stmttrnrs_end);

        // Extract block content (skip opening tag)
        blocks.push_back(content.substr(start + 9, end - start - 9));

        pos = end;
        if (close_tag != std::string::npos && close_tag == end) {
            pos = close_tag + 10; // Skip </STMTTRN>
        }
    }

    return blocks;
}

//' Detect OFX File Version
//'
//' Determine whether an OFX file is version 1.x (SGML) or 2.x (XML).
//'
//' @param content String content of the OFX file
//' @return String: "1" for SGML, "2" for XML, "unknown" otherwise
//' @export
// [[Rcpp::export]]
std::string detect_ofx_version_cpp(std::string content) {
    // Check for XML declaration (OFX 2.x)
    if (content.find("<?xml") != std::string::npos ||
        content.find("<?XML") != std::string::npos) {
        return "2";
    }

    // Check for OFX header (v1.x SGML)
    // OFXHEADER:100 is the standard v1 header
    if (content.find("OFXHEADER:") != std::string::npos) {
        return "1";
    }

    // Check if it looks like SGML (has <OFX> without XML declaration)
    std::string content_upper = content;
    std::transform(content_upper.begin(), content_upper.end(), content_upper.begin(), ::toupper);
    if (content_upper.find("<OFX>") != std::string::npos) {
        // SGML if no XML prolog
        return "1";
    }

    return "unknown";
}

//' Parse OFX Content
//'
//' Parse OFX/QFX file content and extract transaction data.
//' Handles both OFX 1.x (SGML) and 2.x (XML) formats.
//'
//' @param content String content of the OFX file
//' @return List with vectors: dates, amounts, names, fitids, memos, trntype, currency
//' @export
// [[Rcpp::export]]
List parse_ofx_cpp(std::string content) {
    std::vector<std::string> dates;
    std::vector<double> amounts;
    std::vector<std::string> names;
    std::vector<std::string> fitids;
    std::vector<std::string> memos;
    std::vector<std::string> trntypes;

    // Extract currency (CURDEF tag)
    std::string currency = extract_tag_value(content, "CURDEF");
    if (currency.empty()) {
        currency = "USD"; // Default
    }

    // Extract all transaction blocks
    std::vector<std::string> blocks = extract_stmttrn_blocks(content);

    for (const auto& block : blocks) {
        // Extract transaction fields
        std::string dtposted = extract_tag_value(block, "DTPOSTED");
        std::string trnamt = extract_tag_value(block, "TRNAMT");
        std::string name = extract_tag_value(block, "NAME");
        std::string fitid = extract_tag_value(block, "FITID");
        std::string memo = extract_tag_value(block, "MEMO");
        std::string trntype = extract_tag_value(block, "TRNTYPE");

        // Parse date
        std::string date_iso = parse_ofx_date(dtposted);

        // Parse amount
        double amount = 0.0;
        if (!trnamt.empty()) {
            try {
                // Remove any currency symbols or commas
                std::string clean_amt = trnamt;
                clean_amt.erase(std::remove(clean_amt.begin(), clean_amt.end(), ','), clean_amt.end());
                clean_amt.erase(std::remove(clean_amt.begin(), clean_amt.end(), '$'), clean_amt.end());
                amount = std::stod(clean_amt);
            } catch (...) {
                amount = NA_REAL;
            }
        }

        // Use PAYEE as fallback if NAME is empty
        if (name.empty()) {
            name = extract_tag_value(block, "PAYEE");
        }

        // Store extracted values
        dates.push_back(date_iso);
        amounts.push_back(amount);
        names.push_back(name);
        fitids.push_back(fitid);
        memos.push_back(memo);
        trntypes.push_back(trntype);
    }

    return List::create(
        Named("dates") = wrap(dates),
        Named("amounts") = wrap(amounts),
        Named("names") = wrap(names),
        Named("fitids") = wrap(fitids),
        Named("memos") = wrap(memos),
        Named("trntypes") = wrap(trntypes),
        Named("currency") = wrap(currency),
        Named("n_transactions") = wrap(static_cast<int>(blocks.size()))
    );
}

//' Extract OFX Account Info
//'
//' Extract account information from OFX content (ACCTID, BANKID, etc.)
//'
//' @param content String content of the OFX file
//' @return List with account_id, bank_id, account_type, org_name
//' @export
// [[Rcpp::export]]
List extract_ofx_account_info(std::string content) {
    std::string account_id = extract_tag_value(content, "ACCTID");
    std::string bank_id = extract_tag_value(content, "BANKID");
    std::string account_type = extract_tag_value(content, "ACCTTYPE");
    std::string org_name = extract_tag_value(content, "ORG");
    std::string fid = extract_tag_value(content, "FID");

    // Extract statement date range
    std::string dtstart = extract_tag_value(content, "DTSTART");
    std::string dtend = extract_tag_value(content, "DTEND");

    // Extract balance info
    std::string ledger_bal = extract_tag_value(content, "BALAMT");

    return List::create(
        Named("account_id") = wrap(account_id),
        Named("bank_id") = wrap(bank_id),
        Named("account_type") = wrap(account_type),
        Named("org_name") = wrap(org_name),
        Named("fid") = wrap(fid),
        Named("date_start") = wrap(parse_ofx_date(dtstart)),
        Named("date_end") = wrap(parse_ofx_date(dtend)),
        Named("ledger_balance") = wrap(ledger_bal)
    );
}
