// OFX/QFX parser - standalone C++ implementation
// Extracted from packages/gnucashr/src/ofx-parser.cpp (Rcpp removed)

#include "gnucash/ofx.h"
#include <algorithm>
#include <cctype>
#include <cstdlib>

namespace gnucash {

namespace {

std::string trim(const std::string& str) {
    size_t first = str.find_first_not_of(" \t\n\r");
    if (first == std::string::npos) return "";
    size_t last = str.find_last_not_of(" \t\n\r");
    return str.substr(first, last - first + 1);
}

std::string to_upper(const std::string& s) {
    std::string result = s;
    std::transform(result.begin(), result.end(), result.begin(), ::toupper);
    return result;
}

// Extract value between OFX tags (SGML or XML style)
std::string extract_tag_value(const std::string& content, const std::string& tag) {
    std::string open_tag = "<" + tag + ">";
    size_t start = content.find(open_tag);
    if (start == std::string::npos) {
        // Case-insensitive fallback
        std::string content_upper = to_upper(content);
        std::string tag_upper = to_upper(open_tag);
        start = content_upper.find(tag_upper);
        if (start == std::string::npos)
            return "";
    }

    start += open_tag.length();

    size_t end = content.length();

    // Closing tag (XML style)
    std::string close_tag = "</" + tag + ">";
    size_t close_pos = content.find(close_tag, start);
    if (close_pos == std::string::npos) {
        // Case-insensitive
        std::string content_upper = to_upper(content);
        close_pos = content_upper.find(to_upper(close_tag), start);
    }
    if (close_pos != std::string::npos && close_pos < end)
        end = close_pos;

    // Newline (SGML style)
    size_t newline_pos = content.find_first_of("\n\r", start);
    if (newline_pos != std::string::npos && newline_pos < end)
        end = newline_pos;

    // Next opening tag
    size_t next_tag = content.find("<", start);
    if (next_tag != std::string::npos && next_tag < end)
        end = next_tag;

    return trim(content.substr(start, end - start));
}

// Parse OFX date (YYYYMMDD or YYYYMMDDHHMMSS[.XXX][TZ]) -> YYYY-MM-DD
std::string parse_ofx_date(const std::string& date_str) {
    if (date_str.length() < 8)
        return "";
    return date_str.substr(0, 4) + "-" + date_str.substr(4, 2) + "-" + date_str.substr(6, 2);
}

// Extract all STMTTRN blocks from OFX content
std::vector<std::string> extract_stmttrn_blocks(const std::string& content) {
    std::vector<std::string> blocks;
    std::string content_upper = to_upper(content);

    size_t pos = 0;
    while (pos < content.length()) {
        size_t start = content_upper.find("<STMTTRN>", pos);
        if (start == std::string::npos) break;

        size_t end = content.length();
        size_t close_tag = content_upper.find("</STMTTRN>", start + 9);
        size_t next_open = content_upper.find("<STMTTRN>", start + 9);
        size_t list_end = content_upper.find("</BANKTRANLIST>", start + 9);
        size_t stmttrnrs_end = content_upper.find("</STMTTRNRS>", start + 9);

        if (close_tag != std::string::npos) end = std::min(end, close_tag);
        if (next_open != std::string::npos) end = std::min(end, next_open);
        if (list_end != std::string::npos) end = std::min(end, list_end);
        if (stmttrnrs_end != std::string::npos) end = std::min(end, stmttrnrs_end);

        blocks.push_back(content.substr(start + 9, end - start - 9));

        pos = end;
        if (close_tag != std::string::npos && close_tag == end)
            pos = close_tag + 10;
    }

    return blocks;
}

} // anonymous namespace

std::string detect_ofx_version(const std::string& content) {
    if (content.find("<?xml") != std::string::npos ||
        content.find("<?XML") != std::string::npos)
        return "2";

    if (content.find("OFXHEADER:") != std::string::npos)
        return "1";

    if (to_upper(content).find("<OFX>") != std::string::npos)
        return "1";

    return "unknown";
}

OfxParseResult parse_ofx(const std::string& content) {
    OfxParseResult result;

    // Account info
    result.account.account_id = extract_tag_value(content, "ACCTID");
    result.account.bank_id = extract_tag_value(content, "BANKID");
    result.account.account_type = extract_tag_value(content, "ACCTTYPE");
    result.account.org_name = extract_tag_value(content, "ORG");
    result.account.fid = extract_tag_value(content, "FID");
    result.account.date_start = parse_ofx_date(extract_tag_value(content, "DTSTART"));
    result.account.date_end = parse_ofx_date(extract_tag_value(content, "DTEND"));
    result.account.ledger_balance = extract_tag_value(content, "BALAMT");
    result.account.currency = extract_tag_value(content, "CURDEF");
    if (result.account.currency.empty())
        result.account.currency = "USD";

    // Transactions
    auto blocks = extract_stmttrn_blocks(content);
    result.transactions.reserve(blocks.size());

    for (const auto& block : blocks) {
        OfxTransaction txn;
        txn.date = parse_ofx_date(extract_tag_value(block, "DTPOSTED"));
        txn.name = extract_tag_value(block, "NAME");
        if (txn.name.empty())
            txn.name = extract_tag_value(block, "PAYEE");
        txn.fitid = extract_tag_value(block, "FITID");
        txn.memo = extract_tag_value(block, "MEMO");
        txn.trntype = extract_tag_value(block, "TRNTYPE");

        std::string trnamt = extract_tag_value(block, "TRNAMT");
        if (!trnamt.empty()) {
            std::string clean = trnamt;
            clean.erase(std::remove(clean.begin(), clean.end(), ','), clean.end());
            clean.erase(std::remove(clean.begin(), clean.end(), '$'), clean.end());
            try { txn.amount = std::stod(clean); }
            catch (...) { txn.amount = 0.0; }
            txn.amount_fraction = Fraction::from_string(trnamt);
        } else {
            txn.amount = 0.0;
            txn.amount_fraction = {0, 100};
        }

        result.transactions.push_back(std::move(txn));
    }

    return result;
}

} // namespace gnucash
