// Bank feed import engine
// Uses slots table for FITID dedup, posts transactions via Book

#include "gnucash/bank_feed.h"
#include "gnucash/book.h"
#include "gnucash/ofx.h"
#include "gnucash/slots.h"
#include "gnucash/guid.h"
#include "gnucash/types.h"
#include <sqlite3.h>
#include <sstream>
#include <functional>

namespace gnucash {

namespace {
    // Generate a synthetic FITID from transaction data (for CSV without IDs)
    std::string make_synthetic_fitid(const std::string& date,
                                      const Fraction& amount,
                                      const std::string& desc) {
        // Simple hash: date + amount + first 20 chars of description
        std::ostringstream oss;
        oss << date << ":" << amount.num << "/" << amount.denom << ":"
            << desc.substr(0, 20);
        // Use a simple string hash
        std::hash<std::string> hasher;
        auto h = hasher(oss.str());
        // Convert to hex string
        char buf[17];
        snprintf(buf, sizeof(buf), "%016lx", static_cast<unsigned long>(h));
        return std::string("CSV:") + buf;
    }
} // anon

Result<ImportResult> import_ofx(Book& book,
                                 const std::string& content,
                                 const std::string& target_account_guid,
                                 const std::string& imbalance_account_guid) {
    ImportResult result;

    // Parse OFX
    auto parsed = parse_ofx(content);
    result.total_parsed = static_cast<int>(parsed.transactions.size());

    if (result.total_parsed == 0) {
        return Result<ImportResult>::ok(result);
    }

    // Ensure slots table exists
    auto db = book.raw_db();
    auto slots_r = ensure_slots_table(db);
    if (slots_r.is_err())
        return Result<ImportResult>::err("Failed to initialize slots: " + slots_r.unwrap_err());

    // Get currency GUID for transactions
    std::string currency_guid = book.default_currency();
    if (currency_guid.empty()) {
        auto commodities = book.get_commodities();
        if (commodities.empty())
            return Result<ImportResult>::err("No currencies found in book");
        currency_guid = commodities[0].guid;
    }

    // Import each transaction
    for (const auto& ofx_txn : parsed.transactions) {
        // Skip transactions without FITID
        if (ofx_txn.fitid.empty()) {
            result.errors++;
            result.error_messages.push_back("Transaction missing FITID: " + ofx_txn.name);
            continue;
        }

        // Dedup check
        auto existing = find_split_by_fitid(db, target_account_guid, ofx_txn.fitid);
        if (existing) {
            result.duplicates++;
            continue;
        }

        // Build transaction
        Transaction txn;
        txn.currency_guid = currency_guid;
        txn.post_date = ofx_txn.date + " 12:00:00";
        txn.description = ofx_txn.name;

        // Bank account split (uses exact Fraction)
        Split bank_split;
        bank_split.account_guid = target_account_guid;
        bank_split.value = ofx_txn.amount_fraction;
        bank_split.quantity = ofx_txn.amount_fraction;
        bank_split.memo = ofx_txn.memo;
        bank_split.guid = generate_guid();

        // Imbalance split (opposite sign)
        Split imbalance_split;
        imbalance_split.account_guid = imbalance_account_guid;
        imbalance_split.value = negate(ofx_txn.amount_fraction);
        imbalance_split.quantity = negate(ofx_txn.amount_fraction);

        txn.splits = {bank_split, imbalance_split};

        auto post_r = book.post_transaction(txn);
        if (post_r.is_err()) {
            result.errors++;
            result.error_messages.push_back("Failed to post: " + post_r.unwrap_err());
            continue;
        }

        // Set online_id slot on bank split for future dedup
        auto slot_r = set_slot(db, bank_split.guid, "online_id", ofx_txn.fitid);
        if (slot_r.is_err()) {
            // Transaction was posted but slot failed -- log but don't fail
            result.error_messages.push_back("Slot set failed for " + ofx_txn.fitid);
        }

        result.imported++;
        result.imported_guids.push_back(post_r.unwrap());
    }

    return Result<ImportResult>::ok(result);
}

Result<ImportResult> import_csv(Book& book,
                                 const std::string& content,
                                 const CsvFormat& format,
                                 const std::string& target_account_guid,
                                 const std::string& imbalance_account_guid) {
    ImportResult result;

    // Parse CSV
    auto parse_r = parse_csv(content, format);
    if (parse_r.is_err())
        return Result<ImportResult>::err("CSV parse error: " + parse_r.unwrap_err());

    auto& csv_txns = parse_r.unwrap();
    result.total_parsed = static_cast<int>(csv_txns.size());

    if (result.total_parsed == 0) {
        return Result<ImportResult>::ok(result);
    }

    // Ensure slots table exists
    auto db = book.raw_db();
    auto slots_r = ensure_slots_table(db);
    if (slots_r.is_err())
        return Result<ImportResult>::err("Failed to initialize slots: " + slots_r.unwrap_err());

    // Get currency GUID
    std::string currency_guid = book.default_currency();
    if (currency_guid.empty()) {
        auto commodities = book.get_commodities();
        if (commodities.empty())
            return Result<ImportResult>::err("No currencies found in book");
        currency_guid = commodities[0].guid;
    }

    // Import each transaction
    for (const auto& csv_txn : csv_txns) {
        // Generate FITID
        std::string fitid = csv_txn.id.empty()
            ? make_synthetic_fitid(csv_txn.date, csv_txn.amount, csv_txn.description)
            : csv_txn.id;

        // Dedup check
        auto existing = find_split_by_fitid(db, target_account_guid, fitid);
        if (existing) {
            result.duplicates++;
            continue;
        }

        // Build transaction
        Transaction txn;
        txn.currency_guid = currency_guid;
        txn.post_date = csv_txn.date + " 12:00:00";
        txn.description = csv_txn.description;

        Split bank_split;
        bank_split.account_guid = target_account_guid;
        bank_split.value = csv_txn.amount;
        bank_split.quantity = csv_txn.amount;
        bank_split.memo = csv_txn.memo;
        bank_split.guid = generate_guid();

        Split imbalance_split;
        imbalance_split.account_guid = imbalance_account_guid;
        imbalance_split.value = negate(csv_txn.amount);
        imbalance_split.quantity = negate(csv_txn.amount);

        txn.splits = {bank_split, imbalance_split};

        auto post_r = book.post_transaction(txn);
        if (post_r.is_err()) {
            result.errors++;
            result.error_messages.push_back("Failed to post: " + post_r.unwrap_err());
            continue;
        }

        // Set online_id slot
        set_slot(db, bank_split.guid, "online_id", fitid);

        result.imported++;
        result.imported_guids.push_back(post_r.unwrap());
    }

    return Result<ImportResult>::ok(result);
}

std::map<std::string, std::string> check_duplicates(
    Book& book,
    const std::string& account_guid,
    const std::vector<std::string>& fitids) {

    auto db = book.raw_db();
    ensure_slots_table(db);
    return check_fitids(db, account_guid, fitids);
}

} // namespace gnucash
