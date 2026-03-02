#pragma once
// Bank feed import engine
// Imports OFX and CSV statements with FITID dedup via slots table

#include "result.h"
#include "fraction.h"
#include "csv.h"
#include <string>
#include <vector>
#include <map>

namespace gnucash {

// Forward declare Book to avoid circular includes
class Book;

struct ImportResult {
    int total_parsed = 0;
    int imported = 0;
    int duplicates = 0;
    int errors = 0;
    std::vector<std::string> imported_guids;  // transaction GUIDs
    std::vector<std::string> error_messages;
};

// Import OFX content into a book
// - Parses OFX, dedup by FITID via slots table, posts transactions
// - Each OFX transaction becomes a 2-split transaction:
//   target_account (bank account) <-> imbalance_account
// - Sets online_id slot on the target split for future dedup
Result<ImportResult> import_ofx(Book& book,
                                 const std::string& content,
                                 const std::string& target_account_guid,
                                 const std::string& imbalance_account_guid);

// Import CSV content into a book
// - Same flow as OFX but with CSV parser
// - Uses CsvTransaction.id as FITID (if available)
// - Generates synthetic FITID from date+amount+desc if no id column
Result<ImportResult> import_csv(Book& book,
                                 const std::string& content,
                                 const CsvFormat& format,
                                 const std::string& target_account_guid,
                                 const std::string& imbalance_account_guid);

// Batch check which FITIDs already exist for an account
// Returns map of fitid -> existing split_guid
std::map<std::string, std::string> check_duplicates(
    Book& book,
    const std::string& account_guid,
    const std::vector<std::string>& fitids);

} // namespace gnucash
