// Account reconciliation and cross-institution transfer matching

#include "gnucash/reconcile.h"
#include "gnucash/book.h"
#include <sqlite3.h>
#include <cmath>
#include <algorithm>
#include <sstream>
#include <cctype>
#include <unordered_map>

namespace gnucash {

namespace {
    struct StmtGuard {
        sqlite3_stmt* stmt;
        StmtGuard(sqlite3_stmt* s) : stmt(s) {}
        ~StmtGuard() { if (stmt) sqlite3_finalize(stmt); }
    };

    // Calculate date difference in days between two YYYY-MM-DD strings
    int date_diff_days(const std::string& a, const std::string& b) {
        if (a.size() < 10 || b.size() < 10) return 9999;

        // Parse YYYY-MM-DD
        auto parse = [](const std::string& d) -> int {
            int y = std::stoi(d.substr(0, 4));
            int m = std::stoi(d.substr(5, 2));
            int day = std::stoi(d.substr(8, 2));
            // Approximate day number (good enough for <=30 day windows)
            return y * 365 + m * 30 + day;
        };

        return std::abs(parse(a) - parse(b));
    }

    // Simple word overlap similarity between two strings
    double description_similarity(const std::string& a, const std::string& b) {
        if (a.empty() || b.empty()) return 0.0;

        // Tokenize and lowercase
        auto tokenize = [](const std::string& s) {
            std::vector<std::string> tokens;
            std::string token;
            for (char c : s) {
                if (std::isalnum(static_cast<unsigned char>(c))) {
                    token += std::tolower(static_cast<unsigned char>(c));
                } else if (!token.empty()) {
                    tokens.push_back(token);
                    token.clear();
                }
            }
            if (!token.empty()) tokens.push_back(token);
            return tokens;
        };

        auto tokens_a = tokenize(a);
        auto tokens_b = tokenize(b);

        if (tokens_a.empty() || tokens_b.empty()) return 0.0;

        // Count overlapping tokens
        int overlap = 0;
        for (const auto& ta : tokens_a) {
            for (const auto& tb : tokens_b) {
                if (ta == tb) { overlap++; break; }
            }
        }

        // Jaccard-like: overlap / max(|A|, |B|)
        return static_cast<double>(overlap) /
               static_cast<double>(std::max(tokens_a.size(), tokens_b.size()));
    }
} // anon

Result<ReconcileResult> reconcile_account(
    Book& book,
    const std::string& account_guid,
    const std::string& statement_date,
    const Fraction& statement_balance) {

    if (book.is_read_only())
        return Result<ReconcileResult>::err("Database is read-only");

    auto db = book.raw_db();
    ReconcileResult result;
    result.statement_balance = statement_balance;

    // Mark unreconciled splits as cleared up to statement_date
    // Only affect splits for transactions posted on or before statement_date
    sqlite3_stmt* stmt = nullptr;
    int rc = sqlite3_prepare_v2(db,
        "UPDATE splits SET reconcile_state = 'c' "
        "WHERE account_guid = ? "
        "AND reconcile_state = 'n' "
        "AND tx_guid IN ("
        "  SELECT guid FROM transactions WHERE post_date <= ?"
        ")",
        -1, &stmt, nullptr);
    if (rc != SQLITE_OK)
        return Result<ReconcileResult>::err(std::string("SQL error: ") + sqlite3_errmsg(db));
    StmtGuard sg(stmt);

    sqlite3_bind_text(stmt, 1, account_guid.c_str(), -1, SQLITE_STATIC);
    std::string cutoff = statement_date + " 23:59:59";
    sqlite3_bind_text(stmt, 2, cutoff.c_str(), -1, SQLITE_STATIC);

    if (sqlite3_step(stmt) != SQLITE_DONE)
        return Result<ReconcileResult>::err(std::string("Reconcile failed: ") + sqlite3_errmsg(db));

    result.splits_reconciled = sqlite3_changes(db);

    // Calculate actual book balance
    double bal = book.get_account_balance(account_guid, cutoff);
    result.book_balance = Fraction::from_double(bal);
    result.difference = add(statement_balance, negate(result.book_balance));
    result.balanced = result.difference.is_zero();

    return Result<ReconcileResult>::ok(result);
}

std::vector<TransferMatch> find_cross_institution_matches(
    Book& book,
    const std::string& account_a_guid,
    const std::string& account_b_guid,
    const std::string& from_date,
    const std::string& to_date,
    int date_window,
    double min_similarity) {

    std::vector<TransferMatch> matches;

    // Get splits for both accounts in date range
    auto splits_a = book.get_splits_for_account(account_a_guid);
    auto splits_b = book.get_splits_for_account(account_b_guid);

    // Get transactions for date info
    auto txns = book.get_transactions(from_date + " 00:00:00", to_date + " 23:59:59");

    // Build a map of tx_guid -> transaction for quick lookup
    std::unordered_map<std::string, const Transaction*> tx_map;
    for (const auto& tx : txns) {
        tx_map[tx.guid] = &tx;
    }

    // Filter splits to those within date range
    auto in_range = [&](const Split& s) {
        auto it = tx_map.find(s.tx_guid);
        return it != tx_map.end();
    };

    // For each split in A, find matching splits in B
    for (const auto& sa : splits_a) {
        if (!in_range(sa)) continue;
        auto tx_a = tx_map[sa.tx_guid];

        for (const auto& sb : splits_b) {
            if (!in_range(sb)) continue;
            auto tx_b = tx_map[sb.tx_guid];

            // Check amount: must be inverted (A debit = B credit)
            Fraction sum = add(sa.value, sb.value);
            if (!sum.is_zero()) continue;

            // Check date window
            std::string date_a = tx_a->post_date.substr(0, 10);
            std::string date_b = tx_b->post_date.substr(0, 10);
            if (date_diff_days(date_a, date_b) > date_window) continue;

            // Calculate similarity
            double sim = description_similarity(tx_a->description, tx_b->description);

            // Amount match alone gives base similarity of 0.5
            double total_sim = 0.5 + 0.5 * sim;

            if (total_sim < min_similarity) continue;

            TransferMatch match;
            match.split_a_guid = sa.guid;
            match.split_b_guid = sb.guid;
            match.tx_a_guid = sa.tx_guid;
            match.tx_b_guid = sb.tx_guid;
            match.amount = sa.value;
            if (match.amount.num < 0) match.amount.num = -match.amount.num;
            match.date_a = date_a;
            match.date_b = date_b;
            match.desc_a = tx_a->description;
            match.desc_b = tx_b->description;
            match.similarity = total_sim;

            matches.push_back(match);
        }
    }

    // Sort by similarity descending
    std::sort(matches.begin(), matches.end(),
              [](const TransferMatch& a, const TransferMatch& b) {
                  return a.similarity > b.similarity;
              });

    return matches;
}

} // namespace gnucash
