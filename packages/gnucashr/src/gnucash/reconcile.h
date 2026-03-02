#pragma once
// Account reconciliation and cross-institution transfer matching

#include "result.h"
#include "fraction.h"
#include "types.h"
#include <string>
#include <vector>
#include <optional>

namespace gnucash {

class Book;  // forward declare

struct ReconcileResult {
    int splits_reconciled = 0;
    Fraction statement_balance;     // Expected balance
    Fraction book_balance;          // Actual book balance
    Fraction difference;            // statement - book
    bool balanced = false;          // difference is zero
};

struct TransferMatch {
    std::string split_a_guid;       // Split in account A
    std::string split_b_guid;       // Split in account B
    std::string tx_a_guid;          // Transaction in account A
    std::string tx_b_guid;          // Transaction in account B
    Fraction amount;                // Absolute amount
    std::string date_a;
    std::string date_b;
    std::string desc_a;
    std::string desc_b;
    double similarity;              // 0.0-1.0 match confidence
};

// Reconcile an account: mark unreconciled splits as cleared up to statement_date
// statement_balance is the expected balance as of that date
Result<ReconcileResult> reconcile_account(
    Book& book,
    const std::string& account_guid,
    const std::string& statement_date,  // YYYY-MM-DD
    const Fraction& statement_balance);

// Find potential cross-institution transfer matches between two accounts
// Matches by: exact amount (inverted sign) + date within date_window days
std::vector<TransferMatch> find_cross_institution_matches(
    Book& book,
    const std::string& account_a_guid,
    const std::string& account_b_guid,
    const std::string& from_date,       // YYYY-MM-DD
    const std::string& to_date,         // YYYY-MM-DD
    int date_window = 3,
    double min_similarity = 0.5);

} // namespace gnucash
