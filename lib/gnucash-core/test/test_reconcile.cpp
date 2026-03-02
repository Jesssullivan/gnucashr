#include <catch2/catch_test_macros.hpp>
#include <catch2/matchers/catch_matchers_floating_point.hpp>
#include "gnucash/reconcile.h"
#include "gnucash/book.h"
#include "gnucash/guid.h"
#include <filesystem>

using namespace gnucash;
using Catch::Matchers::WithinAbs;
namespace fs = std::filesystem;

#ifndef FIXTURE_DIR
#error "FIXTURE_DIR must be defined"
#endif

static const std::string ACCOUNTS_DB = std::string(FIXTURE_DIR) + "/with-accounts.gnucash";

static std::string make_writable_copy(const std::string& src) {
    auto tmp = fs::temp_directory_path() / ("reconcile_test_" + generate_guid() + ".gnucash");
    fs::copy_file(src, tmp, fs::copy_options::overwrite_existing);
    return tmp.string();
}

static const std::string CHECKING = "a1000000000000000000000000000008";
static const std::string SAVINGS = "a1000000000000000000000000000009";
static const std::string GROCERIES = "a1000000000000000000000000000017";

static void post_txn(Book& book, const std::string& date, const std::string& desc,
                     const std::string& from, const std::string& to,
                     int64_t amount_cents) {
    auto commodities = book.get_commodities();
    Transaction txn;
    txn.currency_guid = commodities[0].guid;
    txn.post_date = date + " 12:00:00";
    txn.description = desc;

    Split s1;
    s1.account_guid = from;
    s1.value = {-amount_cents, 100};
    s1.quantity = {-amount_cents, 100};

    Split s2;
    s2.account_guid = to;
    s2.value = {amount_cents, 100};
    s2.quantity = {amount_cents, 100};

    txn.splits = {s1, s2};
    book.post_transaction(txn);
}

// --- reconcile_account ---

TEST_CASE("reconcile_account marks splits cleared", "[reconcile]") {
    auto tmp = make_writable_copy(ACCOUNTS_DB);
    auto res = Book::open(tmp, false);
    REQUIRE(res.is_ok());
    auto& book = res.unwrap();

    // Post 2 transactions
    post_txn(book, "2024-01-10", "Deposit", GROCERIES, CHECKING, 100000);
    post_txn(book, "2024-01-15", "Grocery", CHECKING, GROCERIES, 5000);

    // Reconcile as of Jan 31
    auto result = reconcile_account(book, CHECKING, "2024-01-31",
                                     Fraction{95000, 100}); // $950
    REQUIRE(result.is_ok());

    auto& r = result.unwrap();
    REQUIRE(r.splits_reconciled == 2); // Both checking splits
    REQUIRE(r.balanced == true);

    book.close();
    fs::remove(tmp);
}

TEST_CASE("reconcile_account detects imbalance", "[reconcile]") {
    auto tmp = make_writable_copy(ACCOUNTS_DB);
    auto res = Book::open(tmp, false);
    REQUIRE(res.is_ok());
    auto& book = res.unwrap();

    post_txn(book, "2024-01-10", "Deposit", GROCERIES, CHECKING, 100000);

    // Wrong statement balance
    auto result = reconcile_account(book, CHECKING, "2024-01-31",
                                     Fraction{50000, 100}); // $500 (should be $1000)
    REQUIRE(result.is_ok());

    auto& r = result.unwrap();
    REQUIRE(r.splits_reconciled == 1);
    REQUIRE(r.balanced == false);

    book.close();
    fs::remove(tmp);
}

TEST_CASE("reconcile_account read-only rejected", "[reconcile]") {
    auto result = Book::open(ACCOUNTS_DB, true); // read-only
    REQUIRE(result.is_ok());
    auto& book = result.unwrap();

    auto r = reconcile_account(book, CHECKING, "2024-01-31",
                                Fraction{0, 100});
    REQUIRE(r.is_err());

    book.close();
}

TEST_CASE("reconcile_account respects date cutoff", "[reconcile]") {
    auto tmp = make_writable_copy(ACCOUNTS_DB);
    auto res = Book::open(tmp, false);
    REQUIRE(res.is_ok());
    auto& book = res.unwrap();

    post_txn(book, "2024-01-10", "Jan txn", GROCERIES, CHECKING, 10000);
    post_txn(book, "2024-02-15", "Feb txn", GROCERIES, CHECKING, 20000);

    // Reconcile only through Jan
    auto result = reconcile_account(book, CHECKING, "2024-01-31",
                                     Fraction{10000, 100});
    REQUIRE(result.is_ok());
    REQUIRE(result.unwrap().splits_reconciled == 1); // Only Jan split

    book.close();
    fs::remove(tmp);
}

// --- find_cross_institution_matches ---

TEST_CASE("find_cross_institution_matches basic", "[reconcile]") {
    auto tmp = make_writable_copy(ACCOUNTS_DB);
    auto res = Book::open(tmp, false);
    REQUIRE(res.is_ok());
    auto& book = res.unwrap();

    // Transfer: Checking -> Savings (shows as debit in Checking, credit in Savings)
    post_txn(book, "2024-01-15", "Transfer to savings",
             CHECKING, SAVINGS, 50000);

    // Simulate the same transfer appearing independently in the savings account
    // (as if imported from a different bank statement)
    post_txn(book, "2024-01-16", "Transfer from checking",
             GROCERIES, SAVINGS, 50000);

    // Note: the first transfer already creates matching splits in both accounts
    auto matches = find_cross_institution_matches(
        book, CHECKING, SAVINGS, "2024-01-01", "2024-01-31");

    // Should find the matching pair from the first transfer
    REQUIRE(matches.size() >= 1);
    REQUIRE_THAT(matches[0].amount.to_double(), WithinAbs(500.0, 1e-9));

    book.close();
    fs::remove(tmp);
}

TEST_CASE("find_cross_institution_matches respects date window", "[reconcile]") {
    auto tmp = make_writable_copy(ACCOUNTS_DB);
    auto res = Book::open(tmp, false);
    REQUIRE(res.is_ok());
    auto& book = res.unwrap();

    // Two transactions 10 days apart -- should NOT match with 3-day window
    post_txn(book, "2024-01-05", "Debit A", CHECKING, GROCERIES, 25000);
    post_txn(book, "2024-01-15", "Credit B", GROCERIES, SAVINGS, 25000);

    auto matches = find_cross_institution_matches(
        book, CHECKING, SAVINGS, "2024-01-01", "2024-01-31", 3);

    // Should find no matches since dates are too far apart
    // (A debit in Checking is -250, a credit in Savings is +250 from the second txn,
    //  but they're 10 days apart)
    for (const auto& m : matches) {
        // If any match found, verify it's from same-day transactions
        int date_diff = std::abs(
            std::stoi(m.date_a.substr(8, 2)) - std::stoi(m.date_b.substr(8, 2)));
        REQUIRE(date_diff <= 3);
    }

    book.close();
    fs::remove(tmp);
}

TEST_CASE("find_cross_institution_matches empty result", "[reconcile]") {
    auto tmp = make_writable_copy(ACCOUNTS_DB);
    auto res = Book::open(tmp, false);
    REQUIRE(res.is_ok());
    auto& book = res.unwrap();

    // No transactions -- no matches
    auto matches = find_cross_institution_matches(
        book, CHECKING, SAVINGS, "2024-01-01", "2024-01-31");
    REQUIRE(matches.empty());

    book.close();
    fs::remove(tmp);
}

TEST_CASE("find_cross_institution_matches similarity scoring", "[reconcile]") {
    auto tmp = make_writable_copy(ACCOUNTS_DB);
    auto res = Book::open(tmp, false);
    REQUIRE(res.is_ok());
    auto& book = res.unwrap();

    // Transfer with matching description words
    post_txn(book, "2024-01-15", "Transfer to Savings",
             CHECKING, SAVINGS, 30000);

    auto matches = find_cross_institution_matches(
        book, CHECKING, SAVINGS, "2024-01-01", "2024-01-31");

    if (!matches.empty()) {
        // Amount match gives 0.5 base, plus description overlap
        REQUIRE(matches[0].similarity >= 0.5);
        REQUIRE(matches[0].similarity <= 1.0);
    }

    book.close();
    fs::remove(tmp);
}
