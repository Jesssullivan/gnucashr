#include <catch2/catch_test_macros.hpp>
#include <catch2/matchers/catch_matchers_floating_point.hpp>
#include "gnucash/book.h"
#include "gnucash/guid.h"
#include <cstdio>
#include <cstdlib>
#include <filesystem>

using namespace gnucash;
using Catch::Matchers::WithinAbs;

namespace fs = std::filesystem;

#ifndef FIXTURE_DIR
#error "FIXTURE_DIR must be defined"
#endif

static const std::string MINIMAL_DB = std::string(FIXTURE_DIR) + "/minimal.gnucash";
static const std::string ACCOUNTS_DB = std::string(FIXTURE_DIR) + "/with-accounts.gnucash";

// Helper: copy a fixture to a temp file for write tests
static std::string make_writable_copy(const std::string& src) {
    auto tmp = fs::temp_directory_path() / ("gnucash_test_" + generate_guid() + ".gnucash");
    fs::copy_file(src, tmp, fs::copy_options::overwrite_existing);
    return tmp.string();
}

// --- Open / close ---

TEST_CASE("Book::open minimal database", "[book]") {
    auto result = Book::open(MINIMAL_DB);
    REQUIRE(result.is_ok());
    auto& book = result.unwrap();
    REQUIRE(book.is_valid());
    REQUIRE(book.is_read_only());
    REQUIRE(book.book_guid() == "f0000000000000000000000000000001");
    REQUIRE(book.root_account_guid() == "a0000000000000000000000000000001");
}

TEST_CASE("Book::open with-accounts database", "[book]") {
    auto result = Book::open(ACCOUNTS_DB);
    REQUIRE(result.is_ok());
    auto& book = result.unwrap();
    REQUIRE(book.book_guid() == "f0000000000000000000000000000002");
    REQUIRE(book.root_account_guid() == "a1000000000000000000000000000001");
}

TEST_CASE("Book::open nonexistent file", "[book]") {
    auto result = Book::open("/tmp/nonexistent_abc123.gnucash");
    REQUIRE(result.is_err());
}

TEST_CASE("Book close and validity", "[book]") {
    auto result = Book::open(MINIMAL_DB);
    REQUIRE(result.is_ok());
    auto& book = result.unwrap();
    REQUIRE(book.is_valid());
    book.close();
    REQUIRE_FALSE(book.is_valid());
}

TEST_CASE("Book move semantics", "[book]") {
    auto result = Book::open(MINIMAL_DB);
    REQUIRE(result.is_ok());
    auto book1 = std::move(result.unwrap());
    REQUIRE(book1.is_valid());

    Book book2 = std::move(book1);
    REQUIRE(book2.is_valid());
    REQUIRE_FALSE(book1.is_valid());
}

// --- Account reading ---

TEST_CASE("get_accounts minimal", "[book]") {
    auto result = Book::open(MINIMAL_DB);
    REQUIRE(result.is_ok());
    auto& book = result.unwrap();

    auto accounts = book.get_accounts();
    REQUIRE(accounts.size() == 1);
    REQUIRE(accounts[0].name == "Root Account");
    REQUIRE(accounts[0].type == AccountType::ROOT);
    REQUIRE(accounts[0].guid == "a0000000000000000000000000000001");
}

TEST_CASE("get_accounts with-accounts", "[book]") {
    auto result = Book::open(ACCOUNTS_DB);
    REQUIRE(result.is_ok());
    auto& book = result.unwrap();

    auto accounts = book.get_accounts();
    REQUIRE(accounts.size() == 20);

    // Verify specific accounts exist
    int bank_count = 0, expense_count = 0, income_count = 0;
    for (const auto& a : accounts) {
        if (a.type == AccountType::BANK) bank_count++;
        if (a.type == AccountType::EXPENSE) expense_count++;
        if (a.type == AccountType::INCOME) income_count++;
    }
    REQUIRE(bank_count == 2);     // Checking + Savings
    REQUIRE(expense_count == 5);  // Expenses + Groceries, Utilities, Rent, Transportation
    REQUIRE(income_count == 3);   // Income + Salary, Interest Income
}

TEST_CASE("get_account by guid", "[book]") {
    auto result = Book::open(ACCOUNTS_DB);
    REQUIRE(result.is_ok());
    auto& book = result.unwrap();

    // Checking Account
    auto checking = book.get_account("a1000000000000000000000000000008");
    REQUIRE(checking.has_value());
    REQUIRE(checking->name == "Checking Account");
    REQUIRE(checking->type == AccountType::BANK);
    REQUIRE(checking->code == "1001");
    REQUIRE(checking->description == "Primary checking");
    REQUIRE_FALSE(checking->placeholder);

    // Nonexistent
    auto missing = book.get_account("0000000000000000000000000000dead");
    REQUIRE_FALSE(missing.has_value());
}

TEST_CASE("account_tree builds full paths", "[book]") {
    auto result = Book::open(ACCOUNTS_DB);
    REQUIRE(result.is_ok());
    auto& book = result.unwrap();

    auto tree = book.account_tree();
    REQUIRE(tree.size() == 20);

    // Find Checking Account and verify its path
    for (const auto& a : tree) {
        if (a.name == "Checking Account") {
            // Root Account:Assets:Current Assets:Checking Account
            REQUIRE(a.full_path.find("Assets") != std::string::npos);
            REQUIRE(a.full_path.find("Current Assets") != std::string::npos);
            REQUIRE(a.full_path.find("Checking Account") != std::string::npos);
        }
        if (a.name == "Groceries") {
            REQUIRE(a.full_path.find("Expenses") != std::string::npos);
            REQUIRE(a.full_path.find("Groceries") != std::string::npos);
        }
    }
}

TEST_CASE("get_account_by_path", "[book]") {
    auto result = Book::open(ACCOUNTS_DB);
    REQUIRE(result.is_ok());
    auto& book = result.unwrap();

    // Build tree first so paths are populated
    auto tree = book.account_tree();
    // Find checking path
    std::string checking_path;
    for (const auto& a : tree) {
        if (a.name == "Checking Account") {
            checking_path = a.full_path;
            break;
        }
    }
    REQUIRE_FALSE(checking_path.empty());

    auto found = book.get_account_by_path(checking_path);
    REQUIRE(found.has_value());
    REQUIRE(found->name == "Checking Account");
}

// --- Commodity reading ---

TEST_CASE("get_commodities", "[book]") {
    auto result = Book::open(ACCOUNTS_DB);
    REQUIRE(result.is_ok());
    auto& book = result.unwrap();

    auto commodities = book.get_commodities();
    REQUIRE(commodities.size() >= 1);

    bool found_usd = false;
    for (const auto& c : commodities) {
        if (c.mnemonic == "USD") {
            found_usd = true;
            REQUIRE(c.ns == "CURRENCY");
            REQUIRE(c.fraction == 100);
        }
    }
    REQUIRE(found_usd);
}

// --- Transaction reading (empty fixtures) ---

TEST_CASE("get_transactions empty", "[book]") {
    auto result = Book::open(ACCOUNTS_DB);
    REQUIRE(result.is_ok());
    auto& book = result.unwrap();

    auto txns = book.get_transactions();
    REQUIRE(txns.empty());
}

TEST_CASE("get_splits_for_account empty", "[book]") {
    auto result = Book::open(ACCOUNTS_DB);
    REQUIRE(result.is_ok());
    auto& book = result.unwrap();

    auto splits = book.get_splits_for_account("a1000000000000000000000000000008");
    REQUIRE(splits.empty());
}

// --- Balance / reports (empty) ---

TEST_CASE("get_account_balance no transactions", "[book]") {
    auto result = Book::open(ACCOUNTS_DB);
    REQUIRE(result.is_ok());
    auto& book = result.unwrap();

    auto balance = book.get_account_balance("a1000000000000000000000000000008");
    REQUIRE_THAT(balance, WithinAbs(0.0, 1e-9));
}

TEST_CASE("trial_balance no transactions", "[book]") {
    auto result = Book::open(ACCOUNTS_DB);
    REQUIRE(result.is_ok());
    auto& book = result.unwrap();

    auto tb = book.trial_balance();
    REQUIRE(tb.empty());
}

// --- Write operations ---

TEST_CASE("write operations rejected on read-only", "[book]") {
    auto result = Book::open(ACCOUNTS_DB, true);
    REQUIRE(result.is_ok());
    auto& book = result.unwrap();

    auto r1 = book.create_account("Test", AccountType::BANK,
                                   "a1000000000000000000000000000007");
    REQUIRE(r1.is_err());
    REQUIRE(r1.unwrap_err().find("read-only") != std::string::npos);

    Transaction txn;
    txn.splits = {{}, {}};
    auto r2 = book.post_transaction(txn);
    REQUIRE(r2.is_err());
}

TEST_CASE("create_account", "[book][write]") {
    auto tmp = make_writable_copy(ACCOUNTS_DB);
    auto result = Book::open(tmp, false);
    REQUIRE(result.is_ok());
    auto& book = result.unwrap();

    // Create a new bank account under Current Assets
    auto create_result = book.create_account(
        "Test Checking",
        AccountType::BANK,
        "a1000000000000000000000000000007",  // Current Assets
        "Test account",
        "9999"
    );
    REQUIRE(create_result.is_ok());
    auto new_guid = create_result.unwrap();
    REQUIRE(validate_guid(new_guid));

    // Verify it was created
    auto acct = book.get_account(new_guid);
    REQUIRE(acct.has_value());
    REQUIRE(acct->name == "Test Checking");
    REQUIRE(acct->type == AccountType::BANK);
    REQUIRE(acct->code == "9999");
    REQUIRE(acct->parent_guid == "a1000000000000000000000000000007");

    // Account count increased
    auto accounts = book.get_accounts();
    REQUIRE(accounts.size() == 21);

    book.close();
    fs::remove(tmp);
}

TEST_CASE("create_account validation", "[book][write]") {
    auto tmp = make_writable_copy(ACCOUNTS_DB);
    auto result = Book::open(tmp, false);
    REQUIRE(result.is_ok());
    auto& book = result.unwrap();

    SECTION("empty name") {
        auto r = book.create_account("", AccountType::BANK,
                                      "a1000000000000000000000000000007");
        REQUIRE(r.is_err());
    }

    SECTION("name with colon") {
        auto r = book.create_account("Bad:Name", AccountType::BANK,
                                      "a1000000000000000000000000000007");
        REQUIRE(r.is_err());
    }

    SECTION("invalid parent guid") {
        auto r = book.create_account("Test", AccountType::BANK,
                                      "0000000000000000000000000000dead");
        REQUIRE(r.is_err());
    }

    book.close();
    fs::remove(tmp);
}

TEST_CASE("post_transaction", "[book][write]") {
    auto tmp = make_writable_copy(ACCOUNTS_DB);
    auto result = Book::open(tmp, false);
    REQUIRE(result.is_ok());
    auto& book = result.unwrap();

    // Get the USD commodity guid for transaction currency
    auto commodities = book.get_commodities();
    REQUIRE_FALSE(commodities.empty());
    std::string usd_guid = commodities[0].guid;

    Transaction txn;
    txn.currency_guid = usd_guid;
    txn.post_date = "2026-02-24 12:00:00";
    txn.description = "Test grocery purchase";

    Split s1;
    s1.account_guid = "a1000000000000000000000000000008";  // Checking
    s1.value = {-5000, 100};   // -$50.00
    s1.quantity = {-5000, 100};

    Split s2;
    s2.account_guid = "a1000000000000000000000000000017";  // Groceries
    s2.value = {5000, 100};    // +$50.00
    s2.quantity = {5000, 100};

    txn.splits = {s1, s2};

    auto post_result = book.post_transaction(txn);
    REQUIRE(post_result.is_ok());
    auto tx_guid = post_result.unwrap();
    REQUIRE(validate_guid(tx_guid));

    // Verify transaction exists
    auto loaded = book.get_transaction(tx_guid);
    REQUIRE(loaded.has_value());
    REQUIRE(loaded->description == "Test grocery purchase");
    REQUIRE(loaded->splits.size() == 2);

    // Verify balances
    auto checking_bal = book.get_account_balance("a1000000000000000000000000000008");
    REQUIRE_THAT(checking_bal, WithinAbs(-50.0, 1e-9));

    auto groceries_bal = book.get_account_balance("a1000000000000000000000000000017");
    REQUIRE_THAT(groceries_bal, WithinAbs(50.0, 1e-9));

    // Verify trial balance
    auto tb = book.trial_balance();
    REQUIRE(tb.size() >= 2);

    book.close();
    fs::remove(tmp);
}

TEST_CASE("post_transaction validation", "[book][write]") {
    auto tmp = make_writable_copy(ACCOUNTS_DB);
    auto result = Book::open(tmp, false);
    REQUIRE(result.is_ok());
    auto& book = result.unwrap();

    SECTION("fewer than 2 splits") {
        Transaction txn;
        txn.splits = {Split{}};
        auto r = book.post_transaction(txn);
        REQUIRE(r.is_err());
        REQUIRE(r.unwrap_err().find("2 splits") != std::string::npos);
    }

    SECTION("unbalanced splits") {
        Transaction txn;
        txn.currency_guid = "c1000000000000000000000000000001";
        txn.post_date = "2026-01-01 00:00:00";
        Split s1; s1.account_guid = "a1000000000000000000000000000008";
        s1.value = {5000, 100}; s1.quantity = {5000, 100};
        Split s2; s2.account_guid = "a1000000000000000000000000000017";
        s2.value = {3000, 100}; s2.quantity = {3000, 100};
        txn.splits = {s1, s2};
        auto r = book.post_transaction(txn);
        REQUIRE(r.is_err());
        REQUIRE(r.unwrap_err().find("balance") != std::string::npos);
    }

    SECTION("nonexistent account") {
        Transaction txn;
        txn.currency_guid = "c1000000000000000000000000000001";
        txn.post_date = "2026-01-01 00:00:00";
        Split s1; s1.account_guid = "0000000000000000000000000000dead";
        s1.value = {5000, 100}; s1.quantity = {5000, 100};
        Split s2; s2.account_guid = "a1000000000000000000000000000017";
        s2.value = {-5000, 100}; s2.quantity = {-5000, 100};
        txn.splits = {s1, s2};
        auto r = book.post_transaction(txn);
        REQUIRE(r.is_err());
        REQUIRE(r.unwrap_err().find("Account not found") != std::string::npos);
    }

    book.close();
    fs::remove(tmp);
}

TEST_CASE("delete_transaction", "[book][write]") {
    auto tmp = make_writable_copy(ACCOUNTS_DB);
    auto result = Book::open(tmp, false);
    REQUIRE(result.is_ok());
    auto& book = result.unwrap();

    auto commodities = book.get_commodities();
    std::string usd_guid = commodities[0].guid;

    // Post a transaction first
    Transaction txn;
    txn.currency_guid = usd_guid;
    txn.post_date = "2026-01-15 00:00:00";
    txn.description = "To be deleted";

    Split s1; s1.account_guid = "a1000000000000000000000000000008";
    s1.value = {-1000, 100}; s1.quantity = {-1000, 100};
    Split s2; s2.account_guid = "a1000000000000000000000000000017";
    s2.value = {1000, 100}; s2.quantity = {1000, 100};
    txn.splits = {s1, s2};

    auto post_result = book.post_transaction(txn);
    REQUIRE(post_result.is_ok());
    auto tx_guid = post_result.unwrap();

    // Delete it
    auto del_result = book.delete_transaction(tx_guid);
    REQUIRE(del_result.is_ok());

    // Verify it's gone
    auto loaded = book.get_transaction(tx_guid);
    REQUIRE_FALSE(loaded.has_value());

    // Balance should be zero again
    auto balance = book.get_account_balance("a1000000000000000000000000000008");
    REQUIRE_THAT(balance, WithinAbs(0.0, 1e-9));

    book.close();
    fs::remove(tmp);
}

TEST_CASE("void_transaction", "[book][write]") {
    auto tmp = make_writable_copy(ACCOUNTS_DB);
    auto result = Book::open(tmp, false);
    REQUIRE(result.is_ok());
    auto& book = result.unwrap();

    auto commodities = book.get_commodities();
    std::string usd_guid = commodities[0].guid;

    // Post a transaction
    Transaction txn;
    txn.currency_guid = usd_guid;
    txn.post_date = "2026-02-01 00:00:00";
    txn.description = "Payment to be voided";

    Split s1; s1.account_guid = "a1000000000000000000000000000008";
    s1.value = {-2500, 100}; s1.quantity = {-2500, 100};
    Split s2; s2.account_guid = "a1000000000000000000000000000019";  // Rent
    s2.value = {2500, 100}; s2.quantity = {2500, 100};
    txn.splits = {s1, s2};

    auto post_result = book.post_transaction(txn);
    REQUIRE(post_result.is_ok());
    auto tx_guid = post_result.unwrap();

    // Void it
    auto void_result = book.void_transaction(tx_guid, "Duplicate entry");
    REQUIRE(void_result.is_ok());

    // Transaction still exists but description is voided
    auto loaded = book.get_transaction(tx_guid);
    REQUIRE(loaded.has_value());
    REQUIRE(loaded->description == "Voided: Duplicate entry");

    // Splits should be zeroed
    for (const auto& s : loaded->splits) {
        REQUIRE(s.value.num == 0);
    }

    // Balance should be zero
    auto balance = book.get_account_balance("a1000000000000000000000000000008");
    REQUIRE_THAT(balance, WithinAbs(0.0, 1e-9));

    book.close();
    fs::remove(tmp);
}

// --- Date filtering ---

TEST_CASE("get_transactions date filtering", "[book][write]") {
    auto tmp = make_writable_copy(ACCOUNTS_DB);
    auto result = Book::open(tmp, false);
    REQUIRE(result.is_ok());
    auto& book = result.unwrap();

    auto commodities = book.get_commodities();
    std::string usd_guid = commodities[0].guid;

    // Post two transactions on different dates
    auto post = [&](const std::string& date, const std::string& desc, int amount) {
        Transaction txn;
        txn.currency_guid = usd_guid;
        txn.post_date = date;
        txn.description = desc;
        Split s1; s1.account_guid = "a1000000000000000000000000000008";
        s1.value = {-amount, 100}; s1.quantity = {-amount, 100};
        Split s2; s2.account_guid = "a1000000000000000000000000000017";
        s2.value = {amount, 100}; s2.quantity = {amount, 100};
        txn.splits = {s1, s2};
        return book.post_transaction(txn);
    };

    REQUIRE(post("2026-01-15 00:00:00", "January groceries", 5000).is_ok());
    REQUIRE(post("2026-02-15 00:00:00", "February groceries", 7500).is_ok());

    // All transactions
    auto all = book.get_transactions();
    REQUIRE(all.size() == 2);

    // From date only
    auto from_feb = book.get_transactions("2026-02-01 00:00:00");
    REQUIRE(from_feb.size() == 1);
    REQUIRE(from_feb[0].description == "February groceries");

    // To date only
    auto to_jan = book.get_transactions(std::nullopt, "2026-01-31 23:59:59");
    REQUIRE(to_jan.size() == 1);
    REQUIRE(to_jan[0].description == "January groceries");

    // Date range
    auto range = book.get_transactions("2026-01-01 00:00:00", "2026-12-31 23:59:59");
    REQUIRE(range.size() == 2);

    book.close();
    fs::remove(tmp);
}

// --- Balance with as_of date ---

TEST_CASE("get_account_balance as_of", "[book][write]") {
    auto tmp = make_writable_copy(ACCOUNTS_DB);
    auto result = Book::open(tmp, false);
    REQUIRE(result.is_ok());
    auto& book = result.unwrap();

    auto commodities = book.get_commodities();
    std::string usd_guid = commodities[0].guid;

    // Post two transactions
    {
        Transaction txn;
        txn.currency_guid = usd_guid;
        txn.post_date = "2026-01-15 00:00:00";
        txn.description = "Jan deposit";
        Split s1; s1.account_guid = "a1000000000000000000000000000015";  // Salary
        s1.value = {-300000, 100}; s1.quantity = {-300000, 100};
        Split s2; s2.account_guid = "a1000000000000000000000000000008";  // Checking
        s2.value = {300000, 100}; s2.quantity = {300000, 100};
        txn.splits = {s1, s2};
        REQUIRE(book.post_transaction(txn).is_ok());
    }
    {
        Transaction txn;
        txn.currency_guid = usd_guid;
        txn.post_date = "2026-02-15 00:00:00";
        txn.description = "Feb deposit";
        Split s1; s1.account_guid = "a1000000000000000000000000000015";
        s1.value = {-300000, 100}; s1.quantity = {-300000, 100};
        Split s2; s2.account_guid = "a1000000000000000000000000000008";
        s2.value = {300000, 100}; s2.quantity = {300000, 100};
        txn.splits = {s1, s2};
        REQUIRE(book.post_transaction(txn).is_ok());
    }

    // As of end of Jan: only first deposit
    auto jan_bal = book.get_account_balance("a1000000000000000000000000000008",
                                            "2026-01-31 23:59:59");
    REQUIRE_THAT(jan_bal, WithinAbs(3000.0, 1e-9));

    // As of end of Feb: both deposits
    auto feb_bal = book.get_account_balance("a1000000000000000000000000000008",
                                            "2026-02-28 23:59:59");
    REQUIRE_THAT(feb_bal, WithinAbs(6000.0, 1e-9));

    book.close();
    fs::remove(tmp);
}

// --- update_split ---

TEST_CASE("update_split recategorizes", "[book][write]") {
    auto tmp = make_writable_copy(ACCOUNTS_DB);
    auto result = Book::open(tmp, false);
    REQUIRE(result.is_ok());
    auto& book = result.unwrap();

    auto commodities = book.get_commodities();
    std::string usd_guid = commodities[0].guid;

    // Post a transaction: Checking -> Groceries
    Transaction txn;
    txn.currency_guid = usd_guid;
    txn.post_date = "2026-03-01 12:00:00";
    txn.description = "Recategorize test";

    Split s1;
    s1.account_guid = "a1000000000000000000000000000008"; // Checking
    s1.value = {-2000, 100};
    s1.quantity = {-2000, 100};

    Split s2;
    s2.account_guid = "a1000000000000000000000000000017"; // Groceries
    s2.value = {2000, 100};
    s2.quantity = {2000, 100};

    txn.splits = {s1, s2};
    auto post_r = book.post_transaction(txn);
    REQUIRE(post_r.is_ok());

    // Get the groceries split
    auto tx = book.get_transaction(post_r.unwrap());
    REQUIRE(tx.has_value());
    std::string groceries_split_guid;
    for (const auto& sp : tx->splits) {
        if (sp.account_guid == "a1000000000000000000000000000017")
            groceries_split_guid = sp.guid;
    }
    REQUIRE_FALSE(groceries_split_guid.empty());

    // Recategorize from Groceries to Dining (a1000000000000000000000000000018)
    auto upd = book.update_split(groceries_split_guid,
                                 "a1000000000000000000000000000018");
    REQUIRE(upd.is_ok());

    // Verify: Groceries balance = 0, Dining balance = $20
    auto groc_bal = book.get_account_balance("a1000000000000000000000000000017");
    REQUIRE_THAT(groc_bal, WithinAbs(0.0, 1e-9));

    auto dining_bal = book.get_account_balance("a1000000000000000000000000000018");
    REQUIRE_THAT(dining_bal, WithinAbs(20.0, 1e-9));

    book.close();
    fs::remove(tmp);
}

TEST_CASE("update_split rejects nonexistent split", "[book][write]") {
    auto tmp = make_writable_copy(ACCOUNTS_DB);
    auto result = Book::open(tmp, false);
    REQUIRE(result.is_ok());
    auto& book = result.unwrap();

    auto upd = book.update_split("nonexistent_split_guid",
                                 "a1000000000000000000000000000008");
    REQUIRE(upd.is_err());

    book.close();
    fs::remove(tmp);
}

TEST_CASE("update_split rejects nonexistent account", "[book][write]") {
    auto tmp = make_writable_copy(ACCOUNTS_DB);
    auto result = Book::open(tmp, false);
    REQUIRE(result.is_ok());
    auto& book = result.unwrap();

    auto upd = book.update_split("any_split_guid", "nonexistent_account_guid");
    REQUIRE(upd.is_err());

    book.close();
    fs::remove(tmp);
}
