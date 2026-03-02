#include <catch2/catch_test_macros.hpp>
#include <catch2/matchers/catch_matchers_floating_point.hpp>
#include "gnucash/agent.h"
#include "gnucash/agent_state.h"
#include "gnucash/book.h"
#include "gnucash/guid.h"
#include "gnucash/audit.h"
#include <filesystem>
#include <cstdio>

using namespace gnucash;
using namespace gnucash::agent;
using Catch::Matchers::WithinAbs;
namespace fs = std::filesystem;

#ifndef FIXTURE_DIR
#error "FIXTURE_DIR must be defined"
#endif

static const std::string ACCOUNTS_DB = std::string(FIXTURE_DIR) + "/with-accounts.gnucash";

// Helper: copy fixture to writable temp file
static std::string make_writable_copy(const std::string& src) {
    auto tmp = fs::temp_directory_path() / ("gnucash_agent_test_" + generate_guid() + ".gnucash");
    fs::copy_file(src, tmp, fs::copy_options::overwrite_existing);
    return tmp.string();
}

// Helper: get USD commodity guid from book
static std::string get_usd_guid(Book& book) {
    auto commodities = book.get_commodities();
    return commodities[0].guid;
}

// Well-known account GUIDs from with-accounts fixture
static const char* CHECKING  = "a1000000000000000000000000000008";
static const char* SALARY    = "a1000000000000000000000000000015";
static const char* GROCERIES = "a1000000000000000000000000000017";
static const char* UTILITIES = "a1000000000000000000000000000018";
static const char* RENT      = "a1000000000000000000000000000019";
static const char* TRANSPORT = "a1000000000000000000000000000020";
static const char* CREDIT_CARD = "a1000000000000000000000000000011";

// Helper: post a transaction with description
static std::string post_txn(Book& book, const std::string& desc,
                             const std::string& debit_acct, const std::string& credit_acct,
                             int64_t amount_cents, const std::string& date) {
    auto usd = get_usd_guid(book);
    Transaction txn;
    txn.currency_guid = usd;
    txn.post_date = date;
    txn.description = desc;

    Split s1;
    s1.account_guid = debit_acct;
    s1.value = {amount_cents, 100};
    s1.quantity = {amount_cents, 100};

    Split s2;
    s2.account_guid = credit_acct;
    s2.value = {-amount_cents, 100};
    s2.quantity = {-amount_cents, 100};

    txn.splits = {s1, s2};
    auto result = book.post_transaction(txn);
    REQUIRE(result.is_ok());
    return result.unwrap();
}

// Helper: seed a book with realistic transactions
static void seed_transactions(Book& book) {
    // Income
    post_txn(book, "ACME Corp Salary",    CHECKING, SALARY,    500000, "2026-01-15 00:00:00");
    post_txn(book, "ACME Corp Salary",    CHECKING, SALARY,    500000, "2026-02-15 00:00:00");

    // Groceries (various vendors)
    post_txn(book, "TRADER JOES #123",    GROCERIES, CHECKING,  8500, "2026-01-10 00:00:00");
    post_txn(book, "WHOLE FOODS MARKET",  GROCERIES, CHECKING, 12000, "2026-01-17 00:00:00");
    post_txn(book, "TRADER JOES #123",    GROCERIES, CHECKING,  9200, "2026-02-07 00:00:00");
    post_txn(book, "KROGER GROCERY",      GROCERIES, CHECKING,  6500, "2026-02-14 00:00:00");

    // Utilities
    post_txn(book, "ELECTRIC CO PAYMENT", UTILITIES, CHECKING, 15000, "2026-01-05 00:00:00");
    post_txn(book, "GAS UTILITY BILL",    UTILITIES, CHECKING,  8500, "2026-01-20 00:00:00");

    // Rent
    post_txn(book, "RENT PAYMENT",        RENT, CHECKING,     200000, "2026-01-01 00:00:00");
    post_txn(book, "RENT PAYMENT",        RENT, CHECKING,     200000, "2026-02-01 00:00:00");

    // Transport
    post_txn(book, "UBER RIDE",           TRANSPORT, CHECKING,   2500, "2026-01-12 00:00:00");
    post_txn(book, "LYFT RIDE SHARE",     TRANSPORT, CHECKING,   1800, "2026-01-25 00:00:00");

    // A large anomalous AWS charge (will trigger anomaly detection)
    post_txn(book, "AWS SERVICES CHARGE", UTILITIES, CHECKING, 250000, "2026-02-10 00:00:00");
}


// ========================================================================
// Vendor Pattern Matching Tests
// ========================================================================

TEST_CASE("match_vendors: case-insensitive substring match", "[agent][vendor]") {
    std::vector<Transaction> txns;
    Transaction t1;
    t1.guid = "tx001"; t1.description = "TRADER JOES #123";
    Transaction t2;
    t2.guid = "tx002"; t2.description = "WHOLE FOODS MARKET";
    Transaction t3;
    t3.guid = "tx003"; t3.description = "Random payment";
    txns = {t1, t2, t3};

    std::vector<VendorPattern> patterns = {
        {"trader joes", "Expenses:Groceries", 0.95},
        {"whole foods", "Expenses:Groceries", 0.90},
    };

    auto matches = match_vendors(txns, patterns);
    REQUIRE(matches.size() == 2);
    REQUIRE(matches[0].transaction_guid == "tx001");
    REQUIRE(matches[0].category == "Expenses:Groceries");
    REQUIRE(matches[0].confidence == 0.95);
    REQUIRE(matches[1].transaction_guid == "tx002");
}

TEST_CASE("match_vendors: first match wins", "[agent][vendor]") {
    Transaction t;
    t.guid = "tx001"; t.description = "TRADER JOES ORGANIC";

    std::vector<VendorPattern> patterns = {
        {"trader joes", "Expenses:Groceries", 0.95},
        {"organic", "Expenses:Health", 0.80},
    };

    auto matches = match_vendors({t}, patterns);
    REQUIRE(matches.size() == 1);
    REQUIRE(matches[0].matched_pattern == "trader joes");
    REQUIRE(matches[0].category == "Expenses:Groceries");
}

TEST_CASE("match_vendors: no match returns empty", "[agent][vendor]") {
    Transaction t;
    t.guid = "tx001"; t.description = "Unknown vendor xyz";

    std::vector<VendorPattern> patterns = {
        {"trader joes", "Expenses:Groceries", 0.95},
    };

    auto matches = match_vendors({t}, patterns);
    REQUIRE(matches.empty());
}

TEST_CASE("match_vendors: empty patterns returns empty", "[agent][vendor]") {
    Transaction t;
    t.guid = "tx001"; t.description = "TRADER JOES";

    auto matches = match_vendors({t}, {});
    REQUIRE(matches.empty());
}

TEST_CASE("parse_vendor_patterns: from JSON", "[agent][vendor]") {
    json config = {
        {"vendor_patterns", {
            {{"pattern", "TRADER JOES"}, {"category", "Expenses:Groceries"}, {"confidence", 0.95}},
            {{"pattern", "UBER"}, {"category", "Expenses:Transport"}, {"confidence", 0.85}},
            {{"pattern", ""}, {"category", "Empty"}},  // Should be skipped
        }}
    };

    auto patterns = parse_vendor_patterns(config);
    REQUIRE(patterns.size() == 2);
    REQUIRE(patterns[0].pattern == "TRADER JOES");
    REQUIRE(patterns[1].category == "Expenses:Transport");
}

TEST_CASE("parse_vendor_patterns: missing key returns empty", "[agent][vendor]") {
    json config = {{"other", "data"}};
    auto patterns = parse_vendor_patterns(config);
    REQUIRE(patterns.empty());
}

// ========================================================================
// Agent State DB Tests
// ========================================================================

TEST_CASE("AgentStateDB: open creates database", "[agent][state]") {
    auto tmp = fs::temp_directory_path() / ("state_test_" + generate_guid() + ".gnucash");
    std::string tmp_str = tmp.string();

    auto result = AgentStateDB::open(tmp_str, "test-agent");
    REQUIRE(result.is_ok());

    auto& db = result.unwrap();
    std::string expected_path = tmp_str + ".agent.test-agent.db";
    REQUIRE(db.db_path() == expected_path);
    REQUIRE(fs::exists(expected_path));

    // Cleanup
    fs::remove(expected_path);
}

TEST_CASE("AgentStateDB: set and get round-trip", "[agent][state]") {
    auto tmp = fs::temp_directory_path() / ("state_test_" + generate_guid() + ".gnucash");
    auto result = AgentStateDB::open(tmp.string(), "test");
    REQUIRE(result.is_ok());
    auto& db = result.unwrap();

    auto set_result = db.set("last_run", "2026-03-01T12:00:00Z");
    REQUIRE(set_result.is_ok());

    auto get_result = db.get("last_run");
    REQUIRE(get_result.is_ok());
    REQUIRE(get_result.unwrap().has_value());
    REQUIRE(get_result.unwrap().value() == "2026-03-01T12:00:00Z");

    // Missing key
    auto missing = db.get("nonexistent");
    REQUIRE(missing.is_ok());
    REQUIRE_FALSE(missing.unwrap().has_value());

    fs::remove(db.db_path());
}

TEST_CASE("AgentStateDB: set overwrites existing key", "[agent][state]") {
    auto tmp = fs::temp_directory_path() / ("state_test_" + generate_guid() + ".gnucash");
    auto result = AgentStateDB::open(tmp.string(), "test");
    REQUIRE(result.is_ok());
    auto& db = result.unwrap();

    db.set("key", "value1");
    db.set("key", "value2");

    auto val = db.get("key");
    REQUIRE(val.is_ok());
    REQUIRE(val.unwrap().value() == "value2");

    fs::remove(db.db_path());
}

TEST_CASE("AgentStateDB: review queue", "[agent][state]") {
    auto tmp = fs::temp_directory_path() / ("state_test_" + generate_guid() + ".gnucash");
    auto result = AgentStateDB::open(tmp.string(), "categorizer");
    REQUIRE(result.is_ok());
    auto& db = result.unwrap();

    // Enqueue reviews
    auto r1 = db.enqueue_review("tx001", "Expenses:Groceries", 0.75, "Vendor match: TRADER JOES");
    REQUIRE(r1.is_ok());
    auto id1 = r1.unwrap();

    auto r2 = db.enqueue_review("tx002", "Expenses:Transport", 0.60, "Vendor match: UBER");
    REQUIRE(r2.is_ok());
    auto id2 = r2.unwrap();

    REQUIRE(id1 != id2);

    // Fetch pending reviews
    auto pending = db.pending_reviews();
    REQUIRE(pending.is_ok());
    REQUIRE(pending.unwrap().size() == 2);

    // Both should be present (order depends on insertion within same second)
    bool has_tx001 = false, has_tx002 = false;
    for (const auto& item : pending.unwrap()) {
        if (item.transaction_guid == "tx001") has_tx001 = true;
        if (item.transaction_guid == "tx002") has_tx002 = true;
    }
    REQUIRE(has_tx001);
    REQUIRE(has_tx002);

    // Approve one
    auto approve = db.update_review(id1, ReviewStatus::APPROVED);
    REQUIRE(approve.is_ok());

    // Now only 1 pending
    auto still_pending = db.pending_reviews();
    REQUIRE(still_pending.is_ok());
    REQUIRE(still_pending.unwrap().size() == 1);
    REQUIRE(still_pending.unwrap()[0].transaction_guid == "tx002");

    fs::remove(db.db_path());
}

TEST_CASE("AgentStateDB: review queue limit", "[agent][state]") {
    auto tmp = fs::temp_directory_path() / ("state_test_" + generate_guid() + ".gnucash");
    auto result = AgentStateDB::open(tmp.string(), "test");
    REQUIRE(result.is_ok());
    auto& db = result.unwrap();

    for (int i = 0; i < 10; i++) {
        db.enqueue_review("tx" + std::to_string(i), "cat", 0.5, "reason");
    }

    auto limited = db.pending_reviews(3);
    REQUIRE(limited.is_ok());
    REQUIRE(limited.unwrap().size() == 3);

    fs::remove(db.db_path());
}

// ========================================================================
// spend-monitor Tests
// ========================================================================

TEST_CASE("run_spend_monitor: produces report", "[agent][spend-monitor]") {
    auto tmp = make_writable_copy(ACCOUNTS_DB);
    auto book_result = Book::open(tmp, false);
    REQUIRE(book_result.is_ok());
    auto& book = book_result.unwrap();

    seed_transactions(book);

    dhall::AgentConfig config;
    config.name = "spend-monitor";

    auto state_result = AgentStateDB::open(tmp, "spend-monitor");
    REQUIRE(state_result.is_ok());
    auto& state = state_result.unwrap();

    auto result = run_spend_monitor(book, config, state);
    REQUIRE(result.is_ok());

    auto& agent_result = result.unwrap();
    REQUIRE(agent_result.agent_name == "spend-monitor");
    REQUIRE(agent_result.records_processed == 13);  // 13 transactions seeded
    REQUIRE(agent_result.actions_taken == 0);        // Read-only agent

    // Report structure
    auto& report = agent_result.report;
    REQUIRE(report.contains("transactions_processed"));
    REQUIRE(report.contains("categorized"));
    REQUIRE(report.contains("uncategorized"));
    REQUIRE(report.contains("category_totals"));
    REQUIRE(report.contains("anomalies"));

    REQUIRE(report["transactions_processed"].get<int>() == 13);

    // Category totals should have expense accounts
    auto& cat_totals = report["category_totals"];
    REQUIRE_FALSE(cat_totals.empty());

    book.close();
    fs::remove(tmp);
    fs::remove(state.db_path());
}

// ========================================================================
// report-generator Tests
// ========================================================================

TEST_CASE("run_report_generator: produces financial statements", "[agent][report]") {
    auto tmp = make_writable_copy(ACCOUNTS_DB);
    auto book_result = Book::open(tmp, false);
    REQUIRE(book_result.is_ok());
    auto& book = book_result.unwrap();

    seed_transactions(book);

    dhall::AgentConfig config;
    config.name = "report-generator";

    auto state_result = AgentStateDB::open(tmp, "report-generator");
    REQUIRE(state_result.is_ok());
    auto& state = state_result.unwrap();

    auto result = run_report_generator(book, config, state);
    REQUIRE(result.is_ok());

    auto& agent_result = result.unwrap();
    REQUIRE(agent_result.agent_name == "report-generator");
    REQUIRE(agent_result.actions_taken == 0);  // Read-only

    auto& report = agent_result.report;
    REQUIRE(report.contains("trial_balance"));
    REQUIRE(report.contains("trial_balance_check"));
    REQUIRE(report.contains("income_statement"));
    REQUIRE(report.contains("balance_sheet"));

    // Trial balance should sum to zero (balanced)
    auto& tb_check = report["trial_balance_check"];
    REQUIRE(tb_check["balanced"].get<bool>());

    // Income statement
    auto& income_stmt = report["income_statement"];
    double total_income = income_stmt["total_income"].get<double>();
    double total_expenses = income_stmt["total_expenses"].get<double>();
    double net_income = income_stmt["net_income"].get<double>();

    // We posted $10,000 income ($5000 x 2 salary)
    REQUIRE_THAT(total_income, WithinAbs(10000.0, 0.01));

    // Expenses: groceries(85+120+92+65=362) + utilities(150+85+2500=2735) + rent(2000+2000=4000) + transport(25+18=43)
    // Total expenses = 362 + 2735 + 4000 + 43 = 7140
    REQUIRE_THAT(total_expenses, WithinAbs(7140.0, 0.01));

    // Net income = 10000 - 7140 = 2860
    REQUIRE_THAT(net_income, WithinAbs(2860.0, 0.01));

    // Balance sheet
    auto& bs = report["balance_sheet"];
    REQUIRE(bs.contains("total_assets"));
    REQUIRE(bs.contains("total_liabilities"));
    REQUIRE(bs.contains("total_equity"));

    book.close();
    fs::remove(tmp);
    fs::remove(state.db_path());
}

TEST_CASE("run_report_generator: trial balance entries have required fields", "[agent][report]") {
    auto tmp = make_writable_copy(ACCOUNTS_DB);
    auto book_result = Book::open(tmp, false);
    REQUIRE(book_result.is_ok());
    auto& book = book_result.unwrap();

    seed_transactions(book);

    dhall::AgentConfig config;
    config.name = "report-generator";

    auto state_result = AgentStateDB::open(tmp, "report-generator");
    REQUIRE(state_result.is_ok());
    auto& state = state_result.unwrap();

    auto result = run_report_generator(book, config, state);
    REQUIRE(result.is_ok());

    auto& tb = result.unwrap().report["trial_balance"];
    REQUIRE(tb.is_array());
    REQUIRE(tb.size() > 0);

    for (const auto& entry : tb) {
        REQUIRE(entry.contains("account"));
        REQUIRE(entry.contains("type"));
        REQUIRE(entry.contains("debit"));
        REQUIRE(entry.contains("credit"));
    }

    book.close();
    fs::remove(tmp);
    fs::remove(state.db_path());
}

// ========================================================================
// transaction-categorizer Tests
// ========================================================================

TEST_CASE("run_categorizer: no uncategorized returns zeros", "[agent][categorizer]") {
    auto tmp = make_writable_copy(ACCOUNTS_DB);
    auto book_result = Book::open(tmp, false);
    REQUIRE(book_result.is_ok());
    auto& book = book_result.unwrap();

    // Seed only properly categorized transactions (no Imbalance accounts)
    seed_transactions(book);

    dhall::AgentConfig config;
    config.name = "transaction-categorizer";

    auto state_result = AgentStateDB::open(tmp, "transaction-categorizer");
    REQUIRE(state_result.is_ok());
    auto& state = state_result.unwrap();

    auto result = run_categorizer(book, config, state);
    REQUIRE(result.is_ok());

    auto& agent_result = result.unwrap();
    REQUIRE(agent_result.agent_name == "transaction-categorizer");

    // No uncategorized transactions since all are properly categorized
    auto& report = agent_result.report;
    REQUIRE(report["total_uncategorized"].get<int>() == 0);
    REQUIRE(report["auto_categorized"].get<int>() == 0);
    REQUIRE(report["queued_for_review"].get<int>() == 0);

    book.close();
    fs::remove(tmp);
    fs::remove(state.db_path());
}

// ========================================================================
// Agent Dispatch Tests
// ========================================================================

TEST_CASE("run_agent: dispatch by name", "[agent][dispatch]") {
    auto tmp = make_writable_copy(ACCOUNTS_DB);
    auto book_result = Book::open(tmp, false);
    REQUIRE(book_result.is_ok());
    auto& book = book_result.unwrap();

    seed_transactions(book);

    dhall::AgentConfig config;
    config.name = "spend-monitor";

    auto state_result = AgentStateDB::open(tmp, "spend-monitor");
    REQUIRE(state_result.is_ok());
    auto& state = state_result.unwrap();

    auto result = run_agent("spend-monitor", book, config, state);
    REQUIRE(result.is_ok());
    REQUIRE(result.unwrap().agent_name == "spend-monitor");

    book.close();
    fs::remove(tmp);
    fs::remove(state.db_path());
}

TEST_CASE("run_agent: unknown agent returns error", "[agent][dispatch]") {
    auto tmp = make_writable_copy(ACCOUNTS_DB);
    auto book_result = Book::open(tmp, false);
    REQUIRE(book_result.is_ok());
    auto& book = book_result.unwrap();

    dhall::AgentConfig config;
    config.name = "nonexistent";

    auto state_result = AgentStateDB::open(tmp, "nonexistent");
    REQUIRE(state_result.is_ok());
    auto& state = state_result.unwrap();

    auto result = run_agent("nonexistent", book, config, state);
    REQUIRE(result.is_err());
    REQUIRE(result.unwrap_err().find("Unknown agent") != std::string::npos);

    book.close();
    fs::remove(tmp);
    fs::remove(state.db_path());
}

TEST_CASE("run_agent: report-generator dispatch", "[agent][dispatch]") {
    auto tmp = make_writable_copy(ACCOUNTS_DB);
    auto book_result = Book::open(tmp, false);
    REQUIRE(book_result.is_ok());
    auto& book = book_result.unwrap();

    seed_transactions(book);

    dhall::AgentConfig config;
    config.name = "report-generator";

    auto state_result = AgentStateDB::open(tmp, "report-generator");
    REQUIRE(state_result.is_ok());
    auto& state = state_result.unwrap();

    auto result = run_agent("report-generator", book, config, state);
    REQUIRE(result.is_ok());
    REQUIRE(result.unwrap().agent_name == "report-generator");
    REQUIRE(result.unwrap().report.contains("trial_balance"));

    book.close();
    fs::remove(tmp);
    fs::remove(state.db_path());
}

TEST_CASE("run_agent: transaction-categorizer dispatch", "[agent][dispatch]") {
    auto tmp = make_writable_copy(ACCOUNTS_DB);
    auto book_result = Book::open(tmp, false);
    REQUIRE(book_result.is_ok());
    auto& book = book_result.unwrap();

    dhall::AgentConfig config;
    config.name = "transaction-categorizer";

    auto state_result = AgentStateDB::open(tmp, "transaction-categorizer");
    REQUIRE(state_result.is_ok());
    auto& state = state_result.unwrap();

    auto result = run_agent("transaction-categorizer", book, config, state);
    REQUIRE(result.is_ok());
    REQUIRE(result.unwrap().agent_name == "transaction-categorizer");

    book.close();
    fs::remove(tmp);
    fs::remove(state.db_path());
}

// ========================================================================
// spend-monitor with vendor patterns
// ========================================================================

TEST_CASE("run_spend_monitor: state persists across runs", "[agent][spend-monitor]") {
    auto tmp = make_writable_copy(ACCOUNTS_DB);
    auto book_result = Book::open(tmp, false);
    REQUIRE(book_result.is_ok());
    auto& book = book_result.unwrap();

    seed_transactions(book);

    dhall::AgentConfig config;
    config.name = "spend-monitor";

    auto state_result = AgentStateDB::open(tmp, "spend-monitor");
    REQUIRE(state_result.is_ok());
    auto& state = state_result.unwrap();

    // First run
    auto r1 = run_spend_monitor(book, config, state);
    REQUIRE(r1.is_ok());

    // State should have last_check_timestamp
    auto ts = state.get("last_check_timestamp");
    REQUIRE(ts.is_ok());
    REQUIRE(ts.unwrap().has_value());

    // State should have last_report
    auto rpt = state.get("last_report");
    REQUIRE(rpt.is_ok());
    REQUIRE(rpt.unwrap().has_value());

    book.close();
    fs::remove(tmp);
    fs::remove(state.db_path());
}
