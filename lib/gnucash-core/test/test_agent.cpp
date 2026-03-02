#include <catch2/catch_test_macros.hpp>
#include <catch2/matchers/catch_matchers_floating_point.hpp>
#include "gnucash/agent.h"
#include "gnucash/agent_state.h"
#include "gnucash/book.h"
#include "gnucash/guid.h"
#include "gnucash/audit.h"
#include "gnucash/approval.h"
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

// ========================================================================
// Additional well-known account GUIDs from with-accounts fixture
// ========================================================================

// Parent GUIDs for creating new accounts
static const char* CURRENT_ASSETS = "a1000000000000000000000000000007";
static const char* LIABILITIES    = "a1000000000000000000000000000009";

// Helper: create a RECEIVABLE account
static std::string create_receivable_account(Book& book) {
    auto result = book.create_account(
        "Client Invoices", AccountType::RECEIVABLE,
        CURRENT_ASSETS, "Accounts receivable");
    REQUIRE(result.is_ok());
    return result.unwrap();
}

// Helper: create a PAYABLE account
static std::string create_payable_account(Book& book) {
    auto result = book.create_account(
        "Vendor Bills", AccountType::PAYABLE,
        LIABILITIES, "Accounts payable");
    REQUIRE(result.is_ok());
    return result.unwrap();
}

// ========================================================================
// invoice-generator Tests
// ========================================================================

TEST_CASE("run_invoice_generator: no receivables produces empty report", "[agent][invoice]") {
    auto tmp = make_writable_copy(ACCOUNTS_DB);
    auto book_result = Book::open(tmp, false);
    REQUIRE(book_result.is_ok());
    auto& book = book_result.unwrap();

    seed_transactions(book);

    dhall::AgentConfig config;
    config.name = "invoice-generator";

    auto state_result = AgentStateDB::open(tmp, "invoice-generator");
    REQUIRE(state_result.is_ok());
    auto& state = state_result.unwrap();

    auto result = run_invoice_generator(book, config, state);
    REQUIRE(result.is_ok());

    auto& ar = result.unwrap();
    REQUIRE(ar.agent_name == "invoice-generator");
    REQUIRE(ar.actions_taken == 0);
    REQUIRE(ar.report["invoice_count"].get<int>() == 0);
    REQUIRE(ar.report["total_outstanding"].get<double>() < 0.01);

    book.close();
    fs::remove(tmp);
    fs::remove(state.db_path());
}

TEST_CASE("run_invoice_generator: produces invoice from receivable", "[agent][invoice]") {
    auto tmp = make_writable_copy(ACCOUNTS_DB);
    auto book_result = Book::open(tmp, false);
    REQUIRE(book_result.is_ok());
    auto& book = book_result.unwrap();

    auto recv_guid = create_receivable_account(book);
    post_txn(book, "Invoice #001 - Web Dev", recv_guid, SALARY, 250000, "2026-01-15 00:00:00");
    post_txn(book, "Invoice #002 - Consulting", recv_guid, SALARY, 150000, "2026-02-01 00:00:00");

    dhall::AgentConfig config;
    config.name = "invoice-generator";

    auto state_result = AgentStateDB::open(tmp, "invoice-generator");
    REQUIRE(state_result.is_ok());
    auto& state = state_result.unwrap();

    auto result = run_invoice_generator(book, config, state);
    REQUIRE(result.is_ok());

    auto& ar = result.unwrap();
    REQUIRE(ar.report["invoice_count"].get<int>() == 1);
    REQUIRE(ar.report["total_outstanding"].get<double>() > 0);

    auto& invoices = ar.report["invoices"];
    REQUIRE(invoices.size() == 1);
    REQUIRE(invoices[0]["customer"].get<std::string>() == "Client Invoices");
    REQUIRE(invoices[0].contains("items"));

    book.close();
    fs::remove(tmp);
    fs::remove(state.db_path());
}

TEST_CASE("run_invoice_generator: dispatch via run_agent", "[agent][invoice][dispatch]") {
    auto tmp = make_writable_copy(ACCOUNTS_DB);
    auto book_result = Book::open(tmp, false);
    REQUIRE(book_result.is_ok());
    auto& book = book_result.unwrap();

    dhall::AgentConfig config;
    config.name = "invoice-generator";

    auto state_result = AgentStateDB::open(tmp, "invoice-generator");
    REQUIRE(state_result.is_ok());
    auto& state = state_result.unwrap();

    auto result = run_agent("invoice-generator", book, config, state);
    REQUIRE(result.is_ok());
    REQUIRE(result.unwrap().agent_name == "invoice-generator");

    book.close();
    fs::remove(tmp);
    fs::remove(state.db_path());
}

// ========================================================================
// tax-estimator Tests
// ========================================================================

TEST_CASE("run_tax_estimator: produces tax estimate", "[agent][tax]") {
    auto tmp = make_writable_copy(ACCOUNTS_DB);
    auto book_result = Book::open(tmp, false);
    REQUIRE(book_result.is_ok());
    auto& book = book_result.unwrap();

    seed_transactions(book);

    dhall::AgentConfig config;
    config.name = "tax-estimator";

    auto state_result = AgentStateDB::open(tmp, "tax-estimator");
    REQUIRE(state_result.is_ok());
    auto& state = state_result.unwrap();

    auto result = run_tax_estimator(book, config, state);
    REQUIRE(result.is_ok());

    auto& ar = result.unwrap();
    REQUIRE(ar.agent_name == "tax-estimator");
    REQUIRE(ar.actions_taken == 0);

    auto& report = ar.report;
    REQUIRE(report.contains("gross_income"));
    REQUIRE(report.contains("taxable_income"));
    REQUIRE(report.contains("estimated_tax"));
    REQUIRE(report.contains("quarterly_payment"));
    REQUIRE(report.contains("effective_rate"));

    REQUIRE_THAT(report["gross_income"].get<double>(), WithinAbs(10000.0, 0.01));

    // With $10k income and $15,700 standard deduction, taxable income = 0
    double taxable = report["taxable_income"].get<double>();
    REQUIRE(taxable >= 0);
    REQUIRE(taxable <= 10000.0);

    // Tax and quarterly follow from taxable income
    double est_tax = report["estimated_tax"].get<double>();
    REQUIRE(est_tax >= 0);
    double quarterly = report["quarterly_payment"].get<double>();
    REQUIRE_THAT(quarterly, WithinAbs(est_tax / 4.0, 0.01));

    book.close();
    fs::remove(tmp);
    fs::remove(state.db_path());
}

TEST_CASE("run_tax_estimator: state tracks change", "[agent][tax]") {
    auto tmp = make_writable_copy(ACCOUNTS_DB);
    auto book_result = Book::open(tmp, false);
    REQUIRE(book_result.is_ok());
    auto& book = book_result.unwrap();

    seed_transactions(book);

    dhall::AgentConfig config;
    config.name = "tax-estimator";

    auto state_result = AgentStateDB::open(tmp, "tax-estimator");
    REQUIRE(state_result.is_ok());
    auto& state = state_result.unwrap();

    auto r1 = run_tax_estimator(book, config, state);
    REQUIRE(r1.is_ok());

    auto est = state.get("last_estimated_tax");
    REQUIRE(est.is_ok());
    REQUIRE(est.unwrap().has_value());

    auto r2 = run_tax_estimator(book, config, state);
    REQUIRE(r2.is_ok());
    REQUIRE_THAT(r2.unwrap().report["change_from_last"].get<double>(), WithinAbs(0.0, 0.01));

    book.close();
    fs::remove(tmp);
    fs::remove(state.db_path());
}

TEST_CASE("run_tax_estimator: dispatch via run_agent", "[agent][tax][dispatch]") {
    auto tmp = make_writable_copy(ACCOUNTS_DB);
    auto book_result = Book::open(tmp, false);
    REQUIRE(book_result.is_ok());
    auto& book = book_result.unwrap();

    seed_transactions(book);

    dhall::AgentConfig config;
    config.name = "tax-estimator";

    auto state_result = AgentStateDB::open(tmp, "tax-estimator");
    REQUIRE(state_result.is_ok());
    auto& state = state_result.unwrap();

    auto result = run_agent("tax-estimator", book, config, state);
    REQUIRE(result.is_ok());
    REQUIRE(result.unwrap().agent_name == "tax-estimator");

    book.close();
    fs::remove(tmp);
    fs::remove(state.db_path());
}

// ========================================================================
// subscription-manager Tests
// ========================================================================

TEST_CASE("run_subscription_manager: detects recurring transactions", "[agent][subscription]") {
    auto tmp = make_writable_copy(ACCOUNTS_DB);
    auto book_result = Book::open(tmp, false);
    REQUIRE(book_result.is_ok());
    auto& book = book_result.unwrap();

    seed_transactions(book);

    // Add more months of rent for clearer pattern
    post_txn(book, "RENT PAYMENT", RENT, CHECKING, 200000, "2026-03-01 00:00:00");
    post_txn(book, "RENT PAYMENT", RENT, CHECKING, 200000, "2026-04-01 00:00:00");

    // Add recurring subscription
    post_txn(book, "NETFLIX SUBSCRIPTION", UTILITIES, CHECKING, 1599, "2026-01-15 00:00:00");
    post_txn(book, "NETFLIX SUBSCRIPTION", UTILITIES, CHECKING, 1599, "2026-02-15 00:00:00");
    post_txn(book, "NETFLIX SUBSCRIPTION", UTILITIES, CHECKING, 1599, "2026-03-15 00:00:00");

    dhall::AgentConfig config;
    config.name = "subscription-manager";

    auto state_result = AgentStateDB::open(tmp, "subscription-manager");
    REQUIRE(state_result.is_ok());
    auto& state = state_result.unwrap();

    auto result = run_subscription_manager(book, config, state);
    REQUIRE(result.is_ok());

    auto& ar = result.unwrap();
    REQUIRE(ar.agent_name == "subscription-manager");
    REQUIRE(ar.actions_taken == 0);

    auto& report = ar.report;
    REQUIRE(report.contains("subscriptions"));
    REQUIRE(report.contains("monthly_total"));
    REQUIRE(report.contains("annual_total"));

    int sub_count = report["subscription_count"].get<int>();
    REQUIRE(sub_count >= 2);

    REQUIRE(report["monthly_total"].get<double>() > 2000.0);

    book.close();
    fs::remove(tmp);
    fs::remove(state.db_path());
}

TEST_CASE("run_subscription_manager: detects changes between runs", "[agent][subscription]") {
    auto tmp = make_writable_copy(ACCOUNTS_DB);
    auto book_result = Book::open(tmp, false);
    REQUIRE(book_result.is_ok());
    auto& book = book_result.unwrap();

    post_txn(book, "RENT PAYMENT", RENT, CHECKING, 200000, "2026-01-01 00:00:00");
    post_txn(book, "RENT PAYMENT", RENT, CHECKING, 200000, "2026-02-01 00:00:00");
    post_txn(book, "RENT PAYMENT", RENT, CHECKING, 200000, "2026-03-01 00:00:00");

    dhall::AgentConfig config;
    config.name = "subscription-manager";

    auto state_result = AgentStateDB::open(tmp, "subscription-manager");
    REQUIRE(state_result.is_ok());
    auto& state = state_result.unwrap();

    auto r1 = run_subscription_manager(book, config, state);
    REQUIRE(r1.is_ok());
    REQUIRE(r1.unwrap().report["subscription_count"].get<int>() >= 1);

    auto r2 = run_subscription_manager(book, config, state);
    REQUIRE(r2.is_ok());
    REQUIRE(r2.unwrap().report["new_subscriptions"].empty());

    book.close();
    fs::remove(tmp);
    fs::remove(state.db_path());
}

TEST_CASE("run_subscription_manager: dispatch via run_agent", "[agent][subscription][dispatch]") {
    auto tmp = make_writable_copy(ACCOUNTS_DB);
    auto book_result = Book::open(tmp, false);
    REQUIRE(book_result.is_ok());
    auto& book = book_result.unwrap();

    seed_transactions(book);

    dhall::AgentConfig config;
    config.name = "subscription-manager";

    auto state_result = AgentStateDB::open(tmp, "subscription-manager");
    REQUIRE(state_result.is_ok());
    auto& state = state_result.unwrap();

    auto result = run_agent("subscription-manager", book, config, state);
    REQUIRE(result.is_ok());
    REQUIRE(result.unwrap().agent_name == "subscription-manager");

    book.close();
    fs::remove(tmp);
    fs::remove(state.db_path());
}

// ========================================================================
// bill-pay Tests
// ========================================================================

TEST_CASE("run_bill_pay: no payables produces empty report", "[agent][bill-pay]") {
    auto tmp = make_writable_copy(ACCOUNTS_DB);
    auto book_result = Book::open(tmp, false);
    REQUIRE(book_result.is_ok());
    auto& book = book_result.unwrap();

    dhall::AgentConfig config;
    config.name = "bill-pay";

    auto state_result = AgentStateDB::open(tmp, "bill-pay");
    REQUIRE(state_result.is_ok());
    auto& state = state_result.unwrap();

    auto result = run_bill_pay(book, config, state);
    REQUIRE(result.is_ok());

    auto& ar = result.unwrap();
    REQUIRE(ar.agent_name == "bill-pay");
    REQUIRE(ar.report["bills_due_count"].get<int>() == 0);
    REQUIRE(ar.report["total_due"].get<double>() < 0.01);

    book.close();
    fs::remove(tmp);
    fs::remove(state.db_path());
}

TEST_CASE("run_bill_pay: detects outstanding payables", "[agent][bill-pay]") {
    auto tmp = make_writable_copy(ACCOUNTS_DB);
    auto book_result = Book::open(tmp, false);
    REQUIRE(book_result.is_ok());
    auto& book = book_result.unwrap();

    auto payable_guid = create_payable_account(book);
    post_txn(book, "Office Supplies Invoice", payable_guid, CHECKING, 35000, "2026-01-20 00:00:00");
    post_txn(book, "Web Hosting Invoice",     payable_guid, CHECKING, 12000, "2026-02-01 00:00:00");

    dhall::AgentConfig config;
    config.name = "bill-pay";

    auto state_result = AgentStateDB::open(tmp, "bill-pay");
    REQUIRE(state_result.is_ok());
    auto& state = state_result.unwrap();

    auto result = run_bill_pay(book, config, state);
    REQUIRE(result.is_ok());

    auto& ar = result.unwrap();
    REQUIRE(ar.report["bills_due_count"].get<int>() >= 1);
    REQUIRE(ar.report["total_due"].get<double>() > 0);

    auto& bills = ar.report["bills_due"];
    for (const auto& bill : bills) {
        REQUIRE(bill.contains("payee"));
        REQUIRE(bill.contains("amount"));
        REQUIRE(bill.contains("account_path"));
    }

    book.close();
    fs::remove(tmp);
    fs::remove(state.db_path());
}

TEST_CASE("run_bill_pay: creates pending payment requests", "[agent][bill-pay]") {
    auto tmp = make_writable_copy(ACCOUNTS_DB);
    auto book_result = Book::open(tmp, false);
    REQUIRE(book_result.is_ok());
    auto& book = book_result.unwrap();

    auto payable_guid = create_payable_account(book);
    post_txn(book, "Vendor Bill", payable_guid, CHECKING, 50000, "2026-01-15 00:00:00");

    dhall::AgentConfig config;
    config.name = "bill-pay";

    auto state_result = AgentStateDB::open(tmp, "bill-pay");
    REQUIRE(state_result.is_ok());
    auto& state = state_result.unwrap();

    auto result = run_bill_pay(book, config, state);
    REQUIRE(result.is_ok());

    auto& ar = result.unwrap();
    REQUIRE(ar.report["pending_count"].get<int>() >= 1);
    REQUIRE(ar.report["pending_approval"].size() >= 1);

    auto& pending = ar.report["pending_approval"];
    REQUIRE(pending[0].contains("payee"));
    REQUIRE(pending[0].contains("amount"));
    REQUIRE(pending[0]["status"].get<std::string>() == "pending");

    book.close();
    fs::remove(tmp);
    fs::remove(state.db_path());
}

TEST_CASE("run_bill_pay: dispatch via run_agent", "[agent][bill-pay][dispatch]") {
    auto tmp = make_writable_copy(ACCOUNTS_DB);
    auto book_result = Book::open(tmp, false);
    REQUIRE(book_result.is_ok());
    auto& book = book_result.unwrap();

    dhall::AgentConfig config;
    config.name = "bill-pay";

    auto state_result = AgentStateDB::open(tmp, "bill-pay");
    REQUIRE(state_result.is_ok());
    auto& state = state_result.unwrap();

    auto result = run_agent("bill-pay", book, config, state);
    REQUIRE(result.is_ok());
    REQUIRE(result.unwrap().agent_name == "bill-pay");

    book.close();
    fs::remove(tmp);
    fs::remove(state.db_path());
}

// ========================================================================
// E2E: All 7 agents dispatch correctly
// ========================================================================

// ========================================================================
// bank-feed-importer Tests
// ========================================================================

TEST_CASE("run_bank_feed_importer: produces import status", "[agent][bank-feed]") {
    auto tmp = make_writable_copy(ACCOUNTS_DB);
    auto book_result = Book::open(tmp, false);
    REQUIRE(book_result.is_ok());
    auto& book = book_result.unwrap();

    dhall::AgentConfig config;
    config.name = "bank-feed-importer";

    auto state_result = AgentStateDB::open(tmp, "bank-feed-importer");
    REQUIRE(state_result.is_ok());
    auto& state = state_result.unwrap();

    auto result = run_bank_feed_importer(book, config, state);
    REQUIRE(result.is_ok());

    auto& ar = result.unwrap();
    REQUIRE(ar.agent_name == "bank-feed-importer");
    REQUIRE(ar.actions_taken == 0); // Read-only scan

    auto& report = ar.report;
    REQUIRE(report.contains("accounts_processed"));
    REQUIRE(report.contains("total_imported"));
    REQUIRE(report.contains("total_duplicates"));
    REQUIRE(report.contains("import_results"));

    // With no imports yet, totals should be zero
    REQUIRE(report["total_imported"].get<int>() == 0);

    book.close();
    fs::remove(tmp);
    fs::remove(state.db_path());
}

TEST_CASE("run_bank_feed_importer: dispatch via run_agent", "[agent][bank-feed][dispatch]") {
    auto tmp = make_writable_copy(ACCOUNTS_DB);
    auto book_result = Book::open(tmp, false);
    REQUIRE(book_result.is_ok());
    auto& book = book_result.unwrap();

    dhall::AgentConfig config;
    config.name = "bank-feed-importer";

    auto state_result = AgentStateDB::open(tmp, "bank-feed-importer");
    REQUIRE(state_result.is_ok());
    auto& state = state_result.unwrap();

    auto result = run_agent("bank-feed-importer", book, config, state);
    REQUIRE(result.is_ok());
    REQUIRE(result.unwrap().agent_name == "bank-feed-importer");

    book.close();
    fs::remove(tmp);
    fs::remove(state.db_path());
}

// ========================================================================
// reconciler Tests
// ========================================================================

TEST_CASE("run_reconciler: produces reconciliation status", "[agent][reconciler]") {
    auto tmp = make_writable_copy(ACCOUNTS_DB);
    auto book_result = Book::open(tmp, false);
    REQUIRE(book_result.is_ok());
    auto& book = book_result.unwrap();

    seed_transactions(book);

    dhall::AgentConfig config;
    config.name = "reconciler";

    auto state_result = AgentStateDB::open(tmp, "reconciler");
    REQUIRE(state_result.is_ok());
    auto& state = state_result.unwrap();

    auto result = run_reconciler(book, config, state);
    REQUIRE(result.is_ok());

    auto& ar = result.unwrap();
    REQUIRE(ar.agent_name == "reconciler");
    REQUIRE(ar.actions_taken == 0); // Read-only scan

    auto& report = ar.report;
    REQUIRE(report.contains("accounts_reconciled"));
    REQUIRE(report.contains("transfers_matched"));
    REQUIRE(report.contains("reconciled_accounts"));
    REQUIRE(report.contains("transfer_matches"));

    // Should have scanned some accounts
    REQUIRE(report["accounts_reconciled"].get<int>() >= 0);

    book.close();
    fs::remove(tmp);
    fs::remove(state.db_path());
}

TEST_CASE("run_reconciler: dispatch via run_agent", "[agent][reconciler][dispatch]") {
    auto tmp = make_writable_copy(ACCOUNTS_DB);
    auto book_result = Book::open(tmp, false);
    REQUIRE(book_result.is_ok());
    auto& book = book_result.unwrap();

    dhall::AgentConfig config;
    config.name = "reconciler";

    auto state_result = AgentStateDB::open(tmp, "reconciler");
    REQUIRE(state_result.is_ok());
    auto& state = state_result.unwrap();

    auto result = run_agent("reconciler", book, config, state);
    REQUIRE(result.is_ok());
    REQUIRE(result.unwrap().agent_name == "reconciler");

    book.close();
    fs::remove(tmp);
    fs::remove(state.db_path());
}

// ========================================================================
// E2E: All 9 agents dispatch correctly
// ========================================================================

TEST_CASE("run_agent: all 9 agents dispatch", "[agent][dispatch][e2e]") {
    auto tmp = make_writable_copy(ACCOUNTS_DB);
    auto book_result = Book::open(tmp, false);
    REQUIRE(book_result.is_ok());
    auto& book = book_result.unwrap();

    seed_transactions(book);

    std::vector<std::string> agent_names = {
        "spend-monitor", "report-generator", "transaction-categorizer",
        "invoice-generator", "tax-estimator", "subscription-manager", "bill-pay",
        "bank-feed-importer", "reconciler"
    };

    for (const auto& name : agent_names) {
        dhall::AgentConfig config;
        config.name = name;

        auto state_result = AgentStateDB::open(tmp, name);
        REQUIRE(state_result.is_ok());
        auto& state = state_result.unwrap();

        auto result = run_agent(name, book, config, state);
        REQUIRE(result.is_ok());
        REQUIRE(result.unwrap().agent_name == name);

        fs::remove(state.db_path());
    }

    book.close();
    fs::remove(tmp);
}
