// GnuCash Book - SQLite database reader/writer
// Uses sqlite3 C API directly (no ORM)

#include "gnucash/book.h"
#include "gnucash/guid.h"
#include <sqlite3.h>
#include <unordered_map>
#include <algorithm>
#include <ctime>
#include <sstream>
#include <iomanip>
#include <functional>

namespace gnucash {

namespace {
    // RAII wrapper for sqlite3_stmt
    struct StmtGuard {
        sqlite3_stmt* stmt;
        StmtGuard(sqlite3_stmt* s) : stmt(s) {}
        ~StmtGuard() { if (stmt) sqlite3_finalize(stmt); }
    };

    std::string col_text(sqlite3_stmt* stmt, int col) {
        const char* p = reinterpret_cast<const char*>(sqlite3_column_text(stmt, col));
        return p ? p : "";
    }

    std::optional<std::string> col_text_opt(sqlite3_stmt* stmt, int col) {
        if (sqlite3_column_type(stmt, col) == SQLITE_NULL) return std::nullopt;
        return col_text(stmt, col);
    }

    int64_t col_int64(sqlite3_stmt* stmt, int col) {
        return sqlite3_column_int64(stmt, col);
    }

    int col_int(sqlite3_stmt* stmt, int col) {
        return sqlite3_column_int(stmt, col);
    }

    bool col_bool(sqlite3_stmt* stmt, int col) {
        return sqlite3_column_int(stmt, col) != 0;
    }

    ReconcileState parse_reconcile(const std::string& s) {
        if (s == "c") return ReconcileState::CLEARED;
        if (s == "y") return ReconcileState::RECONCILED;
        return ReconcileState::NOT_RECONCILED;
    }

    std::string now_timestamp() {
        std::time_t t = std::time(nullptr);
        std::tm tm{};
        gmtime_r(&t, &tm);
        std::ostringstream ss;
        ss << std::put_time(&tm, "%Y-%m-%d %H:%M:%S");
        return ss.str();
    }
}

// --- Construction / destruction ---

Book::Book()
    : db_(nullptr), read_only_(true) {}

Book::~Book() {
    close();
}

Book::Book(Book&& other) noexcept
    : db_(other.db_), path_(std::move(other.path_)),
      book_guid_(std::move(other.book_guid_)),
      root_account_guid_(std::move(other.root_account_guid_)),
      default_currency_(std::move(other.default_currency_)),
      read_only_(other.read_only_) {
    other.db_ = nullptr;
}

Book& Book::operator=(Book&& other) noexcept {
    if (this != &other) {
        close();
        db_ = other.db_;
        path_ = std::move(other.path_);
        book_guid_ = std::move(other.book_guid_);
        root_account_guid_ = std::move(other.root_account_guid_);
        default_currency_ = std::move(other.default_currency_);
        read_only_ = other.read_only_;
        other.db_ = nullptr;
    }
    return *this;
}

Result<Book> Book::open(const std::string& path, bool read_only) {
    Book book;
    book.path_ = path;
    book.read_only_ = read_only;

    int flags = read_only ? SQLITE_OPEN_READONLY : SQLITE_OPEN_READWRITE;
    int rc = sqlite3_open_v2(path.c_str(), &book.db_, flags, nullptr);
    if (rc != SQLITE_OK) {
        std::string err = book.db_ ? sqlite3_errmsg(book.db_) : "unknown error";
        if (book.db_) sqlite3_close(book.db_);
        book.db_ = nullptr;
        return Result<Book>::err("Failed to open " + path + ": " + err);
    }

    auto meta = book.init_metadata();
    if (meta.is_err()) {
        sqlite3_close(book.db_);
        book.db_ = nullptr;
        return Result<Book>::err(meta.unwrap_err());
    }

    return Result<Book>::ok(std::move(book));
}

void Book::close() {
    if (db_) {
        sqlite3_close(db_);
        db_ = nullptr;
    }
}

bool Book::is_valid() const { return db_ != nullptr; }
bool Book::is_read_only() const { return read_only_; }
const std::string& Book::path() const { return path_; }
const std::string& Book::book_guid() const { return book_guid_; }
const std::string& Book::root_account_guid() const { return root_account_guid_; }
const std::string& Book::default_currency() const { return default_currency_; }

Result<void> Book::init_metadata() {
    // Read books table
    sqlite3_stmt* stmt = nullptr;
    int rc = sqlite3_prepare_v2(db_,
        "SELECT guid, root_account_guid FROM books LIMIT 1", -1, &stmt, nullptr);
    if (rc != SQLITE_OK) return Result<void>::err("Failed to read books table");
    StmtGuard guard(stmt);

    if (sqlite3_step(stmt) != SQLITE_ROW)
        return Result<void>::err("No book record found");

    book_guid_ = col_text(stmt, 0);
    root_account_guid_ = col_text(stmt, 1);

    // Resolve default currency
    default_currency_ = resolve_currency_guid();
    if (default_currency_.empty()) default_currency_ = "USD";

    return Result<void>::ok();
}

std::string Book::resolve_currency_guid() const {
    // Find the currency used by the root account's children (most common approach)
    // or fall back to first CURRENCY commodity
    sqlite3_stmt* stmt = nullptr;
    int rc = sqlite3_prepare_v2(db_,
        "SELECT c.mnemonic FROM commodities c "
        "JOIN accounts a ON a.commodity_guid = c.guid "
        "WHERE a.parent_guid = ? AND c.namespace IN ('CURRENCY','ISO4217') "
        "LIMIT 1",
        -1, &stmt, nullptr);
    if (rc != SQLITE_OK) return "";
    StmtGuard guard(stmt);

    sqlite3_bind_text(stmt, 1, root_account_guid_.c_str(), -1, SQLITE_STATIC);
    if (sqlite3_step(stmt) == SQLITE_ROW) return col_text(stmt, 0);
    return "";
}

// --- Read operations ---

std::vector<Account> Book::get_accounts() const {
    std::vector<Account> accounts;
    sqlite3_stmt* stmt = nullptr;
    int rc = sqlite3_prepare_v2(db_,
        "SELECT guid, name, account_type, commodity_guid, commodity_scu, "
        "non_std_scu, parent_guid, code, description, hidden, placeholder "
        "FROM accounts", -1, &stmt, nullptr);
    if (rc != SQLITE_OK) return accounts;
    StmtGuard guard(stmt);

    while (sqlite3_step(stmt) == SQLITE_ROW) {
        Account a;
        a.guid = col_text(stmt, 0);
        a.name = col_text(stmt, 1);
        a.type = parse_account_type(col_text(stmt, 2));
        a.commodity_guid = col_text(stmt, 3);
        a.commodity_scu = col_int(stmt, 4);
        a.non_std_scu = col_bool(stmt, 5);
        a.parent_guid = col_text(stmt, 6);
        a.code = col_text(stmt, 7);
        a.description = col_text(stmt, 8);
        a.hidden = col_bool(stmt, 9);
        a.placeholder = col_bool(stmt, 10);
        accounts.push_back(std::move(a));
    }
    return accounts;
}

std::vector<Transaction> Book::get_transactions(
    std::optional<std::string> from_date,
    std::optional<std::string> to_date) const {

    std::string sql = "SELECT guid, currency_guid, num, post_date, enter_date, description "
                      "FROM transactions";
    std::vector<std::string> conditions;
    if (from_date) conditions.push_back("post_date >= ?");
    if (to_date) conditions.push_back("post_date <= ?");
    if (!conditions.empty()) {
        sql += " WHERE ";
        for (size_t i = 0; i < conditions.size(); i++) {
            if (i > 0) sql += " AND ";
            sql += conditions[i];
        }
    }
    sql += " ORDER BY post_date";

    sqlite3_stmt* stmt = nullptr;
    if (sqlite3_prepare_v2(db_, sql.c_str(), -1, &stmt, nullptr) != SQLITE_OK)
        return {};
    StmtGuard guard(stmt);

    int bind_idx = 1;
    if (from_date)
        sqlite3_bind_text(stmt, bind_idx++, from_date->c_str(), -1, SQLITE_STATIC);
    if (to_date)
        sqlite3_bind_text(stmt, bind_idx++, to_date->c_str(), -1, SQLITE_STATIC);

    std::vector<Transaction> txns;
    while (sqlite3_step(stmt) == SQLITE_ROW) {
        Transaction t;
        t.guid = col_text(stmt, 0);
        t.currency_guid = col_text(stmt, 1);
        t.num = col_text(stmt, 2);
        t.post_date = col_text(stmt, 3);
        t.enter_date = col_text(stmt, 4);
        t.description = col_text(stmt, 5);
        txns.push_back(std::move(t));
    }

    // Load splits for each transaction
    for (auto& txn : txns) {
        sqlite3_stmt* ss = nullptr;
        if (sqlite3_prepare_v2(db_,
            "SELECT guid, tx_guid, account_guid, memo, action, reconcile_state, "
            "reconcile_date, value_num, value_denom, quantity_num, quantity_denom, lot_guid "
            "FROM splits WHERE tx_guid = ?",
            -1, &ss, nullptr) != SQLITE_OK) continue;
        StmtGuard sg(ss);
        sqlite3_bind_text(ss, 1, txn.guid.c_str(), -1, SQLITE_STATIC);

        while (sqlite3_step(ss) == SQLITE_ROW) {
            Split s;
            s.guid = col_text(ss, 0);
            s.tx_guid = col_text(ss, 1);
            s.account_guid = col_text(ss, 2);
            s.memo = col_text(ss, 3);
            s.action = col_text(ss, 4);
            s.reconcile_state = parse_reconcile(col_text(ss, 5));
            s.reconcile_date = col_text_opt(ss, 6);
            s.value = {col_int64(ss, 7), col_int64(ss, 8)};
            s.quantity = {col_int64(ss, 9), col_int64(ss, 10)};
            s.lot_guid = col_text_opt(ss, 11);
            txn.splits.push_back(std::move(s));
        }
    }

    return txns;
}

std::vector<Split> Book::get_splits_for_account(const std::string& account_guid) const {
    std::vector<Split> splits;
    sqlite3_stmt* stmt = nullptr;
    if (sqlite3_prepare_v2(db_,
        "SELECT s.guid, s.tx_guid, s.account_guid, s.memo, s.action, "
        "s.reconcile_state, s.reconcile_date, s.value_num, s.value_denom, "
        "s.quantity_num, s.quantity_denom, s.lot_guid "
        "FROM splits s "
        "JOIN transactions t ON s.tx_guid = t.guid "
        "WHERE s.account_guid = ? "
        "ORDER BY t.post_date",
        -1, &stmt, nullptr) != SQLITE_OK) return splits;
    StmtGuard guard(stmt);
    sqlite3_bind_text(stmt, 1, account_guid.c_str(), -1, SQLITE_STATIC);

    while (sqlite3_step(stmt) == SQLITE_ROW) {
        Split s;
        s.guid = col_text(stmt, 0);
        s.tx_guid = col_text(stmt, 1);
        s.account_guid = col_text(stmt, 2);
        s.memo = col_text(stmt, 3);
        s.action = col_text(stmt, 4);
        s.reconcile_state = parse_reconcile(col_text(stmt, 5));
        s.reconcile_date = col_text_opt(stmt, 6);
        s.value = {col_int64(stmt, 7), col_int64(stmt, 8)};
        s.quantity = {col_int64(stmt, 9), col_int64(stmt, 10)};
        s.lot_guid = col_text_opt(stmt, 11);
        splits.push_back(std::move(s));
    }
    return splits;
}

std::vector<Commodity> Book::get_commodities() const {
    std::vector<Commodity> commodities;
    sqlite3_stmt* stmt = nullptr;
    if (sqlite3_prepare_v2(db_,
        "SELECT guid, namespace, mnemonic, fullname, cusip, fraction, "
        "quote_flag, quote_source FROM commodities",
        -1, &stmt, nullptr) != SQLITE_OK) return commodities;
    StmtGuard guard(stmt);

    while (sqlite3_step(stmt) == SQLITE_ROW) {
        Commodity c;
        c.guid = col_text(stmt, 0);
        c.ns = col_text(stmt, 1);
        c.mnemonic = col_text(stmt, 2);
        c.fullname = col_text(stmt, 3);
        c.cusip = col_text(stmt, 4);
        c.fraction = col_int(stmt, 5);
        c.quote_flag = col_bool(stmt, 6);
        c.quote_source = col_text(stmt, 7);
        commodities.push_back(std::move(c));
    }
    return commodities;
}

std::vector<Price> Book::get_prices() const {
    std::vector<Price> prices;
    sqlite3_stmt* stmt = nullptr;
    if (sqlite3_prepare_v2(db_,
        "SELECT guid, commodity_guid, currency_guid, date, source, type, "
        "value_num, value_denom FROM prices",
        -1, &stmt, nullptr) != SQLITE_OK) return prices;
    StmtGuard guard(stmt);

    while (sqlite3_step(stmt) == SQLITE_ROW) {
        Price p;
        p.guid = col_text(stmt, 0);
        p.commodity_guid = col_text(stmt, 1);
        p.currency_guid = col_text(stmt, 2);
        p.date = col_text(stmt, 3);
        p.source = col_text(stmt, 4);
        p.type = col_text(stmt, 5);
        p.value = {col_int64(stmt, 6), col_int64(stmt, 7)};
        prices.push_back(std::move(p));
    }
    return prices;
}

std::optional<Account> Book::get_account(const std::string& guid) const {
    sqlite3_stmt* stmt = nullptr;
    if (sqlite3_prepare_v2(db_,
        "SELECT guid, name, account_type, commodity_guid, commodity_scu, "
        "non_std_scu, parent_guid, code, description, hidden, placeholder "
        "FROM accounts WHERE guid = ?",
        -1, &stmt, nullptr) != SQLITE_OK) return std::nullopt;
    StmtGuard guard(stmt);
    sqlite3_bind_text(stmt, 1, guid.c_str(), -1, SQLITE_STATIC);

    if (sqlite3_step(stmt) != SQLITE_ROW) return std::nullopt;

    Account a;
    a.guid = col_text(stmt, 0);
    a.name = col_text(stmt, 1);
    a.type = parse_account_type(col_text(stmt, 2));
    a.commodity_guid = col_text(stmt, 3);
    a.commodity_scu = col_int(stmt, 4);
    a.non_std_scu = col_bool(stmt, 5);
    a.parent_guid = col_text(stmt, 6);
    a.code = col_text(stmt, 7);
    a.description = col_text(stmt, 8);
    a.hidden = col_bool(stmt, 9);
    a.placeholder = col_bool(stmt, 10);
    return a;
}

std::optional<Account> Book::get_account_by_path(const std::string& path) const {
    auto tree = account_tree();
    for (const auto& a : tree) {
        if (a.full_path == path) return a;
    }
    return std::nullopt;
}

std::optional<Transaction> Book::get_transaction(const std::string& guid) const {
    sqlite3_stmt* stmt = nullptr;
    if (sqlite3_prepare_v2(db_,
        "SELECT guid, currency_guid, num, post_date, enter_date, description "
        "FROM transactions WHERE guid = ?",
        -1, &stmt, nullptr) != SQLITE_OK) return std::nullopt;
    StmtGuard guard(stmt);
    sqlite3_bind_text(stmt, 1, guid.c_str(), -1, SQLITE_STATIC);

    if (sqlite3_step(stmt) != SQLITE_ROW) return std::nullopt;

    Transaction t;
    t.guid = col_text(stmt, 0);
    t.currency_guid = col_text(stmt, 1);
    t.num = col_text(stmt, 2);
    t.post_date = col_text(stmt, 3);
    t.enter_date = col_text(stmt, 4);
    t.description = col_text(stmt, 5);

    // Load splits
    sqlite3_stmt* ss = nullptr;
    if (sqlite3_prepare_v2(db_,
        "SELECT guid, tx_guid, account_guid, memo, action, reconcile_state, "
        "reconcile_date, value_num, value_denom, quantity_num, quantity_denom, lot_guid "
        "FROM splits WHERE tx_guid = ?",
        -1, &ss, nullptr) == SQLITE_OK) {
        StmtGuard sg(ss);
        sqlite3_bind_text(ss, 1, guid.c_str(), -1, SQLITE_STATIC);
        while (sqlite3_step(ss) == SQLITE_ROW) {
            Split s;
            s.guid = col_text(ss, 0);
            s.tx_guid = col_text(ss, 1);
            s.account_guid = col_text(ss, 2);
            s.memo = col_text(ss, 3);
            s.action = col_text(ss, 4);
            s.reconcile_state = parse_reconcile(col_text(ss, 5));
            s.reconcile_date = col_text_opt(ss, 6);
            s.value = {col_int64(ss, 7), col_int64(ss, 8)};
            s.quantity = {col_int64(ss, 9), col_int64(ss, 10)};
            s.lot_guid = col_text_opt(ss, 11);
            t.splits.push_back(std::move(s));
        }
    }

    return t;
}

std::vector<Account> Book::account_tree() const {
    auto accounts = get_accounts();
    build_account_paths(accounts);
    return accounts;
}

void Book::build_account_paths(std::vector<Account>& accounts) const {
    // Build guid -> account index
    std::unordered_map<std::string, size_t> idx;
    for (size_t i = 0; i < accounts.size(); i++) {
        idx[accounts[i].guid] = i;
    }

    // Recursive path builder
    std::function<std::string(const std::string&)> build_path;
    build_path = [&](const std::string& guid) -> std::string {
        auto it = idx.find(guid);
        if (it == idx.end()) return "";
        auto& acct = accounts[it->second];
        if (!acct.full_path.empty()) return acct.full_path;
        if (acct.type == AccountType::ROOT || acct.parent_guid.empty()) {
            acct.full_path = acct.name;
            return acct.full_path;
        }
        std::string parent_path = build_path(acct.parent_guid);
        acct.full_path = parent_path.empty() ? acct.name : parent_path + ":" + acct.name;
        return acct.full_path;
    };

    for (auto& a : accounts) {
        if (a.full_path.empty()) build_path(a.guid);
    }
}

// --- Balance & report operations ---

double Book::get_account_balance(const std::string& account_guid,
                                  std::optional<std::string> as_of) const {
    std::string sql =
        "SELECT SUM(CAST(s.value_num AS REAL) / s.value_denom) "
        "FROM splits s "
        "JOIN transactions t ON s.tx_guid = t.guid "
        "WHERE s.account_guid = ?";
    if (as_of) sql += " AND t.post_date <= ?";

    sqlite3_stmt* stmt = nullptr;
    if (sqlite3_prepare_v2(db_, sql.c_str(), -1, &stmt, nullptr) != SQLITE_OK)
        return 0.0;
    StmtGuard guard(stmt);

    sqlite3_bind_text(stmt, 1, account_guid.c_str(), -1, SQLITE_STATIC);
    if (as_of)
        sqlite3_bind_text(stmt, 2, as_of->c_str(), -1, SQLITE_STATIC);

    if (sqlite3_step(stmt) == SQLITE_ROW) {
        if (sqlite3_column_type(stmt, 0) != SQLITE_NULL)
            return sqlite3_column_double(stmt, 0);
    }
    return 0.0;
}

TrialBalance Book::trial_balance(std::optional<std::string> as_of) const {
    std::string sql =
        "SELECT a.guid, a.name, a.account_type, "
        "SUM(CAST(s.value_num AS REAL) / s.value_denom) as balance "
        "FROM splits s "
        "JOIN transactions t ON s.tx_guid = t.guid "
        "JOIN accounts a ON s.account_guid = a.guid ";
    if (as_of) sql += "WHERE t.post_date <= ? ";
    sql += "GROUP BY a.guid, a.name, a.account_type "
           "HAVING balance != 0 "
           "ORDER BY a.account_type, a.name";

    sqlite3_stmt* stmt = nullptr;
    if (sqlite3_prepare_v2(db_, sql.c_str(), -1, &stmt, nullptr) != SQLITE_OK)
        return {};
    StmtGuard guard(stmt);

    if (as_of)
        sqlite3_bind_text(stmt, 1, as_of->c_str(), -1, SQLITE_STATIC);

    TrialBalance tb;
    while (sqlite3_step(stmt) == SQLITE_ROW) {
        TrialBalanceEntry e;
        e.account_guid = col_text(stmt, 0);
        e.account_name = col_text(stmt, 1);
        e.account_type = parse_account_type(col_text(stmt, 2));
        e.balance = sqlite3_column_double(stmt, 3);
        tb.push_back(std::move(e));
    }

    // Build full paths
    auto tree = account_tree();
    std::unordered_map<std::string, std::string> paths;
    for (const auto& a : tree) paths[a.guid] = a.full_path;
    for (auto& e : tb) {
        auto it = paths.find(e.account_guid);
        if (it != paths.end()) e.full_path = it->second;
    }

    return tb;
}

// --- Write operations ---

Result<std::string> Book::create_account(
    const std::string& name,
    AccountType type,
    const std::string& parent_guid,
    const std::string& description,
    const std::string& code,
    bool hidden,
    bool placeholder) {

    if (read_only_) return Result<std::string>::err("Database is read-only");
    if (name.empty()) return Result<std::string>::err("Account name cannot be empty");
    if (name.find(':') != std::string::npos)
        return Result<std::string>::err("Account name cannot contain ':'");

    // Verify parent exists
    auto parent = get_account(parent_guid);
    if (!parent) return Result<std::string>::err("Parent account not found: " + parent_guid);

    // Get commodity from parent
    std::string commodity_guid = parent->commodity_guid;

    std::string guid = generate_guid();

    sqlite3_stmt* stmt = nullptr;
    int rc = sqlite3_prepare_v2(db_,
        "INSERT INTO accounts (guid, name, account_type, commodity_guid, commodity_scu, "
        "non_std_scu, parent_guid, code, description, hidden, placeholder) "
        "VALUES (?, ?, ?, ?, 100, 0, ?, ?, ?, ?, ?)",
        -1, &stmt, nullptr);
    if (rc != SQLITE_OK)
        return Result<std::string>::err(std::string("SQL prepare failed: ") + sqlite3_errmsg(db_));
    StmtGuard guard(stmt);

    sqlite3_bind_text(stmt, 1, guid.c_str(), -1, SQLITE_STATIC);
    sqlite3_bind_text(stmt, 2, name.c_str(), -1, SQLITE_STATIC);
    sqlite3_bind_text(stmt, 3, account_type_to_string(type).c_str(), -1, SQLITE_TRANSIENT);
    sqlite3_bind_text(stmt, 4, commodity_guid.c_str(), -1, SQLITE_STATIC);
    sqlite3_bind_text(stmt, 5, parent_guid.c_str(), -1, SQLITE_STATIC);
    sqlite3_bind_text(stmt, 6, code.c_str(), -1, SQLITE_STATIC);
    sqlite3_bind_text(stmt, 7, description.c_str(), -1, SQLITE_STATIC);
    sqlite3_bind_int(stmt, 8, hidden ? 1 : 0);
    sqlite3_bind_int(stmt, 9, placeholder ? 1 : 0);

    rc = sqlite3_step(stmt);
    if (rc != SQLITE_DONE)
        return Result<std::string>::err(std::string("Insert failed: ") + sqlite3_errmsg(db_));

    return Result<std::string>::ok(guid);
}

Result<std::string> Book::post_transaction(const Transaction& txn) {
    if (read_only_) return Result<std::string>::err("Database is read-only");

    // Validate: at least 2 splits
    if (txn.splits.size() < 2)
        return Result<std::string>::err("Transaction must have at least 2 splits");

    // Validate: splits balance
    std::vector<Fraction> values;
    for (const auto& s : txn.splits) values.push_back(s.value);
    if (!splits_balance(values))
        return Result<std::string>::err("Splits do not balance to zero");

    // Verify all accounts exist
    for (const auto& s : txn.splits) {
        auto acct = get_account(s.account_guid);
        if (!acct)
            return Result<std::string>::err("Account not found: " + s.account_guid);
    }

    std::string tx_guid = txn.guid.empty() ? generate_guid() : txn.guid;
    std::string enter_date = txn.enter_date.empty() ? now_timestamp() : txn.enter_date;

    // Begin transaction
    sqlite3_exec(db_, "BEGIN TRANSACTION", nullptr, nullptr, nullptr);

    // Insert transaction
    sqlite3_stmt* stmt = nullptr;
    int rc = sqlite3_prepare_v2(db_,
        "INSERT INTO transactions (guid, currency_guid, num, post_date, enter_date, description) "
        "VALUES (?, ?, ?, ?, ?, ?)",
        -1, &stmt, nullptr);
    if (rc != SQLITE_OK) {
        sqlite3_exec(db_, "ROLLBACK", nullptr, nullptr, nullptr);
        return Result<std::string>::err(std::string("SQL error: ") + sqlite3_errmsg(db_));
    }
    StmtGuard tg(stmt);

    sqlite3_bind_text(stmt, 1, tx_guid.c_str(), -1, SQLITE_STATIC);
    sqlite3_bind_text(stmt, 2, txn.currency_guid.c_str(), -1, SQLITE_STATIC);
    sqlite3_bind_text(stmt, 3, txn.num.c_str(), -1, SQLITE_STATIC);
    sqlite3_bind_text(stmt, 4, txn.post_date.c_str(), -1, SQLITE_STATIC);
    sqlite3_bind_text(stmt, 5, enter_date.c_str(), -1, SQLITE_STATIC);
    sqlite3_bind_text(stmt, 6, txn.description.c_str(), -1, SQLITE_STATIC);

    if (sqlite3_step(stmt) != SQLITE_DONE) {
        sqlite3_exec(db_, "ROLLBACK", nullptr, nullptr, nullptr);
        return Result<std::string>::err(std::string("Insert transaction failed: ") + sqlite3_errmsg(db_));
    }

    // Insert splits
    for (const auto& split : txn.splits) {
        sqlite3_stmt* ss = nullptr;
        rc = sqlite3_prepare_v2(db_,
            "INSERT INTO splits (guid, tx_guid, account_guid, memo, action, "
            "reconcile_state, reconcile_date, value_num, value_denom, "
            "quantity_num, quantity_denom, lot_guid) "
            "VALUES (?, ?, ?, ?, ?, 'n', NULL, ?, ?, ?, ?, NULL)",
            -1, &ss, nullptr);
        if (rc != SQLITE_OK) {
            sqlite3_exec(db_, "ROLLBACK", nullptr, nullptr, nullptr);
            return Result<std::string>::err(std::string("SQL error: ") + sqlite3_errmsg(db_));
        }
        StmtGuard sg(ss);

        std::string split_guid = split.guid.empty() ? generate_guid() : split.guid;

        sqlite3_bind_text(ss, 1, split_guid.c_str(), -1, SQLITE_TRANSIENT);
        sqlite3_bind_text(ss, 2, tx_guid.c_str(), -1, SQLITE_STATIC);
        sqlite3_bind_text(ss, 3, split.account_guid.c_str(), -1, SQLITE_STATIC);
        sqlite3_bind_text(ss, 4, split.memo.c_str(), -1, SQLITE_STATIC);
        sqlite3_bind_text(ss, 5, split.action.c_str(), -1, SQLITE_STATIC);
        sqlite3_bind_int64(ss, 6, split.value.num);
        sqlite3_bind_int64(ss, 7, split.value.denom);
        sqlite3_bind_int64(ss, 8, split.quantity.num);
        sqlite3_bind_int64(ss, 9, split.quantity.denom);

        if (sqlite3_step(ss) != SQLITE_DONE) {
            sqlite3_exec(db_, "ROLLBACK", nullptr, nullptr, nullptr);
            return Result<std::string>::err(std::string("Insert split failed: ") + sqlite3_errmsg(db_));
        }
    }

    sqlite3_exec(db_, "COMMIT", nullptr, nullptr, nullptr);
    return Result<std::string>::ok(tx_guid);
}

Result<void> Book::delete_transaction(const std::string& guid) {
    if (read_only_) return Result<void>::err("Database is read-only");

    sqlite3_exec(db_, "BEGIN TRANSACTION", nullptr, nullptr, nullptr);

    // Delete splits first (referential integrity)
    sqlite3_stmt* ss = nullptr;
    sqlite3_prepare_v2(db_, "DELETE FROM splits WHERE tx_guid = ?", -1, &ss, nullptr);
    StmtGuard sg(ss);
    sqlite3_bind_text(ss, 1, guid.c_str(), -1, SQLITE_STATIC);
    sqlite3_step(ss);

    // Delete transaction
    sqlite3_stmt* ts = nullptr;
    sqlite3_prepare_v2(db_, "DELETE FROM transactions WHERE guid = ?", -1, &ts, nullptr);
    StmtGuard tg(ts);
    sqlite3_bind_text(ts, 1, guid.c_str(), -1, SQLITE_STATIC);
    sqlite3_step(ts);

    sqlite3_exec(db_, "COMMIT", nullptr, nullptr, nullptr);
    return Result<void>::ok();
}

Result<void> Book::void_transaction(const std::string& guid, const std::string& reason) {
    if (read_only_) return Result<void>::err("Database is read-only");

    sqlite3_exec(db_, "BEGIN TRANSACTION", nullptr, nullptr, nullptr);

    // Update description with void reason
    sqlite3_stmt* ts = nullptr;
    sqlite3_prepare_v2(db_,
        "UPDATE transactions SET description = ? WHERE guid = ?",
        -1, &ts, nullptr);
    StmtGuard tg(ts);
    std::string desc = "Voided: " + reason;
    sqlite3_bind_text(ts, 1, desc.c_str(), -1, SQLITE_TRANSIENT);
    sqlite3_bind_text(ts, 2, guid.c_str(), -1, SQLITE_STATIC);
    sqlite3_step(ts);

    // Zero out split values
    sqlite3_stmt* ss = nullptr;
    sqlite3_prepare_v2(db_,
        "UPDATE splits SET value_num = 0, quantity_num = 0 WHERE tx_guid = ?",
        -1, &ss, nullptr);
    StmtGuard sg(ss);
    sqlite3_bind_text(ss, 1, guid.c_str(), -1, SQLITE_STATIC);
    sqlite3_step(ss);

    sqlite3_exec(db_, "COMMIT", nullptr, nullptr, nullptr);
    return Result<void>::ok();
}

Result<void> Book::update_split(const std::string& split_guid,
                                const std::string& new_account_guid) {
    if (read_only_) return Result<void>::err("Database is read-only");

    // Verify the target account exists
    auto acct = get_account(new_account_guid);
    if (!acct)
        return Result<void>::err("Account not found: " + new_account_guid);

    sqlite3_stmt* stmt = nullptr;
    int rc = sqlite3_prepare_v2(db_,
        "UPDATE splits SET account_guid = ? WHERE guid = ?",
        -1, &stmt, nullptr);
    if (rc != SQLITE_OK)
        return Result<void>::err(std::string("SQL error: ") + sqlite3_errmsg(db_));
    StmtGuard sg(stmt);

    sqlite3_bind_text(stmt, 1, new_account_guid.c_str(), -1, SQLITE_STATIC);
    sqlite3_bind_text(stmt, 2, split_guid.c_str(), -1, SQLITE_STATIC);

    if (sqlite3_step(stmt) != SQLITE_DONE)
        return Result<void>::err(std::string("Update split failed: ") + sqlite3_errmsg(db_));

    if (sqlite3_changes(db_) == 0)
        return Result<void>::err("Split not found: " + split_guid);

    return Result<void>::ok();
}

sqlite3* Book::raw_db() const {
    return db_;
}

} // namespace gnucash
