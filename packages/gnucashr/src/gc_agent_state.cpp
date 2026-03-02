// Agent state database for gnucashr R package
// Vendored from lib/gnucash-core/src/agent_state.cpp
// Per-agent key-value state + review queue

#include "gnucash/agent_state.h"
#include "gnucash/audit.h"  // for now_iso8601()
#include <sqlite3.h>

namespace gnucash {
namespace agent {

// ========================================================================
// ReviewStatus helpers
// ========================================================================

std::string review_status_to_string(ReviewStatus s) {
    switch (s) {
        case ReviewStatus::PENDING: return "pending";
        case ReviewStatus::APPROVED: return "approved";
        case ReviewStatus::REJECTED: return "rejected";
    }
    return "pending";
}

ReviewStatus parse_review_status(const std::string& s) {
    if (s == "approved") return ReviewStatus::APPROVED;
    if (s == "rejected") return ReviewStatus::REJECTED;
    return ReviewStatus::PENDING;
}

// ========================================================================
// AgentStateDB
// ========================================================================

AgentStateDB::AgentStateDB(void* db, std::string path)
    : db_(db), db_path_(std::move(path)) {}

AgentStateDB::~AgentStateDB() {
    if (db_) {
        sqlite3_close(static_cast<sqlite3*>(db_));
        db_ = nullptr;
    }
}

AgentStateDB::AgentStateDB(AgentStateDB&& other) noexcept
    : db_(other.db_), db_path_(std::move(other.db_path_)) {
    other.db_ = nullptr;
}

AgentStateDB& AgentStateDB::operator=(AgentStateDB&& other) noexcept {
    if (this != &other) {
        if (db_) sqlite3_close(static_cast<sqlite3*>(db_));
        db_ = other.db_;
        db_path_ = std::move(other.db_path_);
        other.db_ = nullptr;
    }
    return *this;
}

Result<AgentStateDB> AgentStateDB::open(const std::string& book_path,
                                         const std::string& agent_name) {
    std::string path = book_path + ".agent." + agent_name + ".db";

    sqlite3* db = nullptr;
    int rc = sqlite3_open(path.c_str(), &db);
    if (rc != SQLITE_OK) {
        std::string err = sqlite3_errmsg(db);
        sqlite3_close(db);
        return Result<AgentStateDB>::err("Failed to open agent state DB: " + err);
    }

    // Enable WAL mode
    sqlite3_exec(db, "PRAGMA journal_mode=WAL;", nullptr, nullptr, nullptr);

    // Create schema
    char* errmsg = nullptr;
    rc = sqlite3_exec(db, AGENT_STATE_SCHEMA, nullptr, nullptr, &errmsg);
    if (rc != SQLITE_OK) {
        std::string err = errmsg ? errmsg : "unknown error";
        sqlite3_free(errmsg);
        sqlite3_close(db);
        return Result<AgentStateDB>::err("Failed to create schema: " + err);
    }

    return Result<AgentStateDB>::ok(AgentStateDB(db, path));
}

Result<void> AgentStateDB::set(const std::string& key, const std::string& value) {
    const char* sql = "INSERT OR REPLACE INTO agent_state (key, value, updated_at) "
                      "VALUES (?, ?, ?)";
    sqlite3_stmt* stmt = nullptr;
    auto* db = static_cast<sqlite3*>(db_);

    int rc = sqlite3_prepare_v2(db, sql, -1, &stmt, nullptr);
    if (rc != SQLITE_OK) {
        return Result<void>::err(sqlite3_errmsg(db));
    }

    std::string now = audit::now_iso8601();
    sqlite3_bind_text(stmt, 1, key.c_str(), -1, SQLITE_TRANSIENT);
    sqlite3_bind_text(stmt, 2, value.c_str(), -1, SQLITE_TRANSIENT);
    sqlite3_bind_text(stmt, 3, now.c_str(), -1, SQLITE_TRANSIENT);

    rc = sqlite3_step(stmt);
    sqlite3_finalize(stmt);

    if (rc != SQLITE_DONE) {
        return Result<void>::err(sqlite3_errmsg(db));
    }
    return Result<void>::ok();
}

Result<std::optional<std::string>> AgentStateDB::get(const std::string& key) {
    const char* sql = "SELECT value FROM agent_state WHERE key = ?";
    sqlite3_stmt* stmt = nullptr;
    auto* db = static_cast<sqlite3*>(db_);

    int rc = sqlite3_prepare_v2(db, sql, -1, &stmt, nullptr);
    if (rc != SQLITE_OK) {
        return Result<std::optional<std::string>>::err(sqlite3_errmsg(db));
    }

    sqlite3_bind_text(stmt, 1, key.c_str(), -1, SQLITE_TRANSIENT);

    rc = sqlite3_step(stmt);
    if (rc == SQLITE_ROW) {
        const char* val = reinterpret_cast<const char*>(sqlite3_column_text(stmt, 0));
        std::string result = val ? val : "";
        sqlite3_finalize(stmt);
        return Result<std::optional<std::string>>::ok(std::optional<std::string>(result));
    }
    sqlite3_finalize(stmt);

    if (rc == SQLITE_DONE) {
        return Result<std::optional<std::string>>::ok(std::nullopt);
    }
    return Result<std::optional<std::string>>::err(sqlite3_errmsg(db));
}

Result<int> AgentStateDB::enqueue_review(const std::string& transaction_guid,
                                          const std::string& suggested_category,
                                          double confidence,
                                          const std::string& reason) {
    const char* sql = "INSERT INTO review_queue "
                      "(transaction_guid, suggested_category, confidence, reason, status, created_at) "
                      "VALUES (?, ?, ?, ?, 'pending', ?)";
    sqlite3_stmt* stmt = nullptr;
    auto* db = static_cast<sqlite3*>(db_);

    int rc = sqlite3_prepare_v2(db, sql, -1, &stmt, nullptr);
    if (rc != SQLITE_OK) {
        return Result<int>::err(sqlite3_errmsg(db));
    }

    std::string now = audit::now_iso8601();
    sqlite3_bind_text(stmt, 1, transaction_guid.c_str(), -1, SQLITE_TRANSIENT);
    sqlite3_bind_text(stmt, 2, suggested_category.c_str(), -1, SQLITE_TRANSIENT);
    sqlite3_bind_double(stmt, 3, confidence);
    sqlite3_bind_text(stmt, 4, reason.c_str(), -1, SQLITE_TRANSIENT);
    sqlite3_bind_text(stmt, 5, now.c_str(), -1, SQLITE_TRANSIENT);

    rc = sqlite3_step(stmt);
    sqlite3_finalize(stmt);

    if (rc != SQLITE_DONE) {
        return Result<int>::err(sqlite3_errmsg(db));
    }

    return Result<int>::ok(static_cast<int>(sqlite3_last_insert_rowid(db)));
}

Result<std::vector<ReviewItem>> AgentStateDB::pending_reviews(int limit) {
    const char* sql = "SELECT id, transaction_guid, suggested_category, confidence, "
                      "reason, status, created_at FROM review_queue "
                      "WHERE status = 'pending' ORDER BY created_at DESC LIMIT ?";
    sqlite3_stmt* stmt = nullptr;
    auto* db = static_cast<sqlite3*>(db_);

    int rc = sqlite3_prepare_v2(db, sql, -1, &stmt, nullptr);
    if (rc != SQLITE_OK) {
        return Result<std::vector<ReviewItem>>::err(sqlite3_errmsg(db));
    }

    sqlite3_bind_int(stmt, 1, limit);

    std::vector<ReviewItem> items;
    while ((rc = sqlite3_step(stmt)) == SQLITE_ROW) {
        ReviewItem item;
        item.id = sqlite3_column_int(stmt, 0);
        item.transaction_guid = reinterpret_cast<const char*>(sqlite3_column_text(stmt, 1));
        item.suggested_category = reinterpret_cast<const char*>(sqlite3_column_text(stmt, 2));
        item.confidence = sqlite3_column_double(stmt, 3);
        const char* reason = reinterpret_cast<const char*>(sqlite3_column_text(stmt, 4));
        item.reason = reason ? reason : "";
        const char* status = reinterpret_cast<const char*>(sqlite3_column_text(stmt, 5));
        item.status = parse_review_status(status ? status : "pending");
        item.created_at = reinterpret_cast<const char*>(sqlite3_column_text(stmt, 6));
        items.push_back(std::move(item));
    }
    sqlite3_finalize(stmt);

    return Result<std::vector<ReviewItem>>::ok(std::move(items));
}

Result<void> AgentStateDB::update_review(int id, ReviewStatus status) {
    const char* sql = "UPDATE review_queue SET status = ? WHERE id = ?";
    sqlite3_stmt* stmt = nullptr;
    auto* db = static_cast<sqlite3*>(db_);

    int rc = sqlite3_prepare_v2(db, sql, -1, &stmt, nullptr);
    if (rc != SQLITE_OK) {
        return Result<void>::err(sqlite3_errmsg(db));
    }

    std::string status_str = review_status_to_string(status);
    sqlite3_bind_text(stmt, 1, status_str.c_str(), -1, SQLITE_TRANSIENT);
    sqlite3_bind_int(stmt, 2, id);

    rc = sqlite3_step(stmt);
    sqlite3_finalize(stmt);

    if (rc != SQLITE_DONE) {
        return Result<void>::err(sqlite3_errmsg(db));
    }
    return Result<void>::ok();
}

} // namespace agent
} // namespace gnucash
