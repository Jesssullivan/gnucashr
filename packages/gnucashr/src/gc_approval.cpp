// Vendored from lib/gnucash-core/src/approval.cpp
// Adapted for R package build (gnucash/ prefix headers)

#include "gnucash/approval.h"
#include "gnucash/guid.h"
#include "gnucash/audit.h"
#include <sqlite3.h>

namespace gnucash {

ApprovalDB::~ApprovalDB() {
    if (db_) {
        sqlite3_close(db_);
        db_ = nullptr;
    }
}

ApprovalDB::ApprovalDB(ApprovalDB&& other) noexcept
    : db_(other.db_), db_path_(std::move(other.db_path_)) {
    other.db_ = nullptr;
}

ApprovalDB& ApprovalDB::operator=(ApprovalDB&& other) noexcept {
    if (this != &other) {
        if (db_) sqlite3_close(db_);
        db_ = other.db_;
        db_path_ = std::move(other.db_path_);
        other.db_ = nullptr;
    }
    return *this;
}

Result<ApprovalDB> ApprovalDB::open(const std::string& book_path) {
    ApprovalDB adb;
    adb.db_path_ = book_path + ".approvals.db";

    int rc = sqlite3_open(adb.db_path_.c_str(), &adb.db_);
    if (rc != SQLITE_OK) {
        std::string errmsg = sqlite3_errmsg(adb.db_);
        sqlite3_close(adb.db_);
        adb.db_ = nullptr;
        return Result<ApprovalDB>::err("Failed to open approval DB: " + errmsg);
    }

    // Enable WAL mode
    sqlite3_exec(adb.db_, "PRAGMA journal_mode=WAL;", nullptr, nullptr, nullptr);

    // Create schema
    char* errmsg = nullptr;
    rc = sqlite3_exec(adb.db_, APPROVAL_SCHEMA, nullptr, nullptr, &errmsg);
    if (rc != SQLITE_OK) {
        std::string error = errmsg ? errmsg : "unknown error";
        sqlite3_free(errmsg);
        return Result<ApprovalDB>::err("Failed to create approval schema: " + error);
    }

    return Result<ApprovalDB>::ok(std::move(adb));
}

Result<std::string> ApprovalDB::create_request(const std::string& agent_name,
                                                const std::string& tool_name,
                                                const json& arguments,
                                                const std::string& requesting_user,
                                                const std::string& reason) {
    std::string id = generate_guid();
    std::string now = audit::now_iso8601();
    std::string args_str = arguments.dump();

    const char* sql = "INSERT INTO approval_requests "
                      "(id, agent_name, tool_name, arguments, requesting_user, reason, created_at, status) "
                      "VALUES (?, ?, ?, ?, ?, ?, ?, 'pending')";

    sqlite3_stmt* stmt = nullptr;
    int rc = sqlite3_prepare_v2(db_, sql, -1, &stmt, nullptr);
    if (rc != SQLITE_OK) {
        return Result<std::string>::err("Failed to prepare insert: " + std::string(sqlite3_errmsg(db_)));
    }

    sqlite3_bind_text(stmt, 1, id.c_str(), -1, SQLITE_TRANSIENT);
    sqlite3_bind_text(stmt, 2, agent_name.c_str(), -1, SQLITE_TRANSIENT);
    sqlite3_bind_text(stmt, 3, tool_name.c_str(), -1, SQLITE_TRANSIENT);
    sqlite3_bind_text(stmt, 4, args_str.c_str(), -1, SQLITE_TRANSIENT);
    sqlite3_bind_text(stmt, 5, requesting_user.c_str(), -1, SQLITE_TRANSIENT);
    sqlite3_bind_text(stmt, 6, reason.c_str(), -1, SQLITE_TRANSIENT);
    sqlite3_bind_text(stmt, 7, now.c_str(), -1, SQLITE_TRANSIENT);

    rc = sqlite3_step(stmt);
    sqlite3_finalize(stmt);

    if (rc != SQLITE_DONE) {
        return Result<std::string>::err("Failed to create request: " + std::string(sqlite3_errmsg(db_)));
    }

    return Result<std::string>::ok(std::move(id));
}

static ApprovalRequest read_row(sqlite3_stmt* stmt) {
    ApprovalRequest req;
    req.id = reinterpret_cast<const char*>(sqlite3_column_text(stmt, 0));
    req.agent_name = reinterpret_cast<const char*>(sqlite3_column_text(stmt, 1));
    req.tool_name = reinterpret_cast<const char*>(sqlite3_column_text(stmt, 2));

    const char* args_text = reinterpret_cast<const char*>(sqlite3_column_text(stmt, 3));
    req.arguments = json::parse(args_text ? args_text : "{}");

    req.requesting_user = reinterpret_cast<const char*>(sqlite3_column_text(stmt, 4));
    req.reason = reinterpret_cast<const char*>(sqlite3_column_text(stmt, 5));
    req.created_at = reinterpret_cast<const char*>(sqlite3_column_text(stmt, 6));
    req.status = reinterpret_cast<const char*>(sqlite3_column_text(stmt, 7));

    auto col8 = sqlite3_column_text(stmt, 8);
    if (col8) req.approver = reinterpret_cast<const char*>(col8);

    auto col9 = sqlite3_column_text(stmt, 9);
    if (col9) req.resolved_at = reinterpret_cast<const char*>(col9);

    auto col10 = sqlite3_column_text(stmt, 10);
    if (col10) req.rejection_reason = reinterpret_cast<const char*>(col10);

    return req;
}

Result<std::vector<ApprovalRequest>> ApprovalDB::pending_requests(int limit) {
    const char* sql = "SELECT id, agent_name, tool_name, arguments, requesting_user, "
                      "reason, created_at, status, approver, resolved_at, rejection_reason "
                      "FROM approval_requests WHERE status = 'pending' "
                      "ORDER BY created_at ASC LIMIT ?";

    sqlite3_stmt* stmt = nullptr;
    int rc = sqlite3_prepare_v2(db_, sql, -1, &stmt, nullptr);
    if (rc != SQLITE_OK) {
        return Result<std::vector<ApprovalRequest>>::err(
            "Failed to query: " + std::string(sqlite3_errmsg(db_)));
    }

    sqlite3_bind_int(stmt, 1, limit);

    std::vector<ApprovalRequest> results;
    while (sqlite3_step(stmt) == SQLITE_ROW) {
        results.push_back(read_row(stmt));
    }
    sqlite3_finalize(stmt);

    return Result<std::vector<ApprovalRequest>>::ok(std::move(results));
}

Result<void> ApprovalDB::approve(const std::string& id, const std::string& approver) {
    std::string now = audit::now_iso8601();
    const char* sql = "UPDATE approval_requests SET status = 'approved', "
                      "approver = ?, resolved_at = ? WHERE id = ? AND status = 'pending'";

    sqlite3_stmt* stmt = nullptr;
    int rc = sqlite3_prepare_v2(db_, sql, -1, &stmt, nullptr);
    if (rc != SQLITE_OK) {
        return Result<void>::err("Failed to prepare: " + std::string(sqlite3_errmsg(db_)));
    }

    sqlite3_bind_text(stmt, 1, approver.c_str(), -1, SQLITE_TRANSIENT);
    sqlite3_bind_text(stmt, 2, now.c_str(), -1, SQLITE_TRANSIENT);
    sqlite3_bind_text(stmt, 3, id.c_str(), -1, SQLITE_TRANSIENT);

    rc = sqlite3_step(stmt);
    sqlite3_finalize(stmt);

    if (rc != SQLITE_DONE) {
        return Result<void>::err("Failed to approve: " + std::string(sqlite3_errmsg(db_)));
    }

    if (sqlite3_changes(db_) == 0) {
        return Result<void>::err("No pending request found with id: " + id);
    }

    return Result<void>::ok();
}

Result<void> ApprovalDB::reject(const std::string& id, const std::string& approver,
                                 const std::string& reason) {
    std::string now = audit::now_iso8601();
    const char* sql = "UPDATE approval_requests SET status = 'rejected', "
                      "approver = ?, resolved_at = ?, rejection_reason = ? "
                      "WHERE id = ? AND status = 'pending'";

    sqlite3_stmt* stmt = nullptr;
    int rc = sqlite3_prepare_v2(db_, sql, -1, &stmt, nullptr);
    if (rc != SQLITE_OK) {
        return Result<void>::err("Failed to prepare: " + std::string(sqlite3_errmsg(db_)));
    }

    sqlite3_bind_text(stmt, 1, approver.c_str(), -1, SQLITE_TRANSIENT);
    sqlite3_bind_text(stmt, 2, now.c_str(), -1, SQLITE_TRANSIENT);
    sqlite3_bind_text(stmt, 3, reason.c_str(), -1, SQLITE_TRANSIENT);
    sqlite3_bind_text(stmt, 4, id.c_str(), -1, SQLITE_TRANSIENT);

    rc = sqlite3_step(stmt);
    sqlite3_finalize(stmt);

    if (rc != SQLITE_DONE) {
        return Result<void>::err("Failed to reject: " + std::string(sqlite3_errmsg(db_)));
    }

    if (sqlite3_changes(db_) == 0) {
        return Result<void>::err("No pending request found with id: " + id);
    }

    return Result<void>::ok();
}

Result<std::optional<ApprovalRequest>> ApprovalDB::get_request(const std::string& id) {
    const char* sql = "SELECT id, agent_name, tool_name, arguments, requesting_user, "
                      "reason, created_at, status, approver, resolved_at, rejection_reason "
                      "FROM approval_requests WHERE id = ?";

    sqlite3_stmt* stmt = nullptr;
    int rc = sqlite3_prepare_v2(db_, sql, -1, &stmt, nullptr);
    if (rc != SQLITE_OK) {
        return Result<std::optional<ApprovalRequest>>::err(
            "Failed to query: " + std::string(sqlite3_errmsg(db_)));
    }

    sqlite3_bind_text(stmt, 1, id.c_str(), -1, SQLITE_TRANSIENT);

    std::optional<ApprovalRequest> result;
    if (sqlite3_step(stmt) == SQLITE_ROW) {
        result = read_row(stmt);
    }
    sqlite3_finalize(stmt);

    return Result<std::optional<ApprovalRequest>>::ok(std::move(result));
}

} // namespace gnucash
