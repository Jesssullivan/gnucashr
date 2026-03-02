#pragma once
// Approval queue for write operations requiring human authorization
// Shared SQLite DB at {book}.approvals.db
// Vendored from lib/gnucash-core for R package use

#include "result.h"
#include <string>
#include <vector>
#include <optional>
#include <nlohmann/json.hpp>

struct sqlite3;

namespace gnucash {

using json = nlohmann::json;

struct ApprovalRequest {
    std::string id;              // GUID
    std::string agent_name;
    std::string tool_name;
    json arguments;
    std::string requesting_user;
    std::string reason;          // Why approval is needed
    std::string created_at;      // ISO 8601
    std::string status;          // "pending", "approved", "rejected", "expired"
    std::optional<std::string> approver;
    std::optional<std::string> resolved_at;
    std::optional<std::string> rejection_reason;
};

class ApprovalDB {
public:
    ~ApprovalDB();
    ApprovalDB(ApprovalDB&& other) noexcept;
    ApprovalDB& operator=(ApprovalDB&& other) noexcept;
    ApprovalDB(const ApprovalDB&) = delete;
    ApprovalDB& operator=(const ApprovalDB&) = delete;

    // Open or create approval DB for a book
    static Result<ApprovalDB> open(const std::string& book_path);

    // Create a new approval request, returns the request ID
    Result<std::string> create_request(const std::string& agent_name,
                                        const std::string& tool_name,
                                        const json& arguments,
                                        const std::string& requesting_user,
                                        const std::string& reason);

    // List pending requests
    Result<std::vector<ApprovalRequest>> pending_requests(int limit = 50);

    // Approve a pending request
    Result<void> approve(const std::string& id, const std::string& approver);

    // Reject a pending request
    Result<void> reject(const std::string& id, const std::string& approver,
                        const std::string& reason);

    // Get a specific request
    Result<std::optional<ApprovalRequest>> get_request(const std::string& id);

    // DB file path
    const std::string& db_path() const { return db_path_; }

private:
    ApprovalDB() = default;
    sqlite3* db_ = nullptr;
    std::string db_path_;
};

// Schema for approval database
constexpr const char* APPROVAL_SCHEMA = R"SQL(
CREATE TABLE IF NOT EXISTS approval_requests (
    id TEXT PRIMARY KEY,
    agent_name TEXT NOT NULL,
    tool_name TEXT NOT NULL,
    arguments TEXT NOT NULL,
    requesting_user TEXT NOT NULL,
    reason TEXT NOT NULL,
    created_at TEXT NOT NULL,
    status TEXT NOT NULL DEFAULT 'pending',
    approver TEXT,
    resolved_at TEXT,
    rejection_reason TEXT
);

CREATE INDEX IF NOT EXISTS idx_approval_status ON approval_requests(status);
CREATE INDEX IF NOT EXISTS idx_approval_agent ON approval_requests(agent_name);
CREATE INDEX IF NOT EXISTS idx_approval_created ON approval_requests(created_at);
)SQL";

} // namespace gnucash
