#pragma once
// Audit Trail System for gnucash-mcp
// Logs all MCP tool calls with before/after state capture
// Aperture-compatible schema for future Tailscale integration
//
// Audit logs stored in: {book_path}.audit.db (separate SQLite database)
// Format designed for export to Tailscale Aperture for policy enforcement

#include <string>
#include <optional>
#include <nlohmann/json.hpp>
#include "result.h"

namespace gnucash {
namespace audit {

using json = nlohmann::json;

// ========================================================================
// Audit Record Types
// ========================================================================

/// Classification of operation
enum class Classification {
    READ,    // Read-only operation (auto tier)
    WRITE    // Write operation (approve tier)
};

/// Operation type (for write operations)
enum class Operation {
    NONE,       // Not a mutation
    CREATE,     // Create new entity
    UPDATE,     // Update existing entity
    DELETE,     // Delete entity
    VOID        // Void transaction
};

/// Entity type being operated on
enum class EntityType {
    NONE,
    TRANSACTION,
    ACCOUNT,
    SPLIT,
    COMMODITY,
    PRICE,
    BOOK
};

/// Result status
enum class ResultStatus {
    SUCCESS,
    ERROR
};

/// Authorization level (for future agent work)
enum class AuthorizationLevel {
    AUTO,       // Automatically approved
    APPROVE,    // Requires approval
    REVIEW      // Requires human review
};

/// Audit record structure
struct AuditRecord {
    // Timestamp
    std::string timestamp;              // ISO 8601 UTC

    // Identity (Aperture-compatible, nullable for local dev)
    std::optional<std::string> user_email;    // Tailscale identity email
    std::optional<std::string> user_name;     // Display name
    std::optional<std::string> node_name;     // Tailscale node hostname

    // Operation
    std::string tool_name;              // MCP tool name (e.g., "gnucash_open")
    Classification classification;      // READ or WRITE
    Operation operation;                // CREATE, UPDATE, DELETE, VOID
    EntityType entity_type;             // TRANSACTION, ACCOUNT, etc.

    // Request
    std::optional<std::string> request_id;    // MCP call id
    json arguments;                     // JSON serialized params

    // Resource
    std::string book_path;              // GnuCash file path
    std::optional<std::string> entity_guid;   // Affected entity GUID

    // State
    std::optional<json> before_state;   // Snapshot before operation
    std::optional<json> after_state;    // Snapshot after operation

    // Result
    ResultStatus result_status;         // SUCCESS or ERROR
    std::optional<std::string> error_message;

    // Authorization (future)
    std::optional<AuthorizationLevel> authorization_level;
    std::optional<std::string> approval_guid;

    // Metadata
    std::optional<int> duration_ms;     // Execution time
    std::optional<std::string> reasoning;     // LLM reasoning (for categorization)
};

// ========================================================================
// Audit Logger
// ========================================================================

/// Audit logger instance (manages SQLite connection)
class AuditLogger {
public:
    /// Open audit database for a book
    /// Creates {book_path}.audit.db if it doesn't exist
    static Result<AuditLogger> open(const std::string& book_path);

    /// Close audit database
    ~AuditLogger();

    // Disable copy, enable move
    AuditLogger(const AuditLogger&) = delete;
    AuditLogger& operator=(const AuditLogger&) = delete;
    AuditLogger(AuditLogger&&) noexcept;
    AuditLogger& operator=(AuditLogger&&) noexcept;

    /// Log a tool call
    Result<void> log(const AuditRecord& record);

    /// Query audit log with filters
    struct QueryFilters {
        std::optional<std::string> since;          // ISO 8601 timestamp
        std::optional<std::string> until;          // ISO 8601 timestamp
        std::optional<std::string> tool_name;      // Exact match
        std::optional<Classification> classification;
        std::optional<std::string> user_email;     // Exact match
        std::optional<std::string> entity_guid;    // Exact match
        int limit = 100;                           // Max records to return
    };

    /// Query audit log
    Result<std::vector<AuditRecord>> query(const QueryFilters& filters);

    /// Export audit log to Aperture JSONL format
    Result<std::string> export_aperture(const QueryFilters& filters);

private:
    AuditLogger(void* db);
    void* db_;  // SQLite3 connection (opaque pointer)
};

// ========================================================================
// Helper Functions
// ========================================================================

/// Get current timestamp in ISO 8601 UTC format
std::string now_iso8601();

/// Convert Classification to string
std::string classification_to_string(Classification c);

/// Convert Operation to string
std::string operation_to_string(Operation op);

/// Convert EntityType to string
std::string entity_type_to_string(EntityType et);

/// Convert ResultStatus to string
std::string result_status_to_string(ResultStatus rs);

/// Convert AuthorizationLevel to string
std::string authorization_level_to_string(AuthorizationLevel al);

/// Parse Classification from string
Classification parse_classification(const std::string& s);

/// Parse Operation from string
Operation parse_operation(const std::string& s);

/// Parse EntityType from string
EntityType parse_entity_type(const std::string& s);

/// Parse ResultStatus from string
ResultStatus parse_result_status(const std::string& s);

/// Parse AuthorizationLevel from string
AuthorizationLevel parse_authorization_level(const std::string& s);

// ========================================================================
// Schema Constants
// ========================================================================

/// SQL schema for audit database
constexpr const char* AUDIT_SCHEMA = R"SQL(
CREATE TABLE IF NOT EXISTS audit_log (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    timestamp TEXT NOT NULL,

    -- Identity (Aperture-compatible)
    user_email TEXT,
    user_name TEXT,
    node_name TEXT,

    -- Operation
    tool_name TEXT NOT NULL,
    classification TEXT NOT NULL,
    operation TEXT,
    entity_type TEXT,

    -- Request
    request_id TEXT,
    arguments_json TEXT,

    -- Resource
    book_path TEXT NOT NULL,
    entity_guid TEXT,

    -- State
    before_state_json TEXT,
    after_state_json TEXT,

    -- Result
    result_status TEXT NOT NULL,
    error_message TEXT,

    -- Authorization (future)
    authorization_level TEXT,
    approval_guid TEXT,

    -- Metadata
    duration_ms INTEGER,
    reasoning TEXT
);

CREATE INDEX IF NOT EXISTS idx_audit_timestamp ON audit_log(timestamp);
CREATE INDEX IF NOT EXISTS idx_audit_tool ON audit_log(tool_name);
CREATE INDEX IF NOT EXISTS idx_audit_user ON audit_log(user_email);
CREATE INDEX IF NOT EXISTS idx_audit_entity ON audit_log(entity_guid);
CREATE INDEX IF NOT EXISTS idx_audit_classification ON audit_log(classification);
)SQL";

} // namespace audit
} // namespace gnucash
