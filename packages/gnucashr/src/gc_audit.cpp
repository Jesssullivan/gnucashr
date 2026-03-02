// Audit trail for gnucashr R package
// Vendored from lib/gnucash-core/src/audit.cpp
// Added Windows compatibility guards for gmtime_r/gmtime_s

#include "gnucash/audit.h"
#include <sqlite3.h>
#include <ctime>
#include <iomanip>
#include <sstream>
#include <vector>

namespace gnucash {
namespace audit {

// ========================================================================
// Helper Functions
// ========================================================================

std::string now_iso8601() {
    auto now = std::time(nullptr);
    std::tm tm;
#ifdef _WIN32
    gmtime_s(&tm, &now);
#else
    gmtime_r(&now, &tm);
#endif
    std::ostringstream oss;
    oss << std::put_time(&tm, "%Y-%m-%dT%H:%M:%SZ");
    return oss.str();
}

std::string classification_to_string(Classification c) {
    switch (c) {
        case Classification::READ: return "read";
        case Classification::WRITE: return "write";
    }
    return "unknown";
}

std::string operation_to_string(Operation op) {
    switch (op) {
        case Operation::NONE: return "none";
        case Operation::CREATE: return "create";
        case Operation::UPDATE: return "update";
        case Operation::DELETE: return "delete";
        case Operation::VOID: return "void";
    }
    return "unknown";
}

std::string entity_type_to_string(EntityType et) {
    switch (et) {
        case EntityType::NONE: return "none";
        case EntityType::TRANSACTION: return "transaction";
        case EntityType::ACCOUNT: return "account";
        case EntityType::SPLIT: return "split";
        case EntityType::COMMODITY: return "commodity";
        case EntityType::PRICE: return "price";
        case EntityType::BOOK: return "book";
    }
    return "unknown";
}

std::string result_status_to_string(ResultStatus rs) {
    switch (rs) {
        case ResultStatus::SUCCESS: return "success";
        case ResultStatus::ERROR: return "error";
    }
    return "unknown";
}

std::string authorization_level_to_string(AuthorizationLevel al) {
    switch (al) {
        case AuthorizationLevel::AUTO: return "auto";
        case AuthorizationLevel::APPROVE: return "approve";
        case AuthorizationLevel::REVIEW: return "review";
    }
    return "unknown";
}

Classification parse_classification(const std::string& s) {
    if (s == "write") return Classification::WRITE;
    return Classification::READ;
}

Operation parse_operation(const std::string& s) {
    if (s == "create") return Operation::CREATE;
    if (s == "update") return Operation::UPDATE;
    if (s == "delete") return Operation::DELETE;
    if (s == "void") return Operation::VOID;
    return Operation::NONE;
}

EntityType parse_entity_type(const std::string& s) {
    if (s == "transaction") return EntityType::TRANSACTION;
    if (s == "account") return EntityType::ACCOUNT;
    if (s == "split") return EntityType::SPLIT;
    if (s == "commodity") return EntityType::COMMODITY;
    if (s == "price") return EntityType::PRICE;
    if (s == "book") return EntityType::BOOK;
    return EntityType::NONE;
}

ResultStatus parse_result_status(const std::string& s) {
    if (s == "error") return ResultStatus::ERROR;
    return ResultStatus::SUCCESS;
}

AuthorizationLevel parse_authorization_level(const std::string& s) {
    if (s == "approve") return AuthorizationLevel::APPROVE;
    if (s == "review") return AuthorizationLevel::REVIEW;
    return AuthorizationLevel::AUTO;
}

// ========================================================================
// AuditLogger Implementation
// ========================================================================

AuditLogger::AuditLogger(void* db) : db_(db) {}

AuditLogger::~AuditLogger() {
    if (db_) {
        sqlite3_close(static_cast<sqlite3*>(db_));
    }
}

AuditLogger::AuditLogger(AuditLogger&& other) noexcept : db_(other.db_) {
    other.db_ = nullptr;
}

AuditLogger& AuditLogger::operator=(AuditLogger&& other) noexcept {
    if (this != &other) {
        if (db_) {
            sqlite3_close(static_cast<sqlite3*>(db_));
        }
        db_ = other.db_;
        other.db_ = nullptr;
    }
    return *this;
}

Result<AuditLogger> AuditLogger::open(const std::string& book_path) {
    std::string audit_path = book_path + ".audit.db";

    sqlite3* db = nullptr;
    int rc = sqlite3_open(audit_path.c_str(), &db);
    if (rc != SQLITE_OK) {
        std::string error = "Failed to open audit database: ";
        if (db) {
            error += sqlite3_errmsg(db);
            sqlite3_close(db);
        } else {
            error += "sqlite3_open returned nullptr";
        }
        return Result<AuditLogger>::err(error);
    }

    // Create schema if needed
    char* errmsg = nullptr;
    rc = sqlite3_exec(db, AUDIT_SCHEMA, nullptr, nullptr, &errmsg);
    if (rc != SQLITE_OK) {
        std::string error = "Failed to create audit schema: ";
        error += errmsg ? errmsg : "unknown error";
        sqlite3_free(errmsg);
        sqlite3_close(db);
        return Result<AuditLogger>::err(error);
    }

    return Result<AuditLogger>::ok(AuditLogger(db));
}

Result<void> AuditLogger::log(const AuditRecord& record) {
    sqlite3* db = static_cast<sqlite3*>(db_);

    const char* sql = R"SQL(
        INSERT INTO audit_log (
            timestamp, user_email, user_name, node_name,
            tool_name, classification, operation, entity_type,
            request_id, arguments_json,
            book_path, entity_guid,
            before_state_json, after_state_json,
            result_status, error_message,
            authorization_level, approval_guid,
            duration_ms, reasoning
        ) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
    )SQL";

    sqlite3_stmt* stmt = nullptr;
    int rc = sqlite3_prepare_v2(db, sql, -1, &stmt, nullptr);
    if (rc != SQLITE_OK) {
        return Result<void>::err("Failed to prepare audit insert: " +
            std::string(sqlite3_errmsg(db)));
    }

    // Bind parameters
    int idx = 1;
    sqlite3_bind_text(stmt, idx++, record.timestamp.c_str(), -1, SQLITE_TRANSIENT);

    // Identity (nullable)
    if (record.user_email)
        sqlite3_bind_text(stmt, idx++, record.user_email->c_str(), -1, SQLITE_TRANSIENT);
    else
        sqlite3_bind_null(stmt, idx++);

    if (record.user_name)
        sqlite3_bind_text(stmt, idx++, record.user_name->c_str(), -1, SQLITE_TRANSIENT);
    else
        sqlite3_bind_null(stmt, idx++);

    if (record.node_name)
        sqlite3_bind_text(stmt, idx++, record.node_name->c_str(), -1, SQLITE_TRANSIENT);
    else
        sqlite3_bind_null(stmt, idx++);

    // Operation
    sqlite3_bind_text(stmt, idx++, record.tool_name.c_str(), -1, SQLITE_TRANSIENT);
    sqlite3_bind_text(stmt, idx++, classification_to_string(record.classification).c_str(), -1, SQLITE_TRANSIENT);
    sqlite3_bind_text(stmt, idx++, operation_to_string(record.operation).c_str(), -1, SQLITE_TRANSIENT);
    sqlite3_bind_text(stmt, idx++, entity_type_to_string(record.entity_type).c_str(), -1, SQLITE_TRANSIENT);

    // Request
    if (record.request_id)
        sqlite3_bind_text(stmt, idx++, record.request_id->c_str(), -1, SQLITE_TRANSIENT);
    else
        sqlite3_bind_null(stmt, idx++);

    std::string args_json = record.arguments.dump();
    sqlite3_bind_text(stmt, idx++, args_json.c_str(), -1, SQLITE_TRANSIENT);

    // Resource
    sqlite3_bind_text(stmt, idx++, record.book_path.c_str(), -1, SQLITE_TRANSIENT);

    if (record.entity_guid)
        sqlite3_bind_text(stmt, idx++, record.entity_guid->c_str(), -1, SQLITE_TRANSIENT);
    else
        sqlite3_bind_null(stmt, idx++);

    // State
    if (record.before_state) {
        std::string before = record.before_state->dump();
        sqlite3_bind_text(stmt, idx++, before.c_str(), -1, SQLITE_TRANSIENT);
    } else {
        sqlite3_bind_null(stmt, idx++);
    }

    if (record.after_state) {
        std::string after = record.after_state->dump();
        sqlite3_bind_text(stmt, idx++, after.c_str(), -1, SQLITE_TRANSIENT);
    } else {
        sqlite3_bind_null(stmt, idx++);
    }

    // Result
    sqlite3_bind_text(stmt, idx++, result_status_to_string(record.result_status).c_str(), -1, SQLITE_TRANSIENT);

    if (record.error_message)
        sqlite3_bind_text(stmt, idx++, record.error_message->c_str(), -1, SQLITE_TRANSIENT);
    else
        sqlite3_bind_null(stmt, idx++);

    // Authorization (nullable)
    if (record.authorization_level) {
        sqlite3_bind_text(stmt, idx++,
            authorization_level_to_string(*record.authorization_level).c_str(),
            -1, SQLITE_TRANSIENT);
    } else {
        sqlite3_bind_null(stmt, idx++);
    }

    if (record.approval_guid)
        sqlite3_bind_text(stmt, idx++, record.approval_guid->c_str(), -1, SQLITE_TRANSIENT);
    else
        sqlite3_bind_null(stmt, idx++);

    // Metadata
    if (record.duration_ms)
        sqlite3_bind_int(stmt, idx++, *record.duration_ms);
    else
        sqlite3_bind_null(stmt, idx++);

    if (record.reasoning)
        sqlite3_bind_text(stmt, idx++, record.reasoning->c_str(), -1, SQLITE_TRANSIENT);
    else
        sqlite3_bind_null(stmt, idx++);

    // Execute
    rc = sqlite3_step(stmt);
    sqlite3_finalize(stmt);

    if (rc != SQLITE_DONE) {
        return Result<void>::err("Failed to insert audit record: " +
            std::string(sqlite3_errmsg(db)));
    }

    return Result<void>::ok();
}

Result<std::vector<AuditRecord>> AuditLogger::query(const QueryFilters& filters) {
    sqlite3* db = static_cast<sqlite3*>(db_);

    // Build query with filters
    std::ostringstream sql;
    sql << "SELECT timestamp, user_email, user_name, node_name, "
        << "tool_name, classification, operation, entity_type, "
        << "request_id, arguments_json, book_path, entity_guid, "
        << "before_state_json, after_state_json, result_status, error_message, "
        << "authorization_level, approval_guid, duration_ms, reasoning "
        << "FROM audit_log WHERE 1=1";

    std::vector<std::string> bind_values;

    if (filters.since) {
        sql << " AND timestamp >= ?";
        bind_values.push_back(*filters.since);
    }
    if (filters.until) {
        sql << " AND timestamp <= ?";
        bind_values.push_back(*filters.until);
    }
    if (filters.tool_name) {
        sql << " AND tool_name = ?";
        bind_values.push_back(*filters.tool_name);
    }
    if (filters.classification) {
        sql << " AND classification = ?";
        bind_values.push_back(classification_to_string(*filters.classification));
    }
    if (filters.user_email) {
        sql << " AND user_email = ?";
        bind_values.push_back(*filters.user_email);
    }
    if (filters.entity_guid) {
        sql << " AND entity_guid = ?";
        bind_values.push_back(*filters.entity_guid);
    }

    sql << " ORDER BY timestamp DESC LIMIT ?";
    bind_values.push_back(std::to_string(filters.limit));

    // Prepare statement
    sqlite3_stmt* stmt = nullptr;
    int rc = sqlite3_prepare_v2(db, sql.str().c_str(), -1, &stmt, nullptr);
    if (rc != SQLITE_OK) {
        return Result<std::vector<AuditRecord>>::err(
            "Failed to prepare audit query: " + std::string(sqlite3_errmsg(db)));
    }

    // Bind values
    for (size_t i = 0; i < bind_values.size(); ++i) {
        sqlite3_bind_text(stmt, i + 1, bind_values[i].c_str(), -1, SQLITE_TRANSIENT);
    }

    // Execute and collect results
    std::vector<AuditRecord> records;
    while ((rc = sqlite3_step(stmt)) == SQLITE_ROW) {
        AuditRecord record;

        int col = 0;
        record.timestamp = reinterpret_cast<const char*>(sqlite3_column_text(stmt, col++));

        // Identity (nullable)
        if (sqlite3_column_type(stmt, col) != SQLITE_NULL)
            record.user_email = reinterpret_cast<const char*>(sqlite3_column_text(stmt, col));
        col++;

        if (sqlite3_column_type(stmt, col) != SQLITE_NULL)
            record.user_name = reinterpret_cast<const char*>(sqlite3_column_text(stmt, col));
        col++;

        if (sqlite3_column_type(stmt, col) != SQLITE_NULL)
            record.node_name = reinterpret_cast<const char*>(sqlite3_column_text(stmt, col));
        col++;

        // Operation
        record.tool_name = reinterpret_cast<const char*>(sqlite3_column_text(stmt, col++));
        record.classification = parse_classification(
            reinterpret_cast<const char*>(sqlite3_column_text(stmt, col++)));
        record.operation = parse_operation(
            reinterpret_cast<const char*>(sqlite3_column_text(stmt, col++)));
        record.entity_type = parse_entity_type(
            reinterpret_cast<const char*>(sqlite3_column_text(stmt, col++)));

        // Request
        if (sqlite3_column_type(stmt, col) != SQLITE_NULL)
            record.request_id = reinterpret_cast<const char*>(sqlite3_column_text(stmt, col));
        col++;

        const char* args_json = reinterpret_cast<const char*>(sqlite3_column_text(stmt, col++));
        if (args_json) {
            record.arguments = json::parse(args_json);
        }

        // Resource
        record.book_path = reinterpret_cast<const char*>(sqlite3_column_text(stmt, col++));

        if (sqlite3_column_type(stmt, col) != SQLITE_NULL)
            record.entity_guid = reinterpret_cast<const char*>(sqlite3_column_text(stmt, col));
        col++;

        // State
        if (sqlite3_column_type(stmt, col) != SQLITE_NULL) {
            const char* before = reinterpret_cast<const char*>(sqlite3_column_text(stmt, col));
            record.before_state = json::parse(before);
        }
        col++;

        if (sqlite3_column_type(stmt, col) != SQLITE_NULL) {
            const char* after = reinterpret_cast<const char*>(sqlite3_column_text(stmt, col));
            record.after_state = json::parse(after);
        }
        col++;

        // Result
        record.result_status = parse_result_status(
            reinterpret_cast<const char*>(sqlite3_column_text(stmt, col++)));

        if (sqlite3_column_type(stmt, col) != SQLITE_NULL)
            record.error_message = reinterpret_cast<const char*>(sqlite3_column_text(stmt, col));
        col++;

        // Authorization
        if (sqlite3_column_type(stmt, col) != SQLITE_NULL) {
            record.authorization_level = parse_authorization_level(
                reinterpret_cast<const char*>(sqlite3_column_text(stmt, col)));
        }
        col++;

        if (sqlite3_column_type(stmt, col) != SQLITE_NULL)
            record.approval_guid = reinterpret_cast<const char*>(sqlite3_column_text(stmt, col));
        col++;

        // Metadata
        if (sqlite3_column_type(stmt, col) != SQLITE_NULL)
            record.duration_ms = sqlite3_column_int(stmt, col);
        col++;

        if (sqlite3_column_type(stmt, col) != SQLITE_NULL)
            record.reasoning = reinterpret_cast<const char*>(sqlite3_column_text(stmt, col));
        col++;

        records.push_back(std::move(record));
    }

    sqlite3_finalize(stmt);

    if (rc != SQLITE_DONE) {
        return Result<std::vector<AuditRecord>>::err(
            "Failed to query audit log: " + std::string(sqlite3_errmsg(db)));
    }

    return Result<std::vector<AuditRecord>>::ok(std::move(records));
}

Result<std::string> AuditLogger::export_aperture(const QueryFilters& filters) {
    auto records_result = query(filters);
    if (records_result.is_err()) {
        return Result<std::string>::err(records_result.unwrap_err());
    }

    auto records = records_result.unwrap();
    std::ostringstream output;

    for (const auto& record : records) {
        json line = {
            {"timestamp", record.timestamp},
            {"resource", "gnucash://" + record.book_path +
                (record.entity_guid ? "/" + entity_type_to_string(record.entity_type) +
                    "/" + *record.entity_guid : "")},
            {"action", record.tool_name},
            {"decision", record.result_status == ResultStatus::SUCCESS ? "allow" : "deny"},
            {"metadata", {
                {"tool", record.tool_name},
                {"classification", classification_to_string(record.classification)}
            }}
        };

        // Add identity if present
        if (record.user_email)
            line["user"] = *record.user_email;
        if (record.node_name)
            line["node"] = *record.node_name;

        // Add duration if present
        if (record.duration_ms)
            line["metadata"]["duration_ms"] = *record.duration_ms;

        output << line.dump() << "\n";
    }

    return Result<std::string>::ok(output.str());
}

} // namespace audit
} // namespace gnucash
