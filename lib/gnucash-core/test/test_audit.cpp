#include <catch2/catch_test_macros.hpp>
#include "gnucash/audit.h"
#include <nlohmann/json.hpp>
#include <string>
#include <cstdio>
#include <filesystem>

using json = nlohmann::json;
using namespace gnucash::audit;

// Helper: create a temporary book path for audit DB
static std::string temp_book_path() {
    static int counter = 0;
    return std::string(FIXTURE_DIR) + "/tmp_audit_test_" + std::to_string(counter++) + ".gnucash";
}

// Helper: clean up audit DB file
static void cleanup_audit_db(const std::string& book_path) {
    std::string db_path = book_path + ".audit.db";
    std::remove(db_path.c_str());
    std::remove(book_path.c_str());
}

// ========================================================================
// Enum conversion round-trips
// ========================================================================

TEST_CASE("classification: string round-trip", "[audit][enum]") {
    REQUIRE(parse_classification(classification_to_string(Classification::READ)) == Classification::READ);
    REQUIRE(parse_classification(classification_to_string(Classification::WRITE)) == Classification::WRITE);
}

TEST_CASE("operation: string round-trip", "[audit][enum]") {
    REQUIRE(parse_operation(operation_to_string(Operation::NONE)) == Operation::NONE);
    REQUIRE(parse_operation(operation_to_string(Operation::CREATE)) == Operation::CREATE);
    REQUIRE(parse_operation(operation_to_string(Operation::UPDATE)) == Operation::UPDATE);
    REQUIRE(parse_operation(operation_to_string(Operation::DELETE)) == Operation::DELETE);
    REQUIRE(parse_operation(operation_to_string(Operation::VOID)) == Operation::VOID);
}

TEST_CASE("entity_type: string round-trip", "[audit][enum]") {
    REQUIRE(parse_entity_type(entity_type_to_string(EntityType::NONE)) == EntityType::NONE);
    REQUIRE(parse_entity_type(entity_type_to_string(EntityType::TRANSACTION)) == EntityType::TRANSACTION);
    REQUIRE(parse_entity_type(entity_type_to_string(EntityType::ACCOUNT)) == EntityType::ACCOUNT);
}

TEST_CASE("result_status: string round-trip", "[audit][enum]") {
    REQUIRE(parse_result_status(result_status_to_string(ResultStatus::SUCCESS)) == ResultStatus::SUCCESS);
    REQUIRE(parse_result_status(result_status_to_string(ResultStatus::ERROR)) == ResultStatus::ERROR);
}

TEST_CASE("authorization_level: string round-trip", "[audit][enum]") {
    REQUIRE(parse_authorization_level(authorization_level_to_string(AuthorizationLevel::AUTO)) == AuthorizationLevel::AUTO);
    REQUIRE(parse_authorization_level(authorization_level_to_string(AuthorizationLevel::REVIEW)) == AuthorizationLevel::REVIEW);
    REQUIRE(parse_authorization_level(authorization_level_to_string(AuthorizationLevel::APPROVE)) == AuthorizationLevel::APPROVE);
}

// ========================================================================
// Timestamp
// ========================================================================

TEST_CASE("now_iso8601: format", "[audit][timestamp]") {
    auto ts = now_iso8601();
    // Should be like "2026-03-02T..."
    REQUIRE(ts.size() >= 19);
    REQUIRE(ts[4] == '-');
    REQUIRE(ts[7] == '-');
    REQUIRE(ts[10] == 'T');
}

TEST_CASE("now_iso8601: two calls are ordered", "[audit][timestamp]") {
    auto t1 = now_iso8601();
    auto t2 = now_iso8601();
    REQUIRE(t1 <= t2);
}

// ========================================================================
// AuditLogger lifecycle
// ========================================================================

TEST_CASE("AuditLogger::open creates database", "[audit][logger]") {
    auto path = temp_book_path();
    auto result = AuditLogger::open(path);
    REQUIRE(result.is_ok());

    // Verify the .audit.db file exists
    std::string db_path = path + ".audit.db";
    REQUIRE(std::filesystem::exists(db_path));

    cleanup_audit_db(path);
}

TEST_CASE("AuditLogger: log and query round-trip", "[audit][logger]") {
    auto path = temp_book_path();
    auto logger_result = AuditLogger::open(path);
    REQUIRE(logger_result.is_ok());
    auto logger = std::move(logger_result.unwrap());

    // Log a record
    AuditRecord record;
    record.timestamp = now_iso8601();
    record.tool_name = "gnucash_get_accounts";
    record.classification = Classification::READ;
    record.operation = Operation::NONE;
    record.entity_type = EntityType::NONE;
    record.arguments = json::object();
    record.book_path = path;
    record.result_status = ResultStatus::SUCCESS;
    record.duration_ms = 42;

    auto log_result = logger.log(record);
    REQUIRE(log_result.is_ok());

    // Query all records
    AuditLogger::QueryFilters filters;
    auto query_result = logger.query(filters);
    REQUIRE(query_result.is_ok());
    auto records = query_result.unwrap();

    REQUIRE(records.size() == 1);
    REQUIRE(records[0].tool_name == "gnucash_get_accounts");
    REQUIRE(records[0].classification == Classification::READ);
    REQUIRE(records[0].result_status == ResultStatus::SUCCESS);

    cleanup_audit_db(path);
}

TEST_CASE("AuditLogger: log multiple and query with tool_name filter", "[audit][logger]") {
    auto path = temp_book_path();
    auto logger_result = AuditLogger::open(path);
    REQUIRE(logger_result.is_ok());
    auto logger = std::move(logger_result.unwrap());

    // Log three different tool calls
    for (const auto& tool : {"gnucash_open", "gnucash_get_accounts", "gnucash_get_transactions"}) {
        AuditRecord record;
        record.timestamp = now_iso8601();
        record.tool_name = tool;
        record.classification = Classification::READ;
        record.operation = Operation::NONE;
        record.entity_type = EntityType::NONE;
        record.arguments = json::object();
        record.book_path = path;
        record.result_status = ResultStatus::SUCCESS;
        logger.log(record);
    }

    // Query without filter -> all 3
    AuditLogger::QueryFilters all_filters;
    auto all = logger.query(all_filters);
    REQUIRE(all.is_ok());
    REQUIRE(all.unwrap().size() == 3);

    // Query with tool_name filter -> 1
    AuditLogger::QueryFilters tool_filter;
    tool_filter.tool_name = "gnucash_get_accounts";
    auto filtered = logger.query(tool_filter);
    REQUIRE(filtered.is_ok());
    REQUIRE(filtered.unwrap().size() == 1);
    REQUIRE(filtered.unwrap()[0].tool_name == "gnucash_get_accounts");

    cleanup_audit_db(path);
}

TEST_CASE("AuditLogger: classification filter", "[audit][logger]") {
    auto path = temp_book_path();
    auto logger_result = AuditLogger::open(path);
    REQUIRE(logger_result.is_ok());
    auto logger = std::move(logger_result.unwrap());

    // Log one read and one write
    AuditRecord read_record;
    read_record.timestamp = now_iso8601();
    read_record.tool_name = "gnucash_get_accounts";
    read_record.classification = Classification::READ;
    read_record.operation = Operation::NONE;
    read_record.entity_type = EntityType::NONE;
    read_record.arguments = json::object();
    read_record.book_path = path;
    read_record.result_status = ResultStatus::SUCCESS;
    logger.log(read_record);

    AuditRecord write_record;
    write_record.timestamp = now_iso8601();
    write_record.tool_name = "gnucash_post_transaction";
    write_record.classification = Classification::WRITE;
    write_record.operation = Operation::CREATE;
    write_record.entity_type = EntityType::TRANSACTION;
    write_record.arguments = json::object();
    write_record.book_path = path;
    write_record.result_status = ResultStatus::SUCCESS;
    logger.log(write_record);

    // Filter for writes only
    AuditLogger::QueryFilters write_filter;
    write_filter.classification = Classification::WRITE;
    auto writes = logger.query(write_filter);
    REQUIRE(writes.is_ok());
    REQUIRE(writes.unwrap().size() == 1);
    REQUIRE(writes.unwrap()[0].tool_name == "gnucash_post_transaction");

    cleanup_audit_db(path);
}

TEST_CASE("AuditLogger: limit filter", "[audit][logger]") {
    auto path = temp_book_path();
    auto logger_result = AuditLogger::open(path);
    REQUIRE(logger_result.is_ok());
    auto logger = std::move(logger_result.unwrap());

    // Log 5 records
    for (int i = 0; i < 5; i++) {
        AuditRecord record;
        record.timestamp = now_iso8601();
        record.tool_name = "gnucash_info";
        record.classification = Classification::READ;
        record.operation = Operation::NONE;
        record.entity_type = EntityType::NONE;
        record.arguments = json::object();
        record.book_path = path;
        record.result_status = ResultStatus::SUCCESS;
        logger.log(record);
    }

    // Query with limit 2
    AuditLogger::QueryFilters limited;
    limited.limit = 2;
    auto result = logger.query(limited);
    REQUIRE(result.is_ok());
    REQUIRE(result.unwrap().size() == 2);

    cleanup_audit_db(path);
}

TEST_CASE("AuditLogger: error records", "[audit][logger]") {
    auto path = temp_book_path();
    auto logger_result = AuditLogger::open(path);
    REQUIRE(logger_result.is_ok());
    auto logger = std::move(logger_result.unwrap());

    AuditRecord record;
    record.timestamp = now_iso8601();
    record.tool_name = "gnucash_open";
    record.classification = Classification::READ;
    record.operation = Operation::NONE;
    record.entity_type = EntityType::NONE;
    record.arguments = {{"path", "/nonexistent"}};
    record.book_path = path;
    record.result_status = ResultStatus::ERROR;
    record.error_message = "file not found";
    logger.log(record);

    AuditLogger::QueryFilters filters;
    auto result = logger.query(filters);
    REQUIRE(result.is_ok());
    auto records = result.unwrap();
    REQUIRE(records.size() == 1);
    REQUIRE(records[0].result_status == ResultStatus::ERROR);
    REQUIRE(records[0].error_message.has_value());
    REQUIRE(records[0].error_message.value() == "file not found");

    cleanup_audit_db(path);
}

TEST_CASE("AuditLogger: entity guid tracking", "[audit][logger]") {
    auto path = temp_book_path();
    auto logger_result = AuditLogger::open(path);
    REQUIRE(logger_result.is_ok());
    auto logger = std::move(logger_result.unwrap());

    AuditRecord record;
    record.timestamp = now_iso8601();
    record.tool_name = "gnucash_post_transaction";
    record.classification = Classification::WRITE;
    record.operation = Operation::CREATE;
    record.entity_type = EntityType::TRANSACTION;
    record.arguments = json::object();
    record.book_path = path;
    record.entity_guid = "a0b1c2d3e4f5a6b7c8d9e0f1a2b3c4d5";
    record.result_status = ResultStatus::SUCCESS;
    record.duration_ms = 15;
    logger.log(record);

    // Filter by entity guid
    AuditLogger::QueryFilters filters;
    filters.entity_guid = "a0b1c2d3e4f5a6b7c8d9e0f1a2b3c4d5";
    auto result = logger.query(filters);
    REQUIRE(result.is_ok());
    REQUIRE(result.unwrap().size() == 1);
    REQUIRE(result.unwrap()[0].entity_guid.value() == "a0b1c2d3e4f5a6b7c8d9e0f1a2b3c4d5");

    cleanup_audit_db(path);
}

// ========================================================================
// Aperture Export
// ========================================================================

TEST_CASE("AuditLogger: export_aperture produces JSONL", "[audit][aperture]") {
    auto path = temp_book_path();
    auto logger_result = AuditLogger::open(path);
    REQUIRE(logger_result.is_ok());
    auto logger = std::move(logger_result.unwrap());

    AuditRecord record;
    record.timestamp = now_iso8601();
    record.tool_name = "gnucash_get_accounts";
    record.classification = Classification::READ;
    record.operation = Operation::NONE;
    record.entity_type = EntityType::NONE;
    record.arguments = json::object();
    record.book_path = path;
    record.result_status = ResultStatus::SUCCESS;
    record.user_email = "test@example.com";
    record.node_name = "dev-laptop";
    logger.log(record);

    AuditLogger::QueryFilters filters;
    auto export_result = logger.export_aperture(filters);
    REQUIRE(export_result.is_ok());

    // Parse JSONL output (one line per record)
    auto jsonl = export_result.unwrap();
    REQUIRE(!jsonl.empty());

    // Parse first line as JSON
    auto line_end = jsonl.find('\n');
    std::string first_line = (line_end != std::string::npos) ? jsonl.substr(0, line_end) : jsonl;
    auto parsed = json::parse(first_line);

    REQUIRE(parsed.contains("timestamp"));
    REQUIRE(parsed.contains("action"));
    REQUIRE(parsed["action"] == "gnucash_get_accounts");
    REQUIRE(parsed.contains("decision"));
    REQUIRE(parsed["decision"] == "allow");
    REQUIRE(parsed["metadata"]["tool"] == "gnucash_get_accounts");
    REQUIRE(parsed["user"] == "test@example.com");
    REQUIRE(parsed["node"] == "dev-laptop");

    cleanup_audit_db(path);
}

// ========================================================================
// Schema Constants
// ========================================================================

TEST_CASE("AUDIT_SCHEMA: contains required tables and indexes", "[audit][schema]") {
    std::string schema(AUDIT_SCHEMA);
    REQUIRE(schema.find("CREATE TABLE IF NOT EXISTS audit_log") != std::string::npos);
    REQUIRE(schema.find("idx_audit_timestamp") != std::string::npos);
    REQUIRE(schema.find("idx_audit_tool") != std::string::npos);
    REQUIRE(schema.find("idx_audit_user") != std::string::npos);
    REQUIRE(schema.find("idx_audit_entity") != std::string::npos);
    REQUIRE(schema.find("idx_audit_classification") != std::string::npos);
}
