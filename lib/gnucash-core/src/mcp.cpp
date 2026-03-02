#include "gnucash/mcp.h"
#include "gnucash/audit.h"
#include "gnucash/security.h"
#include "gnucash/identity.h"
#include "json_api.h"
#include <iostream>
#include <chrono>

namespace gnucash {
namespace mcp {

// ========================================================================
// Global State
// ========================================================================

// Audit logger (initialized when book is opened)
static std::optional<audit::AuditLogger> g_audit_logger;

// Current book path (for audit logging)
static std::string g_book_path;

// Agent configuration (filters available tools)
static std::optional<dhall::AgentConfig> g_agent_config;

// Identity for audit trail
static std::optional<Identity> g_identity;

// Security policy and rate limiter
static std::optional<SecurityPolicy> g_security_policy;
static RateLimiter g_rate_limiter;

// ========================================================================
// Protocol Detection
// ========================================================================

ProtocolMode detect_protocol(const json& request) {
    // MCP uses JSON-RPC 2.0 with "jsonrpc" field
    if (request.contains("jsonrpc") && request["jsonrpc"] == "2.0") {
        return ProtocolMode::MCP;
    }
    // Legacy protocol uses "method" without "jsonrpc"
    return ProtocolMode::LEGACY;
}

// ========================================================================
// JSON-RPC 2.0 Helpers
// ========================================================================

json make_jsonrpc_error(int code, const std::string& message, const json& id) {
    return {
        {"jsonrpc", JSONRPC_VERSION},
        {"error", {
            {"code", code},
            {"message", message}
        }},
        {"id", id}
    };
}

json make_jsonrpc_result(const json& result, const json& id) {
    return {
        {"jsonrpc", JSONRPC_VERSION},
        {"result", result},
        {"id", id}
    };
}

// ========================================================================
// MCP Handlers
// ========================================================================

json handle_initialize(const json& params, const json& id) {
    InitializeResult result;
    result.protocol_version = MCP_PROTOCOL_VERSION;
    result.capabilities.supports_tools = true;
    result.capabilities.supports_prompts = false;
    result.capabilities.supports_resources = false;
    result.capabilities.supports_sampling = false;
    result.server_info.name = "gnucash-mcp";
    result.server_info.version = "0.3.0";

    json response_data = {
        {"protocolVersion", result.protocol_version},
        {"capabilities", {
            {"tools", result.capabilities.supports_tools},
            {"prompts", result.capabilities.supports_prompts},
            {"resources", result.capabilities.supports_resources},
            {"sampling", result.capabilities.supports_sampling}
        }},
        {"serverInfo", {
            {"name", result.server_info.name},
            {"version", result.server_info.version}
        }}
    };

    return make_jsonrpc_result(response_data, id);
}

void handle_initialized() {
    // Notification - no response needed
    // Could log or set server state here if needed
}

json handle_tools_list(const json& id) {
    auto tools = get_tool_definitions();

    json tools_array = json::array();
    for (const auto& tool : tools) {
        json schema = {
            {"type", tool.input_schema.type},
            {"properties", tool.input_schema.properties}
        };
        if (!tool.input_schema.required.empty()) {
            schema["required"] = tool.input_schema.required;
        }
        if (tool.input_schema.additional_properties.has_value()) {
            schema["additionalProperties"] = *tool.input_schema.additional_properties;
        }

        tools_array.push_back({
            {"name", tool.name},
            {"description", tool.description},
            {"inputSchema", schema}
        });
    }

    return make_jsonrpc_result({{"tools", tools_array}}, id);
}

json handle_tools_call(const json& params, const json& id) {
    // Extract tool name and arguments
    if (!params.contains("name") || !params.contains("arguments")) {
        return make_jsonrpc_error(error_codes::INVALID_PARAMS,
            "Missing 'name' or 'arguments' in tools/call params", id);
    }

    std::string tool_name = params["name"];
    json arguments = params["arguments"];

    // Start timing
    auto start_time = std::chrono::steady_clock::now();

    // Initialize audit record
    audit::AuditRecord audit_record;
    audit_record.timestamp = audit::now_iso8601();
    audit_record.tool_name = tool_name;
    audit_record.arguments = arguments;
    audit_record.book_path = g_book_path;
    if (!id.is_null() && id.is_string()) {
        audit_record.request_id = id.get<std::string>();
    } else if (!id.is_null() && id.is_number()) {
        audit_record.request_id = std::to_string(id.get<int>());
    }

    // Classify operation (write operations)
    bool is_write = (tool_name == "gnucash_create_account" ||
                     tool_name == "gnucash_post_transaction" ||
                     tool_name == "gnucash_delete_transaction" ||
                     tool_name == "gnucash_void_transaction" ||
                     tool_name == "gnucash_import_ofx" ||
                     tool_name == "gnucash_import_csv" ||
                     tool_name == "gnucash_set_slot" ||
                     tool_name == "gnucash_update_split" ||
                     tool_name == "gnucash_reconcile_account");

    audit_record.classification = is_write ?
        audit::Classification::WRITE : audit::Classification::READ;

    // Determine operation type and entity type for write operations
    if (tool_name == "gnucash_create_account") {
        audit_record.operation = audit::Operation::CREATE;
        audit_record.entity_type = audit::EntityType::ACCOUNT;
    } else if (tool_name == "gnucash_post_transaction") {
        audit_record.operation = audit::Operation::CREATE;
        audit_record.entity_type = audit::EntityType::TRANSACTION;
    } else if (tool_name == "gnucash_delete_transaction") {
        audit_record.operation = audit::Operation::DELETE;
        audit_record.entity_type = audit::EntityType::TRANSACTION;
    } else if (tool_name == "gnucash_void_transaction") {
        audit_record.operation = audit::Operation::VOID;
        audit_record.entity_type = audit::EntityType::TRANSACTION;
    } else if (tool_name == "gnucash_import_ofx" || tool_name == "gnucash_import_csv") {
        audit_record.operation = audit::Operation::CREATE;
        audit_record.entity_type = audit::EntityType::TRANSACTION;
    } else if (tool_name == "gnucash_update_split" || tool_name == "gnucash_reconcile_account") {
        audit_record.operation = audit::Operation::UPDATE;
        audit_record.entity_type = audit::EntityType::TRANSACTION;
    } else if (tool_name == "gnucash_set_slot") {
        audit_record.operation = audit::Operation::UPDATE;
        audit_record.entity_type = audit::EntityType::NONE;
    } else {
        audit_record.operation = audit::Operation::NONE;
        audit_record.entity_type = audit::EntityType::NONE;
    }

    // Populate identity in audit record
    if (g_identity.has_value()) {
        audit_record.user_email = g_identity->user_id;
        audit_record.user_name = g_identity->display_name;
        audit_record.node_name = g_identity->node_name;
    }

    // Security check (if enforcement enabled)
    if (g_security_policy.has_value() && g_security_policy->enforcement_enabled) {
        auto sec = security_check(*g_security_policy, tool_name, arguments, g_rate_limiter);
        audit_record.authorization_level = classify_tool(tool_name);

        if (sec.decision == SecurityDecision::DENY) {
            audit_record.result_status = audit::ResultStatus::ERROR;
            auto elapsed = std::chrono::steady_clock::now() - start_time;
            audit_record.duration_ms = std::chrono::duration_cast<std::chrono::milliseconds>(elapsed).count();
            if (g_audit_logger) g_audit_logger->log(audit_record);
            return make_jsonrpc_error(error_codes::INTERNAL_ERROR,
                "Security: " + sec.reason, id);
        }

        if (sec.decision == SecurityDecision::REQUIRE_APPROVAL ||
            sec.decision == SecurityDecision::QUEUE_REVIEW) {
            audit_record.result_status = audit::ResultStatus::ERROR;
            auto elapsed = std::chrono::steady_clock::now() - start_time;
            audit_record.duration_ms = std::chrono::duration_cast<std::chrono::milliseconds>(elapsed).count();
            if (g_audit_logger) g_audit_logger->log(audit_record);

            json content = {{
                {"type", "text"},
                {"text", "Operation requires approval: " + sec.reason +
                         (sec.approval_id ? " (approval_id: " + *sec.approval_id + ")" : "")}
            }};
            return make_jsonrpc_result({{"content", content}, {"isError", true}}, id);
        }
    }

    // Map MCP tool names to legacy method names
    // MCP: "gnucash_open" → Legacy: "open"
    std::string method_name = tool_name;
    const std::string prefix = "gnucash_";
    if (tool_name.size() >= prefix.size() &&
        tool_name.compare(0, prefix.size(), prefix) == 0) {
        method_name = tool_name.substr(prefix.size());  // Remove "gnucash_" prefix
    }

    // Special handling for open command - initialize audit logger
    if (method_name == "open" && arguments.contains("path")) {
        g_book_path = arguments["path"].get<std::string>();
        auto logger_result = audit::AuditLogger::open(g_book_path);
        if (logger_result.is_ok()) {
            g_audit_logger = std::move(logger_result.unwrap());
        }
        audit_record.book_path = g_book_path;
    }

    // Special handling for audit_log query
    if (tool_name == "gnucash_audit_log") {
        if (!g_audit_logger) {
            audit_record.result_status = audit::ResultStatus::ERROR;
            audit_record.error_message = "no book open";
            return make_jsonrpc_error(error_codes::INTERNAL_ERROR,
                "no book open", id);
        }

        // Build query filters
        audit::AuditLogger::QueryFilters filters;
        if (arguments.contains("since"))
            filters.since = arguments["since"].get<std::string>();
        if (arguments.contains("until"))
            filters.until = arguments["until"].get<std::string>();
        if (arguments.contains("tool_name"))
            filters.tool_name = arguments["tool_name"].get<std::string>();
        if (arguments.contains("classification")) {
            std::string cls = arguments["classification"].get<std::string>();
            filters.classification = audit::parse_classification(cls);
        }
        if (arguments.contains("user_email"))
            filters.user_email = arguments["user_email"].get<std::string>();
        if (arguments.contains("entity_guid"))
            filters.entity_guid = arguments["entity_guid"].get<std::string>();
        if (arguments.contains("limit"))
            filters.limit = arguments["limit"].get<int>();

        // Query audit log
        auto query_result = g_audit_logger->query(filters);

        // Calculate duration
        auto end_time = std::chrono::steady_clock::now();
        auto duration = std::chrono::duration_cast<std::chrono::milliseconds>(
            end_time - start_time);
        audit_record.duration_ms = static_cast<int>(duration.count());

        if (query_result.is_err()) {
            audit_record.result_status = audit::ResultStatus::ERROR;
            audit_record.error_message = query_result.unwrap_err();
            g_audit_logger->log(audit_record);
            return make_jsonrpc_error(error_codes::INTERNAL_ERROR,
                query_result.unwrap_err(), id);
        }

        // Format results
        auto records = query_result.unwrap();
        std::ostringstream output;
        output << "Audit Log (" << records.size() << " records)\n\n";

        for (const auto& rec : records) {
            output << rec.timestamp << "  ["
                   << audit::classification_to_string(rec.classification) << "]  "
                   << rec.tool_name << "\n";

            if (rec.user_email)
                output << "  user: " << *rec.user_email << "\n";
            if (rec.entity_guid)
                output << "  entity: " << *rec.entity_guid << " ("
                       << audit::entity_type_to_string(rec.entity_type) << ")\n";
            if (rec.error_message)
                output << "  ERROR: " << *rec.error_message << "\n";
            if (rec.duration_ms)
                output << "  duration: " << *rec.duration_ms << "ms\n";

            output << "\n";
        }

        audit_record.result_status = audit::ResultStatus::SUCCESS;
        g_audit_logger->log(audit_record);

        json mcp_result = {
            {"content", json::array({
                {
                    {"type", "text"},
                    {"text", output.str()}
                }
            })}
        };

        return make_jsonrpc_result(mcp_result, id);
    }

    // Capture before_state for destructive write operations
    if (is_write && (tool_name == "gnucash_delete_transaction" ||
                     tool_name == "gnucash_void_transaction")) {
        if (arguments.contains("guid")) {
            json before_req = {
                {"method", "get_transaction"},
                {"params", {{"guid", arguments["guid"]}}},
                {"id", "before_state"}
            };
            json before_resp = gnucash::dispatch(before_req);
            if (before_resp.contains("result") && !before_resp.contains("error")) {
                audit_record.before_state = before_resp["result"];
                audit_record.entity_guid = arguments["guid"];
            }
        }
    }

    // Build legacy JSON request
    json legacy_request = {
        {"method", method_name},
        {"params", arguments},
        {"id", id}
    };

    // Dispatch to legacy handler (from json_api.cpp)
    json legacy_response = gnucash::dispatch(legacy_request);

    // Calculate duration
    auto end_time = std::chrono::steady_clock::now();
    auto duration = std::chrono::duration_cast<std::chrono::milliseconds>(
        end_time - start_time);
    audit_record.duration_ms = static_cast<int>(duration.count());

    // Check for error
    if (legacy_response.contains("error")) {
        // Log error
        audit_record.result_status = audit::ResultStatus::ERROR;
        audit_record.error_message = legacy_response["error"]["message"];

        if (g_audit_logger) {
            g_audit_logger->log(audit_record);
        }

        // Convert legacy error to JSON-RPC error
        std::string error_msg = legacy_response["error"]["message"];
        return make_jsonrpc_error(error_codes::INTERNAL_ERROR, error_msg, id);
    }

    // Log success
    audit_record.result_status = audit::ResultStatus::SUCCESS;

    // Extract entity GUID from result if present
    json result_value = legacy_response["result"];
    if (result_value.is_object()) {
        if (result_value.contains("guid")) {
            audit_record.entity_guid = result_value["guid"].get<std::string>();
        } else if (result_value.contains("book_guid")) {
            audit_record.entity_guid = result_value["book_guid"].get<std::string>();
        }
    }

    // Capture after_state for create operations
    if (is_write && audit_record.entity_guid) {
        if (tool_name == "gnucash_create_account") {
            json after_req = {
                {"method", "get_account"},
                {"params", {{"guid", *audit_record.entity_guid}}},
                {"id", "after_state"}
            };
            json after_resp = gnucash::dispatch(after_req);
            if (after_resp.contains("result") && !after_resp.contains("error")) {
                audit_record.after_state = after_resp["result"];
            }
        } else if (tool_name == "gnucash_post_transaction") {
            json after_req = {
                {"method", "get_transaction"},
                {"params", {{"guid", *audit_record.entity_guid}}},
                {"id", "after_state"}
            };
            json after_resp = gnucash::dispatch(after_req);
            if (after_resp.contains("result") && !after_resp.contains("error")) {
                audit_record.after_state = after_resp["result"];
            }
        } else if (tool_name == "gnucash_void_transaction") {
            // After voiding, capture the voided state
            json after_req = {
                {"method", "get_transaction"},
                {"params", {{"guid", *audit_record.entity_guid}}},
                {"id", "after_state"}
            };
            json after_resp = gnucash::dispatch(after_req);
            if (after_resp.contains("result") && !after_resp.contains("error")) {
                audit_record.after_state = after_resp["result"];
            }
        }
        // delete_transaction: no after_state (entity no longer exists)
    }

    if (g_audit_logger) {
        g_audit_logger->log(audit_record);
    }

    // Wrap result in MCP content format
    std::string text_content;
    if (result_value.is_string()) {
        text_content = result_value.get<std::string>();
    } else {
        text_content = result_value.dump(2);  // Pretty-print JSON
    }

    json mcp_result = {
        {"content", json::array({
            {
                {"type", "text"},
                {"text", text_content}
            }
        })}
    };

    return make_jsonrpc_result(mcp_result, id);
}

json handle_ping(const json& id) {
    return make_jsonrpc_result(json::object(), id);
}

json mcp_dispatch(const json& request) {
    json id = request.value("id", json(nullptr));

    try {
        if (!request.contains("method")) {
            return make_jsonrpc_error(error_codes::INVALID_REQUEST,
                "Missing 'method' field", id);
        }

        std::string method = request["method"];
        json params = request.value("params", json::object());

        if (method == "initialize") {
            return handle_initialize(params, id);
        }
        else if (method == "initialized") {
            handle_initialized();
            return json(nullptr);  // No response for notifications
        }
        else if (method == "tools/list") {
            return handle_tools_list(id);
        }
        else if (method == "tools/call") {
            return handle_tools_call(params, id);
        }
        else if (method == "ping") {
            return handle_ping(id);
        }
        else {
            return make_jsonrpc_error(error_codes::METHOD_NOT_FOUND,
                "Unknown method: " + method, id);
        }
    }
    catch (const std::exception& e) {
        return make_jsonrpc_error(error_codes::INTERNAL_ERROR,
            std::string("Internal error: ") + e.what(), id);
    }
}

void run_mcp_loop() {
    std::string line;
    ProtocolMode mode = ProtocolMode::MCP;  // Default to MCP
    bool first_request = true;

    while (std::getline(std::cin, line)) {
        if (line.empty()) continue;

        json response;
        try {
            json request = json::parse(line);

            // Auto-detect protocol on first request
            if (first_request) {
                mode = detect_protocol(request);
                first_request = false;
            }

            if (mode == ProtocolMode::MCP) {
                response = mcp_dispatch(request);
                // Don't output anything for notifications (null response)
                if (!response.is_null()) {
                    std::cout << response.dump() << "\n" << std::flush;
                }
            }
            else {
                // Legacy mode - use original dispatch
                response = gnucash::dispatch(request);
                std::cout << response.dump() << "\n" << std::flush;
            }
        }
        catch (const json::parse_error& e) {
            if (mode == ProtocolMode::MCP) {
                response = make_jsonrpc_error(error_codes::PARSE_ERROR,
                    std::string("JSON parse error: ") + e.what(), nullptr);
            } else {
                response = {
                    {"error", {{"message", std::string("JSON parse error: ") + e.what()}}},
                    {"id", nullptr}
                };
            }
            std::cout << response.dump() << "\n" << std::flush;
        }
    }
}

// ========================================================================
// Tool Registry (Task 5.1.2 - Tool definitions)
// ========================================================================

void set_agent_config(const dhall::AgentConfig& config) {
    g_agent_config = config;
}

std::optional<dhall::AgentConfig> get_agent_config() {
    return g_agent_config;
}

void set_identity(const Identity& identity) {
    g_identity = identity;
}

void set_security_policy(SecurityPolicy policy) {
    g_security_policy = std::move(policy);
}

std::vector<ToolDefinition> get_tool_definitions() {
    // Get all tools
    std::vector<ToolDefinition> all_tools;

    // Tool 1: gnucash_open
    all_tools.push_back({
        "gnucash_open",
        "Open a GnuCash book file",
        {
            "object",
            {
                {"path", {{"type", "string"}, {"description", "Path to GnuCash book file"}}},
                {"read_only", {{"type", "boolean"}, {"description", "Open in read-only mode"}, {"default", true}}}
            },
            {"path"}
        }
    });

    // Tool 2: gnucash_close
    all_tools.push_back({
        "gnucash_close",
        "Close the currently open GnuCash book",
        {"object", json::object(), {}}
    });

    // Tool 3: gnucash_info
    all_tools.push_back({
        "gnucash_info",
        "Get information about the currently open book",
        {"object", json::object(), {}}
    });

    // Tool 4: gnucash_get_accounts
    all_tools.push_back({
        "gnucash_get_accounts",
        "List all accounts in the book",
        {"object", json::object(), {}}
    });

    // Tool 5: gnucash_account_tree
    all_tools.push_back({
        "gnucash_account_tree",
        "Get hierarchical account tree with full paths",
        {"object", json::object(), {}}
    });

    // Tool 6: gnucash_get_account
    all_tools.push_back({
        "gnucash_get_account",
        "Get account details by GUID",
        {
            "object",
            {
                {"guid", {{"type", "string"}, {"description", "Account GUID"}}}
            },
            {"guid"}
        }
    });

    // Tool 7: gnucash_get_account_by_path
    all_tools.push_back({
        "gnucash_get_account_by_path",
        "Get account by full path (e.g., 'Assets:Current Assets:Checking')",
        {
            "object",
            {
                {"path", {{"type", "string"}, {"description", "Account path"}}}
            },
            {"path"}
        }
    });

    // Tool 8: gnucash_get_transactions
    all_tools.push_back({
        "gnucash_get_transactions",
        "Query transactions with optional date filtering",
        {
            "object",
            {
                {"from_date", {{"type", "string"}, {"description", "Start date (YYYY-MM-DD)"}}},
                {"to_date", {{"type", "string"}, {"description", "End date (YYYY-MM-DD)"}}}
            },
            {}
        }
    });

    // Tool 9: gnucash_get_transaction
    all_tools.push_back({
        "gnucash_get_transaction",
        "Get transaction details by GUID",
        {
            "object",
            {
                {"guid", {{"type", "string"}, {"description", "Transaction GUID"}}}
            },
            {"guid"}
        }
    });

    // Tool 10: gnucash_get_splits
    all_tools.push_back({
        "gnucash_get_splits",
        "Get all splits for an account",
        {
            "object",
            {
                {"account_guid", {{"type", "string"}, {"description", "Account GUID"}}}
            },
            {"account_guid"}
        }
    });

    // Tool 11: gnucash_get_balance
    all_tools.push_back({
        "gnucash_get_balance",
        "Get account balance as of a specific date",
        {
            "object",
            {
                {"account_guid", {{"type", "string"}, {"description", "Account GUID"}}},
                {"as_of", {{"type", "string"}, {"description", "Date (YYYY-MM-DD)"}}}
            },
            {"account_guid"}
        }
    });

    // Tool 12: gnucash_trial_balance
    all_tools.push_back({
        "gnucash_trial_balance",
        "Get trial balance (all account balances)",
        {
            "object",
            {
                {"as_of", {{"type", "string"}, {"description", "Date (YYYY-MM-DD)"}}}
            },
            {}
        }
    });

    // Tool 13: gnucash_get_commodities
    all_tools.push_back({
        "gnucash_get_commodities",
        "List all commodities (currencies and securities)",
        {"object", json::object(), {}}
    });

    // Tool 14: gnucash_get_prices
    all_tools.push_back({
        "gnucash_get_prices",
        "Get price database entries",
        {"object", json::object(), {}}
    });

    // Tool 15: gnucash_create_account (WRITE)
    all_tools.push_back({
        "gnucash_create_account",
        "Create a new account (requires write access)",
        {
            "object",
            {
                {"name", {{"type", "string"}, {"description", "Account name"}}},
                {"type", {{"type", "string"}, {"description", "Account type (ASSET, LIABILITY, etc.)"}}},
                {"parent_guid", {{"type", "string"}, {"description", "Parent account GUID"}}},
                {"description", {{"type", "string"}, {"description", "Account description"}}},
                {"code", {{"type", "string"}, {"description", "Account code"}}},
                {"hidden", {{"type", "boolean"}, {"description", "Hidden flag"}}},
                {"placeholder", {{"type", "boolean"}, {"description", "Placeholder flag"}}}
            },
            {"name", "type", "parent_guid"}
        }
    });

    // Tool 16: gnucash_post_transaction (WRITE)
    all_tools.push_back({
        "gnucash_post_transaction",
        "Create a new transaction (requires write access)",
        {
            "object",
            {
                {"description", {{"type", "string"}, {"description", "Transaction description"}}},
                {"post_date", {{"type", "string"}, {"description", "Post date (YYYY-MM-DD)"}}},
                {"splits", {{"type", "array"}, {"description", "Array of splits"}}},
                {"num", {{"type", "string"}, {"description", "Check/transaction number"}}},
                {"currency_guid", {{"type", "string"}, {"description", "Currency GUID"}}}
            },
            {"description", "post_date", "splits"}
        }
    });

    // Tool 17: gnucash_delete_transaction (WRITE)
    all_tools.push_back({
        "gnucash_delete_transaction",
        "Delete a transaction (requires write access)",
        {
            "object",
            {
                {"guid", {{"type", "string"}, {"description", "Transaction GUID"}}}
            },
            {"guid"}
        }
    });

    // Tool 18: gnucash_void_transaction (WRITE)
    all_tools.push_back({
        "gnucash_void_transaction",
        "Void a transaction with reversal (requires write access)",
        {
            "object",
            {
                {"guid", {{"type", "string"}, {"description", "Transaction GUID"}}},
                {"reason", {{"type", "string"}, {"description", "Void reason"}}}
            },
            {"guid", "reason"}
        }
    });

    // Tool 19: gnucash_parse_ofx
    all_tools.push_back({
        "gnucash_parse_ofx",
        "Parse OFX/QFX bank statement file content",
        {
            "object",
            {
                {"content", {{"type", "string"}, {"description", "OFX file content"}}}
            },
            {"content"}
        }
    });

    // Tool 20: gnucash_audit_log
    all_tools.push_back({
        "gnucash_audit_log",
        "Query audit trail with filters",
        {
            "object",
            {
                {"since", {{"type", "string"}, {"description", "Start timestamp (ISO 8601)"}}},
                {"until", {{"type", "string"}, {"description", "End timestamp (ISO 8601)"}}},
                {"tool_name", {{"type", "string"}, {"description", "Filter by tool name"}}},
                {"classification", {{"type", "string"}, {"enum", json::array({"read", "write"})}}},
                {"user_email", {{"type", "string"}, {"description", "Filter by user email"}}},
                {"entity_guid", {{"type", "string"}, {"description", "Filter by entity GUID"}}},
                {"limit", {{"type", "integer"}, {"description", "Max records to return"}, {"default", 100}}}
            },
            {}
        }
    });

    // Tool 21: gnucash_import_ofx (WRITE)
    all_tools.push_back({
        "gnucash_import_ofx",
        "Import OFX/QFX bank statement with FITID dedup. Creates transactions in target account with Imbalance splits.",
        {
            "object",
            {
                {"content", {{"type", "string"}, {"description", "OFX file content"}}},
                {"account_path", {{"type", "string"}, {"description", "Target bank account path (e.g., 'Assets:Current Assets:Checking')"}}},
                {"imbalance_account", {{"type", "string"}, {"description", "Imbalance account path"}, {"default", "Imbalance-USD"}}}
            },
            {"content", "account_path"}
        }
    });

    // Tool 22: gnucash_import_csv (WRITE)
    all_tools.push_back({
        "gnucash_import_csv",
        "Import CSV bank statement (PayPal, Stripe, Venmo, Apple Card, or generic format) with dedup.",
        {
            "object",
            {
                {"content", {{"type", "string"}, {"description", "CSV file content"}}},
                {"account_path", {{"type", "string"}, {"description", "Target bank account path"}}},
                {"format", {{"type", "string"}, {"enum", json::array({"paypal", "stripe", "venmo", "apple_card", "generic"})}, {"default", "generic"}}},
                {"imbalance_account", {{"type", "string"}, {"description", "Imbalance account path"}, {"default", "Imbalance-USD"}}},
                {"delimiter", {{"type", "string"}, {"description", "CSV delimiter (generic format)"}}},
                {"date_col", {{"type", "integer"}, {"description", "Date column index (0-based)"}}},
                {"amount_col", {{"type", "integer"}, {"description", "Amount column index"}}},
                {"desc_col", {{"type", "integer"}, {"description", "Description column index"}}},
                {"date_format", {{"type", "string"}, {"description", "Date format: YYYY-MM-DD, MM/DD/YYYY, etc."}}},
                {"has_header", {{"type", "boolean"}, {"description", "CSV has header row"}, {"default", true}}}
            },
            {"content", "account_path"}
        }
    });

    // Tool 23: gnucash_check_duplicates (READ)
    all_tools.push_back({
        "gnucash_check_duplicates",
        "Check which FITIDs already exist for an account (batch dedup check)",
        {
            "object",
            {
                {"account_path", {{"type", "string"}, {"description", "Account path to check"}}},
                {"fitids", {{"type", "array"}, {"items", {{"type", "string"}}}, {"description", "List of FITIDs to check"}}}
            },
            {"account_path", "fitids"}
        }
    });

    // Tool 24: gnucash_get_slots (READ)
    all_tools.push_back({
        "gnucash_get_slots",
        "Get metadata slots for an entity (account, split, or transaction)",
        {
            "object",
            {
                {"obj_guid", {{"type", "string"}, {"description", "Entity GUID"}}},
                {"name", {{"type", "string"}, {"description", "Specific slot name (omit for all slots)"}}}
            },
            {"obj_guid"}
        }
    });

    // Tool 25: gnucash_set_slot (WRITE)
    all_tools.push_back({
        "gnucash_set_slot",
        "Set a metadata slot on an entity (creates or updates)",
        {
            "object",
            {
                {"obj_guid", {{"type", "string"}, {"description", "Entity GUID"}}},
                {"name", {{"type", "string"}, {"description", "Slot name"}}},
                {"value", {{"type", "string"}, {"description", "Slot value"}}}
            },
            {"obj_guid", "name", "value"}
        }
    });

    // Tool 26: gnucash_update_split (WRITE)
    all_tools.push_back({
        "gnucash_update_split",
        "Recategorize a split by changing its account assignment",
        {
            "object",
            {
                {"split_guid", {{"type", "string"}, {"description", "Split GUID to update"}}},
                {"new_account_path", {{"type", "string"}, {"description", "New account path"}}}
            },
            {"split_guid", "new_account_path"}
        }
    });

    // Tool 27: gnucash_reconcile_account (WRITE)
    all_tools.push_back({
        "gnucash_reconcile_account",
        "Reconcile account: mark unreconciled splits as cleared up to statement date, report balance difference",
        {
            "object",
            {
                {"account_path", {{"type", "string"}, {"description", "Account path"}}},
                {"statement_date", {{"type", "string"}, {"description", "Statement date (YYYY-MM-DD)"}}},
                {"statement_balance", {{"type", "string"}, {"description", "Statement ending balance (e.g., '1234.56')"}}}
            },
            {"account_path", "statement_date", "statement_balance"}
        }
    });

    // Tool 28: gnucash_match_imported (READ)
    all_tools.push_back({
        "gnucash_match_imported",
        "Find potential cross-institution transfer matches between two accounts",
        {
            "object",
            {
                {"account_a_path", {{"type", "string"}, {"description", "First account path"}}},
                {"account_b_path", {{"type", "string"}, {"description", "Second account path"}}},
                {"from_date", {{"type", "string"}, {"description", "Start date (YYYY-MM-DD)"}}},
                {"to_date", {{"type", "string"}, {"description", "End date (YYYY-MM-DD)"}}},
                {"date_window", {{"type", "integer"}, {"description", "Max days between matching transactions"}, {"default", 3}}},
                {"min_similarity", {{"type", "number"}, {"description", "Min match similarity (0-1)"}, {"default", 0.5}}}
            },
            {"account_a_path", "account_b_path"}
        }
    });

    // Tool 29: gnucash_bank_feed_status (READ)
    all_tools.push_back({
        "gnucash_bank_feed_status",
        "Get import status for bank-connected accounts (imported count, unreconciled count)",
        {
            "object",
            {
                {"account_path", {{"type", "string"}, {"description", "Account path (omit for all accounts)"}}}
            },
            {}
        }
    });

    // Filter tools if agent config is loaded
    if (g_agent_config) {
        std::vector<ToolDefinition> filtered_tools;
        for (const auto& tool : all_tools) {
            // Check if tool is in agent's allowed list
            bool allowed = false;
            for (const auto& allowed_tool : g_agent_config->tools) {
                if (tool.name == allowed_tool) {
                    allowed = true;
                    break;
                }
            }
            if (allowed) {
                filtered_tools.push_back(tool);
            }
        }
        return filtered_tools;
    }

    return all_tools;
}

std::optional<ToolDefinition> get_tool_definition(const std::string& name) {
    auto tools = get_tool_definitions();
    for (const auto& tool : tools) {
        if (tool.name == name) {
            return tool;
        }
    }
    return std::nullopt;
}

} // namespace mcp
} // namespace gnucash
