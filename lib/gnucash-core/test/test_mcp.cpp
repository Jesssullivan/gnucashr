#include <catch2/catch_test_macros.hpp>
#include "gnucash/mcp.h"
#include "gnucash/dhall_config.h"
#include <nlohmann/json.hpp>
#include <string>

using json = nlohmann::json;
using namespace gnucash::mcp;

// ========================================================================
// Protocol Detection
// ========================================================================

TEST_CASE("detect_protocol: MCP request", "[mcp][protocol]") {
    json request = {{"jsonrpc", "2.0"}, {"method", "initialize"}, {"id", 1}};
    REQUIRE(detect_protocol(request) == ProtocolMode::MCP);
}

TEST_CASE("detect_protocol: legacy request", "[mcp][protocol]") {
    json request = {{"method", "open"}, {"params", {{"path", "/tmp/test.gnucash"}}}, {"id", 1}};
    REQUIRE(detect_protocol(request) == ProtocolMode::LEGACY);
}

TEST_CASE("detect_protocol: MCP requires version 2.0", "[mcp][protocol]") {
    json request = {{"jsonrpc", "1.0"}, {"method", "initialize"}, {"id", 1}};
    REQUIRE(detect_protocol(request) == ProtocolMode::LEGACY);
}

// ========================================================================
// JSON-RPC 2.0 Helpers
// ========================================================================

TEST_CASE("make_jsonrpc_error: format", "[mcp][jsonrpc]") {
    auto err = make_jsonrpc_error(error_codes::PARSE_ERROR, "bad json", 42);
    REQUIRE(err["jsonrpc"] == "2.0");
    REQUIRE(err["error"]["code"] == -32700);
    REQUIRE(err["error"]["message"] == "bad json");
    REQUIRE(err["id"] == 42);
}

TEST_CASE("make_jsonrpc_error: null id", "[mcp][jsonrpc]") {
    auto err = make_jsonrpc_error(error_codes::INTERNAL_ERROR, "oops", nullptr);
    REQUIRE(err["id"].is_null());
}

TEST_CASE("make_jsonrpc_result: format", "[mcp][jsonrpc]") {
    json payload = {{"key", "value"}};
    auto res = make_jsonrpc_result(payload, 7);
    REQUIRE(res["jsonrpc"] == "2.0");
    REQUIRE(res["result"]["key"] == "value");
    REQUIRE(res["id"] == 7);
}

// ========================================================================
// MCP Dispatch Routing
// ========================================================================

TEST_CASE("mcp_dispatch: initialize", "[mcp][dispatch]") {
    json request = {
        {"jsonrpc", "2.0"},
        {"method", "initialize"},
        {"params", json::object()},
        {"id", 1}
    };
    auto response = mcp_dispatch(request);

    REQUIRE(response["jsonrpc"] == "2.0");
    REQUIRE(response["id"] == 1);
    auto result = response["result"];
    REQUIRE(result["protocolVersion"] == MCP_PROTOCOL_VERSION);
    REQUIRE(result["serverInfo"]["name"] == "gnucash-mcp");
    REQUIRE(result["serverInfo"]["version"] == "0.3.0");
    REQUIRE(result["capabilities"]["tools"] == true);
}

TEST_CASE("mcp_dispatch: initialized notification returns null", "[mcp][dispatch]") {
    json request = {
        {"jsonrpc", "2.0"},
        {"method", "initialized"}
    };
    auto response = mcp_dispatch(request);
    REQUIRE(response.is_null());
}

TEST_CASE("mcp_dispatch: ping", "[mcp][dispatch]") {
    json request = {
        {"jsonrpc", "2.0"},
        {"method", "ping"},
        {"id", 99}
    };
    auto response = mcp_dispatch(request);
    REQUIRE(response["jsonrpc"] == "2.0");
    REQUIRE(response["result"].is_object());
    REQUIRE(response["id"] == 99);
}

TEST_CASE("mcp_dispatch: unknown method", "[mcp][dispatch]") {
    json request = {
        {"jsonrpc", "2.0"},
        {"method", "nonexistent"},
        {"id", 5}
    };
    auto response = mcp_dispatch(request);
    REQUIRE(response["error"]["code"] == error_codes::METHOD_NOT_FOUND);
    REQUIRE(response["error"]["message"].get<std::string>().find("nonexistent") != std::string::npos);
}

TEST_CASE("mcp_dispatch: missing method field", "[mcp][dispatch]") {
    json request = {
        {"jsonrpc", "2.0"},
        {"id", 6}
    };
    auto response = mcp_dispatch(request);
    REQUIRE(response["error"]["code"] == error_codes::INVALID_REQUEST);
}

// ========================================================================
// tools/list
// ========================================================================

TEST_CASE("tools/list: returns all 20 tools without agent config", "[mcp][tools]") {
    // Clear any agent config
    // Note: there's no clear function, but we test default state
    json request = {
        {"jsonrpc", "2.0"},
        {"method", "tools/list"},
        {"id", 10}
    };
    auto response = mcp_dispatch(request);
    auto tools = response["result"]["tools"];
    REQUIRE(tools.is_array());
    // Should have at least 20 tools (exact count verified in integration tests)
    REQUIRE(tools.size() >= 20);
}

TEST_CASE("tools/list: each tool has required fields", "[mcp][tools]") {
    json request = {
        {"jsonrpc", "2.0"},
        {"method", "tools/list"},
        {"id", 11}
    };
    auto response = mcp_dispatch(request);
    auto tools = response["result"]["tools"];

    for (const auto& tool : tools) {
        REQUIRE(tool.contains("name"));
        REQUIRE(tool.contains("description"));
        REQUIRE(tool.contains("inputSchema"));
        REQUIRE(tool["name"].is_string());
        REQUIRE(tool["description"].is_string());
        REQUIRE(tool["inputSchema"].is_object());
        REQUIRE(tool["inputSchema"]["type"] == "object");
    }
}

TEST_CASE("tools/list: known tools present", "[mcp][tools]") {
    auto tools = get_tool_definitions();

    // Collect tool names
    std::vector<std::string> names;
    for (const auto& t : tools) {
        names.push_back(t.name);
    }

    // Check key tools exist
    auto has = [&](const std::string& n) {
        return std::find(names.begin(), names.end(), n) != names.end();
    };

    REQUIRE(has("gnucash_open"));
    REQUIRE(has("gnucash_close"));
    REQUIRE(has("gnucash_get_accounts"));
    REQUIRE(has("gnucash_get_transactions"));
    REQUIRE(has("gnucash_post_transaction"));
    REQUIRE(has("gnucash_audit_log"));
    REQUIRE(has("gnucash_parse_ofx"));
}

// ========================================================================
// Tool Filtering (Agent Config)
// ========================================================================

TEST_CASE("tool filtering: agent config restricts tools", "[mcp][filtering]") {
    // Set a minimal agent config
    gnucash::dhall::AgentConfig config;
    config.name = "test-agent";
    config.description = "Test";
    config.tools = {"gnucash_open", "gnucash_get_accounts"};
    config.authorization_level = "Auto";

    set_agent_config(config);

    auto tools = get_tool_definitions();
    REQUIRE(tools.size() == 2);
    REQUIRE(tools[0].name == "gnucash_open");
    REQUIRE(tools[1].name == "gnucash_get_accounts");

    // Clean up: reset to no agent config by setting a config with all tools
    // (there's no unset function, so we need to verify our tests are self-contained)
}

TEST_CASE("tool filtering: agent config via tools/list", "[mcp][filtering]") {
    gnucash::dhall::AgentConfig config;
    config.name = "report-agent";
    config.description = "Reports";
    config.tools = {"gnucash_info", "gnucash_trial_balance", "gnucash_audit_log"};
    config.authorization_level = "Auto";

    set_agent_config(config);

    json request = {
        {"jsonrpc", "2.0"},
        {"method", "tools/list"},
        {"id", 20}
    };
    auto response = mcp_dispatch(request);
    auto tools = response["result"]["tools"];

    REQUIRE(tools.size() == 3);

    std::vector<std::string> names;
    for (const auto& t : tools) {
        names.push_back(t["name"]);
    }

    REQUIRE(std::find(names.begin(), names.end(), "gnucash_info") != names.end());
    REQUIRE(std::find(names.begin(), names.end(), "gnucash_trial_balance") != names.end());
    REQUIRE(std::find(names.begin(), names.end(), "gnucash_audit_log") != names.end());
}

TEST_CASE("tool filtering: empty tools list yields no tools", "[mcp][filtering]") {
    gnucash::dhall::AgentConfig config;
    config.name = "empty-agent";
    config.description = "No tools";
    config.tools = {};
    config.authorization_level = "Auto";

    set_agent_config(config);

    auto tools = get_tool_definitions();
    REQUIRE(tools.empty());
}

// ========================================================================
// tools/call error cases
// ========================================================================

TEST_CASE("tools/call: missing name", "[mcp][call]") {
    json request = {
        {"jsonrpc", "2.0"},
        {"method", "tools/call"},
        {"params", {{"arguments", json::object()}}},
        {"id", 30}
    };
    auto response = mcp_dispatch(request);
    REQUIRE(response["error"]["code"] == error_codes::INVALID_PARAMS);
}

TEST_CASE("tools/call: missing arguments", "[mcp][call]") {
    json request = {
        {"jsonrpc", "2.0"},
        {"method", "tools/call"},
        {"params", {{"name", "gnucash_open"}}},
        {"id", 31}
    };
    auto response = mcp_dispatch(request);
    REQUIRE(response["error"]["code"] == error_codes::INVALID_PARAMS);
}

// ========================================================================
// get_tool_definition (by name)
// ========================================================================

TEST_CASE("get_tool_definition: existing tool", "[mcp][registry]") {
    // Reset agent config to get all tools
    gnucash::dhall::AgentConfig config;
    config.name = "all";
    config.description = "all";
    config.tools = {
        "gnucash_open", "gnucash_close", "gnucash_info",
        "gnucash_get_accounts", "gnucash_account_tree",
        "gnucash_get_account", "gnucash_get_account_by_path",
        "gnucash_get_transactions", "gnucash_get_transaction",
        "gnucash_get_splits", "gnucash_get_balance",
        "gnucash_trial_balance", "gnucash_get_commodities",
        "gnucash_get_prices", "gnucash_create_account",
        "gnucash_post_transaction", "gnucash_delete_transaction",
        "gnucash_void_transaction", "gnucash_parse_ofx",
        "gnucash_audit_log"
    };
    config.authorization_level = "Auto";
    set_agent_config(config);

    auto tool = get_tool_definition("gnucash_open");
    REQUIRE(tool.has_value());
    REQUIRE(tool->name == "gnucash_open");
    REQUIRE(!tool->description.empty());
}

TEST_CASE("get_tool_definition: nonexistent tool", "[mcp][registry]") {
    auto tool = get_tool_definition("gnucash_nonexistent");
    REQUIRE_FALSE(tool.has_value());
}

// ========================================================================
// Error code constants
// ========================================================================

TEST_CASE("error codes: standard JSON-RPC values", "[mcp][errors]") {
    REQUIRE(error_codes::PARSE_ERROR == -32700);
    REQUIRE(error_codes::INVALID_REQUEST == -32600);
    REQUIRE(error_codes::METHOD_NOT_FOUND == -32601);
    REQUIRE(error_codes::INVALID_PARAMS == -32602);
    REQUIRE(error_codes::INTERNAL_ERROR == -32603);
}
