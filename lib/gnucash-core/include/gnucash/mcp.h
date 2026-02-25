#pragma once
// MCP (Model Context Protocol) implementation for gnucash-core
// Implements JSON-RPC 2.0 over stdio for Claude Code integration
//
// Protocol Flow:
//   1. Client sends {"jsonrpc": "2.0", "method": "initialize", ...}
//   2. Server responds with capabilities
//   3. Client sends {"method": "initialized"}
//   4. Client calls tools via {"method": "tools/call", ...}
//   5. Server returns results or errors
//
// Backward Compatibility:
//   - Legacy JSON-lines protocol still supported (no "jsonrpc" field)
//   - Protocol auto-detected from first request

#include <nlohmann/json.hpp>
#include <string>
#include <vector>
#include <optional>
#include "dhall_config.h"

namespace gnucash {
namespace mcp {

using json = nlohmann::json;

// ========================================================================
// MCP Types
// ========================================================================

/// MCP protocol version
constexpr const char* MCP_PROTOCOL_VERSION = "2024-11-05";

/// JSON-RPC 2.0 version
constexpr const char* JSONRPC_VERSION = "2.0";

/// Tool input schema (JSON Schema format)
struct ToolInputSchema {
    std::string type;                        // "object"
    json properties;                         // Field definitions
    std::vector<std::string> required;       // Required field names
    std::optional<json> additional_properties;
};

/// MCP tool definition
struct ToolDefinition {
    std::string name;                        // Tool name (e.g., "gnucash_open")
    std::string description;                 // Human-readable description
    ToolInputSchema input_schema;            // JSON Schema for arguments
};

/// Server capabilities
struct ServerCapabilities {
    bool supports_tools = true;              // Tools supported
    bool supports_prompts = false;           // Prompts not supported
    bool supports_resources = false;         // Resources not supported
    bool supports_sampling = false;          // Sampling not supported
};

/// Server information
struct ServerInfo {
    std::string name = "gnucash-mcp";
    std::string version = "0.3.0";
};

/// MCP initialization params
struct InitializeParams {
    std::string protocol_version;            // Client's protocol version
    ServerCapabilities capabilities;         // Client's capabilities
    json client_info;                        // Client metadata
};

/// MCP initialization result
struct InitializeResult {
    std::string protocol_version;            // Server's protocol version
    ServerCapabilities capabilities;         // Server's capabilities
    ServerInfo server_info;                  // Server metadata
};

/// Tool call parameters
struct ToolCallParams {
    std::string name;                        // Tool name
    json arguments;                          // Tool-specific arguments
};

/// Tool call result (content array format)
struct ToolCallResult {
    std::vector<json> content;               // Array of content items
    bool is_error = false;                   // Whether this is an error result
};

// ========================================================================
// Protocol Detection
// ========================================================================

/// Protocol mode
enum class ProtocolMode {
    MCP,        // JSON-RPC 2.0 with MCP methods
    LEGACY      // Custom JSON-lines protocol
};

/// Detect protocol from first request
ProtocolMode detect_protocol(const json& request);

// ========================================================================
// MCP Handlers
// ========================================================================

/// Handle initialize request
/// Request: {"jsonrpc": "2.0", "method": "initialize", "params": {...}, "id": 1}
/// Response: {"jsonrpc": "2.0", "result": {...}, "id": 1}
json handle_initialize(const json& params, const json& id);

/// Handle initialized notification
/// Request: {"jsonrpc": "2.0", "method": "initialized"}
/// No response (notification)
void handle_initialized();

/// Handle tools/list request
/// Request: {"jsonrpc": "2.0", "method": "tools/list", "id": 1}
/// Response: {"jsonrpc": "2.0", "result": {"tools": [...]}, "id": 1}
json handle_tools_list(const json& id);

/// Handle tools/call request
/// Request: {"jsonrpc": "2.0", "method": "tools/call", "params": {"name": "...", "arguments": {...}}, "id": 1}
/// Response: {"jsonrpc": "2.0", "result": {"content": [...]}, "id": 1}
json handle_tools_call(const json& params, const json& id);

/// Handle ping request
/// Request: {"jsonrpc": "2.0", "method": "ping", "id": 1}
/// Response: {"jsonrpc": "2.0", "result": {}, "id": 1}
json handle_ping(const json& id);

/// Main MCP dispatch (routes to appropriate handler)
json mcp_dispatch(const json& request);

/// Main MCP event loop (reads stdin, dispatches, writes stdout)
void run_mcp_loop();

// ========================================================================
// Tool Registry
// ========================================================================

/// Get all available tool definitions
std::vector<ToolDefinition> get_tool_definitions();

/// Get tool definition by name
std::optional<ToolDefinition> get_tool_definition(const std::string& name);

/// Set agent configuration (filters available tools)
void set_agent_config(const dhall::AgentConfig& config);

/// Get current agent configuration (if any)
std::optional<dhall::AgentConfig> get_agent_config();

// ========================================================================
// JSON-RPC 2.0 Error Codes
// ========================================================================

namespace error_codes {
    constexpr int PARSE_ERROR = -32700;      // Invalid JSON
    constexpr int INVALID_REQUEST = -32600;  // Invalid Request object
    constexpr int METHOD_NOT_FOUND = -32601; // Method does not exist
    constexpr int INVALID_PARAMS = -32602;   // Invalid method parameters
    constexpr int INTERNAL_ERROR = -32603;   // Internal JSON-RPC error
}

/// Make JSON-RPC 2.0 error response
json make_jsonrpc_error(int code, const std::string& message, const json& id);

/// Make JSON-RPC 2.0 success response
json make_jsonrpc_result(const json& result, const json& id);

} // namespace mcp
} // namespace gnucash
