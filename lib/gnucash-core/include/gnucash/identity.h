#pragma once
// Identity resolution for audit trail and authorization
// Resolves who is making requests to the MCP server

#include "result.h"
#include <string>
#include <optional>

namespace gnucash {

struct Identity {
    std::string user_id;       // Primary identifier (email or username)
    std::string display_name;  // Human-readable name
    std::string node_name;     // Hostname
    std::string source;        // How identity was resolved: "cli", "env", "system"
};

// Resolve identity from available sources (priority order):
// 1. cli_identity parameter (--identity flag)
// 2. GNUCASH_USER environment variable
// 3. System username + hostname
Identity resolve_identity(const std::optional<std::string>& cli_identity = std::nullopt);

// Get system username (getenv USER or getpwuid)
std::string get_system_username();

// Get hostname
std::string get_hostname();

} // namespace gnucash
