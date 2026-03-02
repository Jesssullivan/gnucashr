#pragma once
// Dhall configuration parser for agent configs
// Calls dhall CLI to parse and resolve agent configurations

#include <string>
#include <vector>
#include <optional>
#include "result.h"

namespace gnucash {
namespace dhall {

/// Parsed agent configuration
struct AgentConfig {
    std::string name;
    std::string description;
    std::vector<std::string> tools;           // Tool names this agent can use
    std::string authorization_level;          // "auto", "approve", "review"
    std::optional<std::string> schedule;      // "hourly", "daily", "on-demand"
};

/// Parse agent configuration from Dhall file
/// Calls: dhall-to-json --file <path> | jq
Result<AgentConfig> parse_agent_config(const std::string& dhall_path);

/// Check if Dhall CLI is available
bool dhall_available();

} // namespace dhall
} // namespace gnucash
