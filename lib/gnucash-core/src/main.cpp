#include "gnucash/mcp.h"
#include "gnucash/dhall_config.h"
#include "gnucash/identity.h"
#include "gnucash/security.h"
#include <iostream>
#include <string>

void print_usage(const char* prog) {
    std::cerr << "Usage: " << prog << " [OPTIONS]\n\n"
              << "Options:\n"
              << "  --agent <dhall-file>    Load agent configuration from Dhall file\n"
              << "                          (filters available tools to agent's list)\n"
              << "  --identity <email>      Set user identity for audit trail\n"
              << "  --enforce               Enable authorization enforcement\n"
              << "  --help                  Show this help message\n\n"
              << "Environment:\n"
              << "  GNUCASH_USER            User identity (fallback if --identity not set)\n\n"
              << "Examples:\n"
              << "  " << prog << "  # Run with all tools available\n"
              << "  " << prog << " --agent dhall/agents/spend-monitor.dhall\n"
              << "  " << prog << " --agent dhall/agents/spend-monitor.dhall --enforce\n"
              << "  " << prog << " --identity user@example.com --enforce\n";
}

int main(int argc, char* argv[]) {
    // Parse command-line arguments
    std::string agent_config_path;
    std::optional<std::string> cli_identity;
    bool enforce = false;

    for (int i = 1; i < argc; ++i) {
        std::string arg = argv[i];
        if (arg == "--help" || arg == "-h") {
            print_usage(argv[0]);
            return 0;
        } else if (arg == "--agent" && i + 1 < argc) {
            agent_config_path = argv[++i];
        } else if (arg == "--identity" && i + 1 < argc) {
            cli_identity = argv[++i];
        } else if (arg == "--enforce") {
            enforce = true;
        } else {
            std::cerr << "Unknown argument: " << arg << "\n\n";
            print_usage(argv[0]);
            return 1;
        }
    }

    // Resolve identity
    auto identity = gnucash::resolve_identity(cli_identity);
    gnucash::mcp::set_identity(identity);
    std::cerr << "Identity: " << identity.user_id
              << " (source: " << identity.source << ")\n";

    // Load agent config if provided
    gnucash::dhall::AgentConfig agent_config;
    if (!agent_config_path.empty()) {
        auto config_result = gnucash::dhall::parse_agent_config(agent_config_path);
        if (config_result.is_err()) {
            std::cerr << "Failed to load agent config: "
                      << config_result.unwrap_err() << "\n";
            return 1;
        }
        agent_config = config_result.unwrap();
        std::cerr << "Loaded agent: " << agent_config.name << "\n";
        std::cerr << "Tools: " << agent_config.tools.size() << " tools available\n";

        // Set agent config in MCP layer
        gnucash::mcp::set_agent_config(agent_config);
    }

    // Set up security policy
    if (enforce) {
        gnucash::SecurityPolicy policy;
        policy.identity = identity;
        policy.enforcement_enabled = true;
        policy.agent_name = agent_config.name.empty() ? "default" : agent_config.name;

        // Parse agent tier
        if (agent_config.authorization_level == "approve") {
            policy.agent_tier = gnucash::audit::AuthorizationLevel::APPROVE;
        } else if (agent_config.authorization_level == "review") {
            policy.agent_tier = gnucash::audit::AuthorizationLevel::REVIEW;
        } else {
            policy.agent_tier = gnucash::audit::AuthorizationLevel::AUTO;
        }

        // TODO: Load rules from Dhall (dhall-to-json dhall/rules/authorization.dhall)
        // For now, use hardcoded defaults from the Dhall rules

        gnucash::mcp::set_security_policy(std::move(policy));
        std::cerr << "Security enforcement: ENABLED\n";
    }

    // Use MCP protocol (with legacy fallback)
    gnucash::mcp::run_mcp_loop();
    return 0;
}
