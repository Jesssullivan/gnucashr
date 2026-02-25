#include "gnucash/mcp.h"
#include "gnucash/dhall_config.h"
#include <iostream>
#include <string>

void print_usage(const char* prog) {
    std::cerr << "Usage: " << prog << " [OPTIONS]\n\n"
              << "Options:\n"
              << "  --agent <dhall-file>    Load agent configuration from Dhall file\n"
              << "                          (filters available tools to agent's list)\n"
              << "  --help                  Show this help message\n\n"
              << "Examples:\n"
              << "  " << prog << "  # Run with all tools available\n"
              << "  " << prog << " --agent dhall/agents/spend-monitor.dhall\n";
}

int main(int argc, char* argv[]) {
    // Parse command-line arguments
    std::string agent_config_path;

    for (int i = 1; i < argc; ++i) {
        std::string arg = argv[i];
        if (arg == "--help" || arg == "-h") {
            print_usage(argv[0]);
            return 0;
        } else if (arg == "--agent" && i + 1 < argc) {
            agent_config_path = argv[++i];
        } else {
            std::cerr << "Unknown argument: " << arg << "\n\n";
            print_usage(argv[0]);
            return 1;
        }
    }

    // Load agent config if provided
    if (!agent_config_path.empty()) {
        auto config_result = gnucash::dhall::parse_agent_config(agent_config_path);
        if (config_result.is_err()) {
            std::cerr << "Failed to load agent config: "
                      << config_result.unwrap_err() << "\n";
            return 1;
        }
        auto config = config_result.unwrap();
        std::cerr << "Loaded agent: " << config.name << "\n";
        std::cerr << "Tools: " << config.tools.size() << " tools available\n";

        // Set agent config in MCP layer
        gnucash::mcp::set_agent_config(config);
    }

    // Use MCP protocol (with legacy fallback)
    gnucash::mcp::run_mcp_loop();
    return 0;
}
