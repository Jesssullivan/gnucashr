#include "gnucash/dhall_config.h"
#include <nlohmann/json.hpp>
#include <cstdlib>
#include <array>
#include <memory>
#include <stdexcept>
#include <sstream>

namespace gnucash {
namespace dhall {

using json = nlohmann::json;

namespace {

// Execute command and capture stdout
std::string exec(const std::string& cmd) {
    std::array<char, 128> buffer;
    std::string result;
    std::unique_ptr<FILE, int(*)(FILE*)> pipe(popen(cmd.c_str(), "r"), pclose);
    if (!pipe) {
        throw std::runtime_error("popen() failed");
    }
    while (fgets(buffer.data(), buffer.size(), pipe.get()) != nullptr) {
        result += buffer.data();
    }
    return result;
}

} // anonymous namespace

bool dhall_available() {
    try {
        exec("which dhall-to-json 2>/dev/null");
        return true;
    } catch (...) {
        return false;
    }
}

Result<AgentConfig> parse_agent_config(const std::string& dhall_path) {
    // Check if dhall-to-json is available
    if (!dhall_available()) {
        return Result<AgentConfig>::err(
            "dhall-to-json not found. Install dhall: https://dhall-lang.org/");
    }

    // Execute dhall-to-json
    std::string cmd = "dhall-to-json --file \"" + dhall_path + "\" 2>&1";
    std::string output;
    try {
        output = exec(cmd);
    } catch (const std::exception& e) {
        return Result<AgentConfig>::err(
            "Failed to execute dhall-to-json: " + std::string(e.what()));
    }

    // Parse JSON output
    json config;
    try {
        config = json::parse(output);
    } catch (const json::parse_error& e) {
        return Result<AgentConfig>::err(
            "Failed to parse dhall-to-json output: " + std::string(e.what()) +
            "\nOutput: " + output);
    }

    // Extract fields
    AgentConfig result;

    if (!config.contains("name") || !config["name"].is_string()) {
        return Result<AgentConfig>::err("Agent config missing 'name' field");
    }
    result.name = config["name"];

    if (!config.contains("description") || !config["description"].is_string()) {
        return Result<AgentConfig>::err("Agent config missing 'description' field");
    }
    result.description = config["description"];

    if (!config.contains("tools") || !config["tools"].is_array()) {
        return Result<AgentConfig>::err("Agent config missing 'tools' array");
    }
    for (const auto& tool : config["tools"]) {
        if (tool.is_string()) {
            result.tools.push_back(tool);
        }
    }

    if (!config.contains("authorization_level") || !config["authorization_level"].is_string()) {
        return Result<AgentConfig>::err("Agent config missing 'authorization_level' field");
    }
    result.authorization_level = config["authorization_level"];

    // Schedule is optional
    if (config.contains("schedule") && !config["schedule"].is_null()) {
        if (config["schedule"].is_string()) {
            result.schedule = config["schedule"];
        } else if (config["schedule"].is_object()) {
            // Handle tagged union (Hourly/Daily/OnDemand)
            if (config["schedule"].contains("Hourly")) {
                result.schedule = "hourly:" + std::to_string(
                    config["schedule"]["Hourly"].get<int>());
            } else if (config["schedule"].contains("Daily")) {
                result.schedule = "daily:" + std::to_string(
                    config["schedule"]["Daily"].get<int>());
            } else if (config["schedule"].contains("OnDemand")) {
                result.schedule = "on-demand";
            }
        }
    }

    return Result<AgentConfig>::ok(std::move(result));
}

} // namespace dhall
} // namespace gnucash
