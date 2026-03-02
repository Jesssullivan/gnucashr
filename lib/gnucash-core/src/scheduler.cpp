#include "gnucash/scheduler.h"
#include "gnucash/agent.h"
#include "gnucash/book.h"
#include "gnucash/dhall_config.h"
#include "gnucash/identity.h"
#include "gnucash/audit.h"
#include <fstream>
#include <iostream>
#include <sstream>
#include <chrono>
#include <thread>
#include <ctime>
#include <cstring>

namespace gnucash {
namespace scheduler {

// ========================================================================
// Time Utilities
// ========================================================================

int64_t now_epoch() {
    return std::chrono::duration_cast<std::chrono::seconds>(
        std::chrono::system_clock::now().time_since_epoch()
    ).count();
}

int64_t parse_iso8601_epoch(const std::string& iso8601) {
    if (iso8601.empty()) return 0;

    struct std::tm tm = {};
    std::memset(&tm, 0, sizeof(tm));

    // Try "YYYY-MM-DD HH:MM:SS" and "YYYY-MM-DDTHH:MM:SS"
    const char* formats[] = {
        "%Y-%m-%d %H:%M:%S",
        "%Y-%m-%dT%H:%M:%S",
        nullptr
    };

    bool parsed = false;
    for (int i = 0; formats[i]; ++i) {
        std::istringstream ss(iso8601);
        ss >> std::get_time(&tm, formats[i]);
        if (!ss.fail()) {
            parsed = true;
            break;
        }
    }

    if (!parsed) return 0;

    // Convert to epoch (UTC)
#ifdef _WIN32
    time_t t = _mkgmtime(&tm);
#else
    time_t t = timegm(&tm);
#endif
    return static_cast<int64_t>(t);
}

// ========================================================================
// Schedule Interval
// ========================================================================

Result<ScheduleInterval> parse_schedule(const std::string& schedule_str) {
    if (schedule_str.empty() || schedule_str == "on-demand") {
        return Result<ScheduleInterval>::ok({IntervalType::ON_DEMAND, 0});
    }

    // Parse "hourly:N" or "daily:N"
    auto colon = schedule_str.find(':');
    if (colon == std::string::npos) {
        // Bare keywords
        if (schedule_str == "hourly") {
            return Result<ScheduleInterval>::ok({IntervalType::HOURLY, 1});
        } else if (schedule_str == "daily") {
            return Result<ScheduleInterval>::ok({IntervalType::DAILY, 1});
        }
        return Result<ScheduleInterval>::err(
            "Unknown schedule format: '" + schedule_str + "'");
    }

    std::string type_str = schedule_str.substr(0, colon);
    std::string value_str = schedule_str.substr(colon + 1);

    int value = 1;
    try {
        value = std::stoi(value_str);
    } catch (...) {
        return Result<ScheduleInterval>::err(
            "Invalid schedule value: '" + value_str + "'");
    }

    if (value <= 0) {
        return Result<ScheduleInterval>::err(
            "Schedule value must be positive, got: " + std::to_string(value));
    }

    if (type_str == "hourly") {
        return Result<ScheduleInterval>::ok({IntervalType::HOURLY, value});
    } else if (type_str == "daily") {
        return Result<ScheduleInterval>::ok({IntervalType::DAILY, value});
    }

    return Result<ScheduleInterval>::err(
        "Unknown schedule type: '" + type_str + "'");
}

bool is_due(const ScheduleInterval& interval, const std::string& last_run_iso8601) {
    // ON_DEMAND never auto-triggers
    if (interval.type == IntervalType::ON_DEMAND) {
        return false;
    }

    // If never run before, it's due
    if (last_run_iso8601.empty()) {
        return true;
    }

    int64_t last_epoch = parse_iso8601_epoch(last_run_iso8601);
    if (last_epoch == 0) {
        // Parse failure -- treat as never run
        return true;
    }

    int64_t current = now_epoch();
    int64_t elapsed = current - last_epoch;

    switch (interval.type) {
        case IntervalType::HOURLY:
            return elapsed >= (interval.value * 3600);
        case IntervalType::DAILY:
            return elapsed >= (interval.value * 86400);
        case IntervalType::ON_DEMAND:
            return false;
    }

    return false;
}

std::string interval_to_string(const ScheduleInterval& interval) {
    switch (interval.type) {
        case IntervalType::HOURLY:
            return "hourly:" + std::to_string(interval.value);
        case IntervalType::DAILY:
            return "daily:" + std::to_string(interval.value);
        case IntervalType::ON_DEMAND:
            return "on-demand";
    }
    return "unknown";
}

// ========================================================================
// Daemon Configuration
// ========================================================================

Result<DaemonConfig> parse_daemon_config(const std::string& json_path) {
    std::ifstream file(json_path);
    if (!file.is_open()) {
        return Result<DaemonConfig>::err("Cannot open config file: " + json_path);
    }

    json config;
    try {
        file >> config;
    } catch (const json::parse_error& e) {
        return Result<DaemonConfig>::err(
            "Failed to parse config JSON: " + std::string(e.what()));
    }

    DaemonConfig result;

    // Parse entries array
    if (!config.contains("entries") || !config["entries"].is_array()) {
        return Result<DaemonConfig>::err("Config missing 'entries' array");
    }

    for (const auto& entry : config["entries"]) {
        AgentEntry ae;
        if (!entry.contains("agent") || !entry["agent"].is_string()) {
            return Result<DaemonConfig>::err("Entry missing 'agent' string");
        }
        ae.agent_dhall_path = entry["agent"];

        if (!entry.contains("book") || !entry["book"].is_string()) {
            return Result<DaemonConfig>::err("Entry missing 'book' string");
        }
        ae.book_path = entry["book"];

        result.entries.push_back(std::move(ae));
    }

    // Optional fields
    if (config.contains("poll_interval_seconds") && config["poll_interval_seconds"].is_number_integer()) {
        result.poll_interval_seconds = config["poll_interval_seconds"].get<int>();
        if (result.poll_interval_seconds < 1) {
            return Result<DaemonConfig>::err("poll_interval_seconds must be >= 1");
        }
    }

    if (config.contains("log_file") && config["log_file"].is_string()) {
        result.log_file = config["log_file"].get<std::string>();
    }

    return Result<DaemonConfig>::ok(std::move(result));
}

// ========================================================================
// One-Shot Execution
// ========================================================================

int run_oneshot(const std::string& book_path,
                const std::string& agent_dhall_path,
                const std::optional<std::string>& cli_identity) {
    // Resolve identity
    auto identity = gnucash::resolve_identity(cli_identity);
    std::cerr << "Identity: " << identity.user_id
              << " (source: " << identity.source << ")\n";

    // Load agent config
    auto config_result = dhall::parse_agent_config(agent_dhall_path);
    if (config_result.is_err()) {
        std::cerr << "Failed to load agent config: "
                  << config_result.unwrap_err() << "\n";
        return 1;
    }
    auto agent_config = config_result.unwrap();
    std::cerr << "Agent: " << agent_config.name << "\n";

    // Open book
    auto book_result = Book::open(book_path);
    if (book_result.is_err()) {
        std::cerr << "Failed to open book: "
                  << book_result.unwrap_err() << "\n";
        return 1;
    }
    auto book = std::move(book_result.unwrap());

    // Open agent state DB
    auto state_result = agent::AgentStateDB::open(book_path, agent_config.name);
    if (state_result.is_err()) {
        std::cerr << "Failed to open agent state: "
                  << state_result.unwrap_err() << "\n";
        return 1;
    }
    auto state = std::move(state_result.unwrap());

    // Run agent
    auto run_result = agent::run_agent(agent_config.name, book, agent_config, state);
    if (run_result.is_err()) {
        std::cerr << "Agent failed: " << run_result.unwrap_err() << "\n";
        return 1;
    }

    auto result = run_result.unwrap();

    // Print report to stdout
    json output;
    output["agent"] = result.agent_name;
    output["timestamp"] = result.run_timestamp;
    output["records_processed"] = result.records_processed;
    output["actions_taken"] = result.actions_taken;
    output["report"] = result.report;

    std::cout << output.dump(2) << "\n";
    return 0;
}

// ========================================================================
// Daemon Loop
// ========================================================================

int run_daemon(const DaemonConfig& config,
               const std::optional<std::string>& cli_identity,
               std::atomic<bool>& running) {
    auto identity = gnucash::resolve_identity(cli_identity);
    std::cerr << "Daemon starting (identity: " << identity.user_id << ")\n";
    std::cerr << "Entries: " << config.entries.size()
              << ", poll interval: " << config.poll_interval_seconds << "s\n";

    while (running.load()) {
        for (const auto& entry : config.entries) {
            if (!running.load()) break;

            // Load agent config
            auto config_result = dhall::parse_agent_config(entry.agent_dhall_path);
            if (config_result.is_err()) {
                std::cerr << "[ERROR] Failed to load " << entry.agent_dhall_path
                          << ": " << config_result.unwrap_err() << "\n";
                continue;
            }
            auto agent_config = config_result.unwrap();

            // Parse schedule
            ScheduleInterval interval;
            if (agent_config.schedule.has_value()) {
                auto interval_result = parse_schedule(*agent_config.schedule);
                if (interval_result.is_err()) {
                    std::cerr << "[WARN] Bad schedule for " << agent_config.name
                              << ": " << interval_result.unwrap_err() << "\n";
                    continue;
                }
                interval = interval_result.unwrap();
            } else {
                // No schedule = on-demand, skip in daemon mode
                continue;
            }

            // Check if due
            auto state_result = agent::AgentStateDB::open(entry.book_path, agent_config.name);
            if (state_result.is_err()) {
                std::cerr << "[ERROR] Failed to open state for "
                          << agent_config.name << ": "
                          << state_result.unwrap_err() << "\n";
                continue;
            }
            auto state = std::move(state_result.unwrap());

            std::string last_run;
            auto last_run_result = state.get("last_daemon_run");
            if (last_run_result.is_ok()) {
                auto opt = last_run_result.unwrap();
                if (opt.has_value()) {
                    last_run = *opt;
                }
            }

            if (!is_due(interval, last_run)) {
                continue;
            }

            // Run agent
            std::cerr << "[RUN] " << agent_config.name
                      << " on " << entry.book_path << "\n";

            auto book_result = Book::open(entry.book_path);
            if (book_result.is_err()) {
                std::cerr << "[ERROR] Failed to open book "
                          << entry.book_path << ": "
                          << book_result.unwrap_err() << "\n";
                continue;
            }
            auto book = std::move(book_result.unwrap());

            auto run_result = agent::run_agent(agent_config.name, book, agent_config, state);
            if (run_result.is_err()) {
                std::cerr << "[ERROR] " << agent_config.name
                          << " failed: " << run_result.unwrap_err() << "\n";
            } else {
                auto result = run_result.unwrap();
                std::cerr << "[OK] " << agent_config.name
                          << ": " << result.records_processed << " records, "
                          << result.actions_taken << " actions\n";
            }

            // Update last run timestamp
            state.set("last_daemon_run", audit::now_iso8601());
        }

        // Sleep with periodic wakeup checks
        for (int i = 0; i < config.poll_interval_seconds && running.load(); ++i) {
            std::this_thread::sleep_for(std::chrono::seconds(1));
        }
    }

    std::cerr << "Daemon stopped\n";
    return 0;
}

} // namespace scheduler
} // namespace gnucash
