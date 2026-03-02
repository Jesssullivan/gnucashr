#pragma once
// Schedule Executor for gnucash-bridge
// Provides --run (one-shot) and --daemon (persistent loop) modes
// Parses schedule intervals from Dhall agent configs

#include "result.h"
#include <string>
#include <vector>
#include <optional>
#include <atomic>
#include <nlohmann/json.hpp>

namespace gnucash {
namespace scheduler {

using json = nlohmann::json;

// ========================================================================
// Schedule Interval
// ========================================================================

enum class IntervalType {
    HOURLY,
    DAILY,
    ON_DEMAND
};

struct ScheduleInterval {
    IntervalType type = IntervalType::ON_DEMAND;
    int value = 1;  // e.g., hourly:2 means every 2 hours
};

/// Parse schedule string from dhall_config ("hourly:1", "daily:2", "on-demand")
Result<ScheduleInterval> parse_schedule(const std::string& schedule_str);

/// Check if an agent is due to run based on its schedule and last run time
/// Returns true if enough time has elapsed since last_run_iso8601
bool is_due(const ScheduleInterval& interval, const std::string& last_run_iso8601);

/// Convert interval to human-readable string
std::string interval_to_string(const ScheduleInterval& interval);

// ========================================================================
// Daemon Configuration
// ========================================================================

struct AgentEntry {
    std::string agent_dhall_path;   // Path to Dhall agent config
    std::string book_path;          // Path to GnuCash file
};

struct DaemonConfig {
    std::vector<AgentEntry> entries;
    int poll_interval_seconds = 60;
    std::optional<std::string> log_file;
};

/// Parse daemon configuration from JSON file
Result<DaemonConfig> parse_daemon_config(const std::string& json_path);

// ========================================================================
// One-Shot Execution
// ========================================================================

/// Run a single agent against a book (--run mode)
/// Opens book, loads agent config, runs agent, prints report to stdout
int run_oneshot(const std::string& book_path,
                const std::string& agent_dhall_path,
                const std::optional<std::string>& cli_identity);

// ========================================================================
// Daemon Loop
// ========================================================================

/// Run the daemon loop (--daemon mode)
/// Polls agents on their schedules, exits on SIGTERM/SIGINT
int run_daemon(const DaemonConfig& config,
               const std::optional<std::string>& cli_identity,
               std::atomic<bool>& running);

// ========================================================================
// Time Utilities
// ========================================================================

/// Parse ISO 8601 timestamp to epoch seconds (UTC)
/// Returns 0 on parse failure
int64_t parse_iso8601_epoch(const std::string& iso8601);

/// Get current time as epoch seconds
int64_t now_epoch();

} // namespace scheduler
} // namespace gnucash
