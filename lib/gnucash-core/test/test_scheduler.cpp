#include <catch2/catch_test_macros.hpp>
#include "gnucash/scheduler.h"
#include "gnucash/audit.h"
#include <fstream>
#include <filesystem>
#include <atomic>

using namespace gnucash::scheduler;

// ========================================================================
// parse_schedule
// ========================================================================

TEST_CASE("parse_schedule: hourly with value", "[scheduler]") {
    auto result = parse_schedule("hourly:1");
    REQUIRE(result.is_ok());
    auto interval = result.unwrap();
    CHECK(interval.type == IntervalType::HOURLY);
    CHECK(interval.value == 1);
}

TEST_CASE("parse_schedule: hourly:4", "[scheduler]") {
    auto result = parse_schedule("hourly:4");
    REQUIRE(result.is_ok());
    auto interval = result.unwrap();
    CHECK(interval.type == IntervalType::HOURLY);
    CHECK(interval.value == 4);
}

TEST_CASE("parse_schedule: daily with value", "[scheduler]") {
    auto result = parse_schedule("daily:2");
    REQUIRE(result.is_ok());
    auto interval = result.unwrap();
    CHECK(interval.type == IntervalType::DAILY);
    CHECK(interval.value == 2);
}

TEST_CASE("parse_schedule: bare hourly", "[scheduler]") {
    auto result = parse_schedule("hourly");
    REQUIRE(result.is_ok());
    auto interval = result.unwrap();
    CHECK(interval.type == IntervalType::HOURLY);
    CHECK(interval.value == 1);
}

TEST_CASE("parse_schedule: bare daily", "[scheduler]") {
    auto result = parse_schedule("daily");
    REQUIRE(result.is_ok());
    auto interval = result.unwrap();
    CHECK(interval.type == IntervalType::DAILY);
    CHECK(interval.value == 1);
}

TEST_CASE("parse_schedule: on-demand", "[scheduler]") {
    auto result = parse_schedule("on-demand");
    REQUIRE(result.is_ok());
    auto interval = result.unwrap();
    CHECK(interval.type == IntervalType::ON_DEMAND);
}

TEST_CASE("parse_schedule: empty string", "[scheduler]") {
    auto result = parse_schedule("");
    REQUIRE(result.is_ok());
    CHECK(result.unwrap().type == IntervalType::ON_DEMAND);
}

TEST_CASE("parse_schedule: invalid format", "[scheduler]") {
    auto result = parse_schedule("weekly:1");
    REQUIRE(result.is_err());
    CHECK(result.unwrap_err().find("Unknown schedule type") != std::string::npos);
}

TEST_CASE("parse_schedule: negative value", "[scheduler]") {
    auto result = parse_schedule("hourly:-1");
    REQUIRE(result.is_err());
}

TEST_CASE("parse_schedule: zero value", "[scheduler]") {
    auto result = parse_schedule("hourly:0");
    REQUIRE(result.is_err());
}

// ========================================================================
// is_due
// ========================================================================

TEST_CASE("is_due: on-demand never due", "[scheduler]") {
    ScheduleInterval interval{IntervalType::ON_DEMAND, 0};
    CHECK_FALSE(is_due(interval, ""));
    CHECK_FALSE(is_due(interval, "2020-01-01 00:00:00"));
}

TEST_CASE("is_due: hourly, never run -> due", "[scheduler]") {
    ScheduleInterval interval{IntervalType::HOURLY, 1};
    CHECK(is_due(interval, ""));
}

TEST_CASE("is_due: hourly, run recently -> not due", "[scheduler]") {
    ScheduleInterval interval{IntervalType::HOURLY, 1};
    // Use current time as last run
    auto now = gnucash::audit::now_iso8601();
    CHECK_FALSE(is_due(interval, now));
}

TEST_CASE("is_due: hourly, run long ago -> due", "[scheduler]") {
    ScheduleInterval interval{IntervalType::HOURLY, 1};
    CHECK(is_due(interval, "2020-01-01 00:00:00"));
}

TEST_CASE("is_due: daily, run today -> not due", "[scheduler]") {
    ScheduleInterval interval{IntervalType::DAILY, 1};
    auto now = gnucash::audit::now_iso8601();
    CHECK_FALSE(is_due(interval, now));
}

TEST_CASE("is_due: invalid timestamp -> due (treated as never run)", "[scheduler]") {
    ScheduleInterval interval{IntervalType::HOURLY, 1};
    CHECK(is_due(interval, "not-a-timestamp"));
}

// ========================================================================
// interval_to_string
// ========================================================================

TEST_CASE("interval_to_string: roundtrip", "[scheduler]") {
    CHECK(interval_to_string({IntervalType::HOURLY, 1}) == "hourly:1");
    CHECK(interval_to_string({IntervalType::DAILY, 2}) == "daily:2");
    CHECK(interval_to_string({IntervalType::ON_DEMAND, 0}) == "on-demand");
}

// ========================================================================
// parse_iso8601_epoch
// ========================================================================

TEST_CASE("parse_iso8601_epoch: valid timestamp", "[scheduler]") {
    // 2020-01-01 00:00:00 UTC = 1577836800
    auto epoch = parse_iso8601_epoch("2020-01-01 00:00:00");
    CHECK(epoch == 1577836800);
}

TEST_CASE("parse_iso8601_epoch: T separator", "[scheduler]") {
    auto epoch = parse_iso8601_epoch("2020-01-01T00:00:00");
    CHECK(epoch == 1577836800);
}

TEST_CASE("parse_iso8601_epoch: empty string", "[scheduler]") {
    CHECK(parse_iso8601_epoch("") == 0);
}

TEST_CASE("parse_iso8601_epoch: garbage", "[scheduler]") {
    CHECK(parse_iso8601_epoch("not-a-date") == 0);
}

// ========================================================================
// parse_daemon_config
// ========================================================================

TEST_CASE("parse_daemon_config: valid config", "[scheduler]") {
    // Write temp config file
    std::string config_path = std::string(FIXTURE_DIR) + "/daemon-config.json";
    auto result = parse_daemon_config(config_path);
    REQUIRE(result.is_ok());
    auto config = result.unwrap();
    CHECK(config.entries.size() == 2);
    CHECK(config.entries[0].agent_dhall_path == "dhall/agents/spend-monitor.dhall");
    CHECK(config.entries[0].book_path == "test.gnucash");
    CHECK(config.poll_interval_seconds == 30);
}

TEST_CASE("parse_daemon_config: missing file", "[scheduler]") {
    auto result = parse_daemon_config("/nonexistent/path.json");
    REQUIRE(result.is_err());
    CHECK(result.unwrap_err().find("Cannot open") != std::string::npos);
}

TEST_CASE("parse_daemon_config: invalid JSON", "[scheduler]") {
    // Create temp file with bad JSON
    auto tmp = std::string(FIXTURE_DIR) + "/bad-daemon.json";
    {
        std::ofstream f(tmp);
        f << "not json";
    }
    auto result = parse_daemon_config(tmp);
    REQUIRE(result.is_err());
    std::filesystem::remove(tmp);
}

TEST_CASE("parse_daemon_config: missing entries", "[scheduler]") {
    auto tmp = std::string(FIXTURE_DIR) + "/no-entries.json";
    {
        std::ofstream f(tmp);
        f << R"({"poll_interval_seconds": 10})";
    }
    auto result = parse_daemon_config(tmp);
    REQUIRE(result.is_err());
    CHECK(result.unwrap_err().find("entries") != std::string::npos);
    std::filesystem::remove(tmp);
}
