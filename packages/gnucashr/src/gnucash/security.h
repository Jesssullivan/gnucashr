#pragma once
// Security sidecar: authorization enforcement, rate limiting, anomaly detection
// Sits between MCP dispatch and tool execution
// Vendored from lib/gnucash-core for R package use

#include "result.h"
#include "identity.h"
#include "audit.h"
#include <string>
#include <vector>
#include <optional>
#include <map>
#include <deque>
#include <chrono>
#include <nlohmann/json.hpp>

namespace gnucash {

using json = nlohmann::json;

// --- Authorization Rule (mirrors Dhall AgentRule) ---

struct AuthorizationRule {
    std::string name;
    std::string description;
    audit::AuthorizationLevel level;
    std::optional<int64_t> max_amount_cents;
    int max_per_hour;
    bool enabled;
};

// --- Security Decision ---

enum class SecurityDecision {
    ALLOW,             // Proceed with execution
    QUEUE_REVIEW,      // Store for human review, return pending
    REQUIRE_APPROVAL,  // Block until approved
    DENY               // Reject outright (rate limit, disabled rule, etc.)
};

std::string security_decision_to_string(SecurityDecision d);

struct SecurityResult {
    SecurityDecision decision;
    std::string reason;
    std::optional<std::string> approval_id;
};

// --- Rate Limiter ---

class RateLimiter {
public:
    // Check if operation is within rate limit and record the attempt
    bool check_and_record(const std::string& agent, const std::string& operation,
                          int max_per_hour);

    // How many requests remaining in the current window
    int remaining(const std::string& agent, const std::string& operation,
                  int max_per_hour) const;

    // Clear all windows (for testing)
    void reset();

private:
    using TimePoint = std::chrono::steady_clock::time_point;
    mutable std::map<std::string, std::deque<TimePoint>> windows_;

    static std::string make_key(const std::string& agent, const std::string& op);
    void prune(const std::string& key) const;
};

// --- MCP Tool Classification ---

// Classify an MCP tool name into an authorization level
// Read tools -> AUTO, write tools -> APPROVE (unless overridden by rules)
audit::AuthorizationLevel classify_tool(const std::string& tool_name);

// Map MCP tool names to Dhall rule operation names
// e.g., "gnucash_post_transaction" -> "post_transaction"
std::string tool_to_operation(const std::string& tool_name);

// --- Security Policy ---

struct SecurityPolicy {
    std::vector<AuthorizationRule> rules;
    Identity identity;
    bool enforcement_enabled = false;
    std::string agent_name;
    audit::AuthorizationLevel agent_tier = audit::AuthorizationLevel::AUTO;

    // Find rule for a given operation
    const AuthorizationRule* find_rule(const std::string& operation) const;
};

// Load authorization rules from Dhall JSON (dhall-to-json output)
std::vector<AuthorizationRule> parse_authorization_rules(const json& rules_json);

// --- Security Check ---

// Main entry point: check whether a tool call should proceed
SecurityResult security_check(const SecurityPolicy& policy,
                              const std::string& tool_name,
                              const json& arguments,
                              RateLimiter& limiter);

// --- Anomaly Detection ---

struct AnomalyCheck {
    bool is_anomalous = false;
    std::string reason;
    double severity = 0.0;  // 0.0-1.0
};

// Check a transaction for anomalies
AnomalyCheck check_transaction_anomaly(const json& arguments,
                                        int64_t amount_threshold_cents = 500000);

// Extract transaction amount from tool arguments (if present)
std::optional<int64_t> extract_amount_cents(const json& arguments);

} // namespace gnucash
