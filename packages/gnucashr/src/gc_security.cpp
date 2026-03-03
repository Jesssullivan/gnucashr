// Vendored from lib/gnucash-core/src/security.cpp
// Adapted for R package build (gnucash/ prefix headers)

#include "gnucash/security.h"
#include <algorithm>
#include <cmath>
#include <numeric>

namespace gnucash {

// --- SecurityDecision ---

std::string security_decision_to_string(SecurityDecision d) {
    switch (d) {
        case SecurityDecision::ALLOW:            return "allow";
        case SecurityDecision::QUEUE_REVIEW:     return "queue_review";
        case SecurityDecision::REQUIRE_APPROVAL: return "require_approval";
        case SecurityDecision::DENY:             return "deny";
    }
    return "unknown";
}

// --- RateLimiter ---

std::string RateLimiter::make_key(const std::string& agent, const std::string& op) {
    return agent + ":" + op;
}

void RateLimiter::prune(const std::string& key) const {
    auto it = windows_.find(key);
    if (it == windows_.end()) return;

    auto& dq = it->second;
    auto cutoff = std::chrono::steady_clock::now() - std::chrono::hours(1);
    while (!dq.empty() && dq.front() < cutoff) {
        dq.pop_front();
    }
}

bool RateLimiter::check_and_record(const std::string& agent, const std::string& operation,
                                    int max_per_hour) {
    auto key = make_key(agent, operation);
    prune(key);

    auto& dq = windows_[key];
    if (static_cast<int>(dq.size()) >= max_per_hour) {
        return false;
    }

    dq.push_back(std::chrono::steady_clock::now());
    return true;
}

int RateLimiter::remaining(const std::string& agent, const std::string& operation,
                            int max_per_hour) const {
    auto key = make_key(agent, operation);
    prune(key);

    auto it = windows_.find(key);
    if (it == windows_.end()) return max_per_hour;

    int used = static_cast<int>(it->second.size());
    return std::max(0, max_per_hour - used);
}

void RateLimiter::reset() {
    windows_.clear();
}

// --- Tool Classification ---

audit::AuthorizationLevel classify_tool(const std::string& tool_name) {
    // Write tools require approval
    if (tool_name == "gnucash_create_account" ||
        tool_name == "gnucash_post_transaction" ||
        tool_name == "gnucash_delete_transaction" ||
        tool_name == "gnucash_void_transaction") {
        return audit::AuthorizationLevel::APPROVE;
    }
    // Everything else is read-only
    return audit::AuthorizationLevel::AUTO;
}

std::string tool_to_operation(const std::string& tool_name) {
    // Strip "gnucash_" prefix to get operation name
    const std::string prefix = "gnucash_";
    if (tool_name.substr(0, prefix.size()) == prefix) {
        return tool_name.substr(prefix.size());
    }
    return tool_name;
}

// --- SecurityPolicy ---

const AuthorizationRule* SecurityPolicy::find_rule(const std::string& operation) const {
    for (const auto& rule : rules) {
        if (rule.name == operation) {
            return &rule;
        }
    }
    return nullptr;
}

// --- Parse rules from Dhall JSON ---

std::vector<AuthorizationRule> parse_authorization_rules(const json& rules_json) {
    std::vector<AuthorizationRule> result;

    if (!rules_json.is_array()) return result;

    for (const auto& r : rules_json) {
        AuthorizationRule rule;
        rule.name = r.value("name", "");
        rule.description = r.value("description", "");

        // Parse authorization level
        std::string level = r.value("authorization", "Auto");
        if (level == "Approve" || level == "approve") {
            rule.level = audit::AuthorizationLevel::APPROVE;
        } else if (level == "Review" || level == "review") {
            rule.level = audit::AuthorizationLevel::REVIEW;
        } else {
            rule.level = audit::AuthorizationLevel::AUTO;
        }

        // max_amount_cents: Dhall Optional Natural
        if (r.contains("max_amount_cents") && !r["max_amount_cents"].is_null()) {
            rule.max_amount_cents = r["max_amount_cents"].get<int64_t>();
        }

        rule.max_per_hour = r.value("rate_limit_per_hour", 60);
        rule.enabled = r.value("enabled", true);

        if (!rule.name.empty()) {
            result.push_back(std::move(rule));
        }
    }

    return result;
}

// --- Security Check ---

SecurityResult security_check(const SecurityPolicy& policy,
                              const std::string& tool_name,
                              const json& arguments,
                              RateLimiter& limiter) {
    // If enforcement is disabled, always allow
    if (!policy.enforcement_enabled) {
        return {SecurityDecision::ALLOW, "enforcement disabled", std::nullopt};
    }

    // Classify the tool
    auto tool_level = classify_tool(tool_name);

    // Read operations always allowed
    if (tool_level == audit::AuthorizationLevel::AUTO) {
        return {SecurityDecision::ALLOW, "read operation", std::nullopt};
    }

    // Map tool to operation name and find matching rule
    auto operation = tool_to_operation(tool_name);
    const auto* rule = policy.find_rule(operation);

    // If no rule found, use the tool's inherent classification
    audit::AuthorizationLevel effective_level = rule ? rule->level : tool_level;

    // Check if rule is disabled
    if (rule && !rule->enabled) {
        return {SecurityDecision::DENY, "operation '" + operation + "' is disabled", std::nullopt};
    }

    // Check rate limit
    int max_rate = rule ? rule->max_per_hour : 20;  // Default 20/hr for unlisted ops
    if (!limiter.check_and_record(policy.agent_name, operation, max_rate)) {
        return {SecurityDecision::DENY,
                "rate limit exceeded for '" + operation + "' (" +
                std::to_string(max_rate) + "/hr)",
                std::nullopt};
    }

    // Check amount limits
    if (rule && rule->max_amount_cents.has_value()) {
        auto amount = extract_amount_cents(arguments);
        if (amount.has_value() && *amount > *rule->max_amount_cents) {
            return {SecurityDecision::REQUIRE_APPROVAL,
                    "amount " + std::to_string(*amount) +
                    " cents exceeds limit of " + std::to_string(*rule->max_amount_cents) +
                    " cents for '" + operation + "'",
                    std::nullopt};
        }
    }

    // Check anomalies
    auto anomaly = check_transaction_anomaly(arguments);
    if (anomaly.is_anomalous && anomaly.severity > 0.7) {
        return {SecurityDecision::REQUIRE_APPROVAL,
                "anomaly detected: " + anomaly.reason,
                std::nullopt};
    }

    // Apply authorization level
    switch (effective_level) {
        case audit::AuthorizationLevel::AUTO:
            return {SecurityDecision::ALLOW, "auto-approved", std::nullopt};

        case audit::AuthorizationLevel::REVIEW:
            return {SecurityDecision::QUEUE_REVIEW,
                    "operation '" + operation + "' requires review",
                    std::nullopt};

        case audit::AuthorizationLevel::APPROVE:
            // Check agent's tier - if agent is Approve-tier, it has pre-authorization
            if (policy.agent_tier == audit::AuthorizationLevel::APPROVE) {
                return {SecurityDecision::ALLOW,
                        "agent '" + policy.agent_name + "' has Approve tier",
                        std::nullopt};
            }
            return {SecurityDecision::REQUIRE_APPROVAL,
                    "operation '" + operation + "' requires approval",
                    std::nullopt};
    }

    return {SecurityDecision::DENY, "unknown authorization state", std::nullopt};
}

// --- Anomaly Detection ---

std::optional<int64_t> extract_amount_cents(const json& arguments) {
    // Check splits for total amount
    if (arguments.contains("splits") && arguments["splits"].is_array()) {
        int64_t max_amount = 0;
        for (const auto& split : arguments["splits"]) {
            int64_t num = split.value("value_num", int64_t(0));
            int64_t denom = split.value("value_denom", int64_t(100));
            if (denom == 0) denom = 100;
            // Normalize to cents (denom=100)
            int64_t cents = std::abs(num * 100 / denom);
            max_amount = std::max(max_amount, cents);
        }
        if (max_amount > 0) return max_amount;
    }

    // Check direct amount field
    if (arguments.contains("amount_cents")) {
        return std::abs(arguments["amount_cents"].get<int64_t>());
    }
    if (arguments.contains("amount")) {
        return static_cast<int64_t>(std::abs(arguments["amount"].get<double>()) * 100);
    }

    return std::nullopt;
}

AnomalyCheck check_transaction_anomaly(const json& arguments,
                                        int64_t amount_threshold_cents) {
    AnomalyCheck result;

    auto amount = extract_amount_cents(arguments);
    if (amount.has_value() && *amount > amount_threshold_cents) {
        result.is_anomalous = true;
        result.reason = "transaction amount $" +
                        std::to_string(*amount / 100) + "." +
                        std::to_string(std::abs(*amount % 100)) +
                        " exceeds threshold $" +
                        std::to_string(amount_threshold_cents / 100);
        result.severity = std::min(1.0, static_cast<double>(*amount) /
                                        static_cast<double>(amount_threshold_cents));
    }

    return result;
}

} // namespace gnucash
