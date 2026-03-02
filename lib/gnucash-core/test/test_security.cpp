#include <catch2/catch_test_macros.hpp>
#include <catch2/matchers/catch_matchers_floating_point.hpp>
#include "gnucash/identity.h"
#include "gnucash/security.h"
#include "gnucash/approval.h"
#include "gnucash/guid.h"
#include <filesystem>
#include <cstdlib>

using namespace gnucash;
using Catch::Matchers::WithinAbs;
namespace fs = std::filesystem;

// ========================================================================
// Identity Tests
// ========================================================================

TEST_CASE("resolve_identity: CLI flag takes priority", "[security][identity]") {
    auto id = resolve_identity("user@example.com");
    REQUIRE(id.user_id == "user@example.com");
    REQUIRE(id.source == "cli");
    REQUIRE_FALSE(id.node_name.empty());
}

TEST_CASE("resolve_identity: system fallback", "[security][identity]") {
    // Ensure GNUCASH_USER is not set
    unsetenv("GNUCASH_USER");
    auto id = resolve_identity(std::nullopt);
    REQUIRE_FALSE(id.user_id.empty());
    REQUIRE(id.source == "system");
    REQUIRE_FALSE(id.node_name.empty());
}

TEST_CASE("resolve_identity: env var", "[security][identity]") {
    setenv("GNUCASH_USER", "envuser@test.com", 1);
    auto id = resolve_identity(std::nullopt);
    REQUIRE(id.user_id == "envuser@test.com");
    REQUIRE(id.source == "env");
    unsetenv("GNUCASH_USER");
}

TEST_CASE("resolve_identity: CLI overrides env", "[security][identity]") {
    setenv("GNUCASH_USER", "env@test.com", 1);
    auto id = resolve_identity("cli@test.com");
    REQUIRE(id.user_id == "cli@test.com");
    REQUIRE(id.source == "cli");
    unsetenv("GNUCASH_USER");
}

TEST_CASE("get_system_username: returns non-empty", "[security][identity]") {
    auto username = get_system_username();
    REQUIRE_FALSE(username.empty());
}

TEST_CASE("get_hostname: returns non-empty", "[security][identity]") {
    auto hostname = get_hostname();
    REQUIRE_FALSE(hostname.empty());
}

// ========================================================================
// Tool Classification Tests
// ========================================================================

TEST_CASE("classify_tool: read tools are AUTO", "[security][classify]") {
    REQUIRE(classify_tool("gnucash_open") == audit::AuthorizationLevel::AUTO);
    REQUIRE(classify_tool("gnucash_get_accounts") == audit::AuthorizationLevel::AUTO);
    REQUIRE(classify_tool("gnucash_get_transactions") == audit::AuthorizationLevel::AUTO);
    REQUIRE(classify_tool("gnucash_trial_balance") == audit::AuthorizationLevel::AUTO);
    REQUIRE(classify_tool("gnucash_audit_log") == audit::AuthorizationLevel::AUTO);
}

TEST_CASE("classify_tool: write tools are APPROVE", "[security][classify]") {
    REQUIRE(classify_tool("gnucash_create_account") == audit::AuthorizationLevel::APPROVE);
    REQUIRE(classify_tool("gnucash_post_transaction") == audit::AuthorizationLevel::APPROVE);
    REQUIRE(classify_tool("gnucash_delete_transaction") == audit::AuthorizationLevel::APPROVE);
    REQUIRE(classify_tool("gnucash_void_transaction") == audit::AuthorizationLevel::APPROVE);
}

TEST_CASE("tool_to_operation: strips gnucash_ prefix", "[security][classify]") {
    REQUIRE(tool_to_operation("gnucash_post_transaction") == "post_transaction");
    REQUIRE(tool_to_operation("gnucash_create_account") == "create_account");
    REQUIRE(tool_to_operation("other_tool") == "other_tool");
}

// ========================================================================
// SecurityDecision Tests
// ========================================================================

TEST_CASE("security_decision_to_string", "[security]") {
    REQUIRE(security_decision_to_string(SecurityDecision::ALLOW) == "allow");
    REQUIRE(security_decision_to_string(SecurityDecision::DENY) == "deny");
    REQUIRE(security_decision_to_string(SecurityDecision::QUEUE_REVIEW) == "queue_review");
    REQUIRE(security_decision_to_string(SecurityDecision::REQUIRE_APPROVAL) == "require_approval");
}

// ========================================================================
// Rate Limiter Tests
// ========================================================================

TEST_CASE("RateLimiter: allows within limit", "[security][rate]") {
    RateLimiter limiter;
    for (int i = 0; i < 5; i++) {
        REQUIRE(limiter.check_and_record("agent1", "post_transaction", 5));
    }
    // 6th should fail
    REQUIRE_FALSE(limiter.check_and_record("agent1", "post_transaction", 5));
}

TEST_CASE("RateLimiter: different agents independent", "[security][rate]") {
    RateLimiter limiter;
    REQUIRE(limiter.check_and_record("agent1", "post_transaction", 1));
    REQUIRE_FALSE(limiter.check_and_record("agent1", "post_transaction", 1));
    // Different agent still has quota
    REQUIRE(limiter.check_and_record("agent2", "post_transaction", 1));
}

TEST_CASE("RateLimiter: different operations independent", "[security][rate]") {
    RateLimiter limiter;
    REQUIRE(limiter.check_and_record("agent1", "post_transaction", 1));
    REQUIRE_FALSE(limiter.check_and_record("agent1", "post_transaction", 1));
    // Different operation still has quota
    REQUIRE(limiter.check_and_record("agent1", "create_account", 1));
}

TEST_CASE("RateLimiter: remaining count", "[security][rate]") {
    RateLimiter limiter;
    REQUIRE(limiter.remaining("agent1", "op", 10) == 10);
    limiter.check_and_record("agent1", "op", 10);
    REQUIRE(limiter.remaining("agent1", "op", 10) == 9);
}

TEST_CASE("RateLimiter: reset clears all", "[security][rate]") {
    RateLimiter limiter;
    limiter.check_and_record("agent1", "op", 1);
    REQUIRE_FALSE(limiter.check_and_record("agent1", "op", 1));
    limiter.reset();
    REQUIRE(limiter.check_and_record("agent1", "op", 1));
}

// ========================================================================
// Security Check Tests
// ========================================================================

TEST_CASE("security_check: enforcement disabled always allows", "[security][check]") {
    SecurityPolicy policy;
    policy.enforcement_enabled = false;
    RateLimiter limiter;

    auto result = security_check(policy, "gnucash_post_transaction", {}, limiter);
    REQUIRE(result.decision == SecurityDecision::ALLOW);
    REQUIRE(result.reason == "enforcement disabled");
}

TEST_CASE("security_check: read operations always allowed", "[security][check]") {
    SecurityPolicy policy;
    policy.enforcement_enabled = true;
    policy.agent_name = "test";
    RateLimiter limiter;

    auto result = security_check(policy, "gnucash_get_accounts", {}, limiter);
    REQUIRE(result.decision == SecurityDecision::ALLOW);
    REQUIRE(result.reason == "read operation");
}

TEST_CASE("security_check: write op with no rule requires approval", "[security][check]") {
    SecurityPolicy policy;
    policy.enforcement_enabled = true;
    policy.agent_name = "test";
    policy.agent_tier = audit::AuthorizationLevel::AUTO;
    RateLimiter limiter;

    auto result = security_check(policy, "gnucash_post_transaction", {}, limiter);
    REQUIRE(result.decision == SecurityDecision::REQUIRE_APPROVAL);
}

TEST_CASE("security_check: approve-tier agent auto-allows writes", "[security][check]") {
    SecurityPolicy policy;
    policy.enforcement_enabled = true;
    policy.agent_name = "privileged";
    policy.agent_tier = audit::AuthorizationLevel::APPROVE;
    RateLimiter limiter;

    auto result = security_check(policy, "gnucash_post_transaction", {}, limiter);
    REQUIRE(result.decision == SecurityDecision::ALLOW);
}

TEST_CASE("security_check: rate limit denies", "[security][check]") {
    SecurityPolicy policy;
    policy.enforcement_enabled = true;
    policy.agent_name = "test";
    policy.agent_tier = audit::AuthorizationLevel::APPROVE;

    AuthorizationRule rule;
    rule.name = "post_transaction";
    rule.level = audit::AuthorizationLevel::APPROVE;
    rule.max_per_hour = 2;
    rule.enabled = true;
    policy.rules.push_back(rule);

    RateLimiter limiter;

    // First two should succeed
    REQUIRE(security_check(policy, "gnucash_post_transaction", {}, limiter).decision == SecurityDecision::ALLOW);
    REQUIRE(security_check(policy, "gnucash_post_transaction", {}, limiter).decision == SecurityDecision::ALLOW);
    // Third should be denied (rate limit)
    auto result = security_check(policy, "gnucash_post_transaction", {}, limiter);
    REQUIRE(result.decision == SecurityDecision::DENY);
    REQUIRE(result.reason.find("rate limit") != std::string::npos);
}

TEST_CASE("security_check: disabled rule denies", "[security][check]") {
    SecurityPolicy policy;
    policy.enforcement_enabled = true;
    policy.agent_name = "test";

    AuthorizationRule rule;
    rule.name = "post_transaction";
    rule.level = audit::AuthorizationLevel::APPROVE;
    rule.max_per_hour = 100;
    rule.enabled = false;
    policy.rules.push_back(rule);

    RateLimiter limiter;
    auto result = security_check(policy, "gnucash_post_transaction", {}, limiter);
    REQUIRE(result.decision == SecurityDecision::DENY);
    REQUIRE(result.reason.find("disabled") != std::string::npos);
}

TEST_CASE("security_check: review tier queues", "[security][check]") {
    SecurityPolicy policy;
    policy.enforcement_enabled = true;
    policy.agent_name = "categorizer";
    policy.agent_tier = audit::AuthorizationLevel::AUTO;

    AuthorizationRule rule;
    rule.name = "post_transaction";
    rule.level = audit::AuthorizationLevel::REVIEW;
    rule.max_per_hour = 100;
    rule.enabled = true;
    policy.rules.push_back(rule);

    RateLimiter limiter;
    auto result = security_check(policy, "gnucash_post_transaction", {}, limiter);
    REQUIRE(result.decision == SecurityDecision::QUEUE_REVIEW);
}

// ========================================================================
// Amount Extraction Tests
// ========================================================================

TEST_CASE("extract_amount_cents: from splits", "[security][amount]") {
    json args = {{"splits", {{{"value_num", 5000}, {"value_denom", 100}}}}};
    auto amount = extract_amount_cents(args);
    REQUIRE(amount.has_value());
    REQUIRE(*amount == 5000);
}

TEST_CASE("extract_amount_cents: from amount field", "[security][amount]") {
    json args = {{"amount", 150.75}};
    auto amount = extract_amount_cents(args);
    REQUIRE(amount.has_value());
    REQUIRE(*amount == 15075);
}

TEST_CASE("extract_amount_cents: no amount returns nullopt", "[security][amount]") {
    json args = {{"description", "test"}};
    auto amount = extract_amount_cents(args);
    REQUIRE_FALSE(amount.has_value());
}

// ========================================================================
// Anomaly Detection Tests
// ========================================================================

TEST_CASE("check_transaction_anomaly: normal amount", "[security][anomaly]") {
    json args = {{"amount", 50.00}};
    auto check = check_transaction_anomaly(args, 500000);  // $5000 threshold
    REQUIRE_FALSE(check.is_anomalous);
}

TEST_CASE("check_transaction_anomaly: large amount flags", "[security][anomaly]") {
    json args = {{"amount", 10000.00}};
    auto check = check_transaction_anomaly(args, 500000);  // $5000 threshold
    REQUIRE(check.is_anomalous);
    REQUIRE(check.severity > 0.0);
    REQUIRE(check.reason.find("exceeds threshold") != std::string::npos);
}

// ========================================================================
// Parse Authorization Rules Tests
// ========================================================================

TEST_CASE("parse_authorization_rules: from JSON", "[security][rules]") {
    json rules_json = json::array({
        {{"name", "post_transaction"}, {"authorization", "Approve"},
         {"max_amount_cents", 100000}, {"rate_limit_per_hour", 20}, {"enabled", true}},
        {{"name", "read_balances"}, {"authorization", "Auto"},
         {"rate_limit_per_hour", 120}, {"enabled", true}},
    });

    auto rules = parse_authorization_rules(rules_json);
    REQUIRE(rules.size() == 2);
    REQUIRE(rules[0].name == "post_transaction");
    REQUIRE(rules[0].level == audit::AuthorizationLevel::APPROVE);
    REQUIRE(rules[0].max_amount_cents.value() == 100000);
    REQUIRE(rules[0].max_per_hour == 20);
    REQUIRE(rules[1].level == audit::AuthorizationLevel::AUTO);
}

TEST_CASE("parse_authorization_rules: empty array", "[security][rules]") {
    auto rules = parse_authorization_rules(json::array());
    REQUIRE(rules.empty());
}

TEST_CASE("parse_authorization_rules: non-array returns empty", "[security][rules]") {
    auto rules = parse_authorization_rules(json::object());
    REQUIRE(rules.empty());
}

// ========================================================================
// Approval Queue Tests
// ========================================================================

TEST_CASE("ApprovalDB: open creates database", "[security][approval]") {
    auto tmp = fs::temp_directory_path() / ("approval_test_" + generate_guid());
    auto result = ApprovalDB::open(tmp.string());
    REQUIRE(result.is_ok());

    auto& db = result.unwrap();
    REQUIRE(fs::exists(db.db_path()));

    fs::remove(db.db_path());
}

TEST_CASE("ApprovalDB: create and list requests", "[security][approval]") {
    auto tmp = fs::temp_directory_path() / ("approval_test_" + generate_guid());
    auto result = ApprovalDB::open(tmp.string());
    REQUIRE(result.is_ok());
    auto& db = result.unwrap();

    auto r1 = db.create_request("agent1", "gnucash_post_transaction",
                                 {{"amount", 500}}, "user@test.com", "Amount exceeds limit");
    REQUIRE(r1.is_ok());
    auto id1 = r1.unwrap();

    auto r2 = db.create_request("agent2", "gnucash_create_account",
                                 {{"name", "Test"}}, "user@test.com", "Requires approval");
    REQUIRE(r2.is_ok());

    auto pending = db.pending_requests();
    REQUIRE(pending.is_ok());
    REQUIRE(pending.unwrap().size() == 2);

    // First request (ASC order)
    REQUIRE(pending.unwrap()[0].id == id1);
    REQUIRE(pending.unwrap()[0].agent_name == "agent1");
    REQUIRE(pending.unwrap()[0].status == "pending");

    fs::remove(db.db_path());
}

TEST_CASE("ApprovalDB: approve request", "[security][approval]") {
    auto tmp = fs::temp_directory_path() / ("approval_test_" + generate_guid());
    auto result = ApprovalDB::open(tmp.string());
    REQUIRE(result.is_ok());
    auto& db = result.unwrap();

    auto r = db.create_request("agent1", "tool", {}, "user", "reason");
    REQUIRE(r.is_ok());
    auto id = r.unwrap();

    auto approve = db.approve(id, "admin@test.com");
    REQUIRE(approve.is_ok());

    // Should no longer be pending
    auto pending = db.pending_requests();
    REQUIRE(pending.is_ok());
    REQUIRE(pending.unwrap().empty());

    // But should still exist as approved
    auto req = db.get_request(id);
    REQUIRE(req.is_ok());
    REQUIRE(req.unwrap().has_value());
    REQUIRE(req.unwrap()->status == "approved");
    REQUIRE(req.unwrap()->approver == "admin@test.com");

    fs::remove(db.db_path());
}

TEST_CASE("ApprovalDB: reject request", "[security][approval]") {
    auto tmp = fs::temp_directory_path() / ("approval_test_" + generate_guid());
    auto result = ApprovalDB::open(tmp.string());
    REQUIRE(result.is_ok());
    auto& db = result.unwrap();

    auto r = db.create_request("agent1", "tool", {}, "user", "reason");
    REQUIRE(r.is_ok());
    auto id = r.unwrap();

    auto reject = db.reject(id, "admin@test.com", "Not authorized");
    REQUIRE(reject.is_ok());

    auto req = db.get_request(id);
    REQUIRE(req.is_ok());
    REQUIRE(req.unwrap()->status == "rejected");
    REQUIRE(req.unwrap()->rejection_reason == "Not authorized");

    fs::remove(db.db_path());
}

TEST_CASE("ApprovalDB: approve nonexistent fails", "[security][approval]") {
    auto tmp = fs::temp_directory_path() / ("approval_test_" + generate_guid());
    auto result = ApprovalDB::open(tmp.string());
    REQUIRE(result.is_ok());
    auto& db = result.unwrap();

    auto approve = db.approve("nonexistent_id", "admin");
    REQUIRE(approve.is_err());

    fs::remove(db.db_path());
}

TEST_CASE("ApprovalDB: limit parameter", "[security][approval]") {
    auto tmp = fs::temp_directory_path() / ("approval_test_" + generate_guid());
    auto result = ApprovalDB::open(tmp.string());
    REQUIRE(result.is_ok());
    auto& db = result.unwrap();

    for (int i = 0; i < 10; i++) {
        db.create_request("agent", "tool", {}, "user", "reason");
    }

    auto limited = db.pending_requests(3);
    REQUIRE(limited.is_ok());
    REQUIRE(limited.unwrap().size() == 3);

    fs::remove(db.db_path());
}
