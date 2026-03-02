#pragma once
// Agent Execution Framework
// Routes --run invocations to specific agent implementations
// Each agent reads GnuCash data, performs analysis, produces a report

#include <string>
#include <vector>
#include <nlohmann/json.hpp>
#include "result.h"
#include "book.h"
#include "dhall_config.h"
#include "agent_state.h"

namespace gnucash {
namespace agent {

using json = nlohmann::json;

// ========================================================================
// Agent Result
// ========================================================================

struct AgentResult {
    json report;                    // Agent-specific output
    int records_processed = 0;
    int actions_taken = 0;
    std::string agent_name;
    std::string run_timestamp;
};

// ========================================================================
// Vendor Pattern Matching (shared across agents)
// ========================================================================

struct VendorPattern {
    std::string pattern;            // Substring match (case-insensitive)
    std::string category;           // Target account path
    double confidence;
};

struct MatchResult {
    std::string transaction_guid;
    std::string description;        // Original transaction description
    std::string matched_pattern;
    std::string category;
    double confidence;
};

/// Match transactions against vendor patterns (case-insensitive substring)
std::vector<MatchResult> match_vendors(
    const std::vector<Transaction>& transactions,
    const std::vector<VendorPattern>& patterns);

/// Parse vendor patterns from Dhall agent config JSON
std::vector<VendorPattern> parse_vendor_patterns(const json& config_json);

// ========================================================================
// spend-monitor
// ========================================================================

struct SpendReport {
    std::string period;             // "2026-03"
    json category_totals;           // {"Expenses:SaaS:AI": 150.00, ...}
    json vendor_totals;             // {"ANTHROPIC": 150.00, ...}
    json anomalies;                 // [{category, amount, avg, stddev, factor}]
    int transactions_processed;
    int categorized_count;
    int uncategorized_count;
};

Result<AgentResult> run_spend_monitor(Book& book, const dhall::AgentConfig& config,
                                       AgentStateDB& state);

// ========================================================================
// report-generator
// ========================================================================

struct FinancialReport {
    json trial_balance;             // All accounts with balances
    json income_statement;          // Revenue, Expenses, Net Income
    json balance_sheet;             // Assets, Liabilities, Equity
    std::string period_start;
    std::string period_end;
    std::string generated_at;
};

Result<AgentResult> run_report_generator(Book& book, const dhall::AgentConfig& config,
                                          AgentStateDB& state);

// ========================================================================
// transaction-categorizer
// ========================================================================

struct CategorizationReport {
    int total_uncategorized;
    int auto_categorized;           // High confidence, applied
    int queued_for_review;          // Low confidence, in review queue
    int already_processed;          // Previously seen
    json categorizations;           // [{guid, description, category, confidence}]
};

Result<AgentResult> run_categorizer(Book& book, const dhall::AgentConfig& config,
                                     AgentStateDB& state);

// ========================================================================
// Agent Dispatch
// ========================================================================

/// Run an agent by name -- dispatches to the correct implementation
Result<AgentResult> run_agent(const std::string& agent_name,
                               Book& book,
                               const dhall::AgentConfig& config,
                               AgentStateDB& state);

} // namespace agent
} // namespace gnucash
