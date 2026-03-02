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
// invoice-generator
// ========================================================================

struct InvoiceReport {
    json invoices;                  // [{customer, items, subtotal, tax, total}]
    double total_outstanding;
    int overdue_count;
    int invoice_count;
};

Result<AgentResult> run_invoice_generator(Book& book, const dhall::AgentConfig& config,
                                           AgentStateDB& state);

// ========================================================================
// tax-estimator
// ========================================================================

struct TaxEstimate {
    double gross_income;
    double deductions;
    double taxable_income;
    double estimated_tax;
    double quarterly_payment;
    double effective_rate;
};

Result<AgentResult> run_tax_estimator(Book& book, const dhall::AgentConfig& config,
                                       AgentStateDB& state);

// ========================================================================
// subscription-manager
// ========================================================================

struct Subscription {
    std::string vendor;             // Normalized vendor name
    double amount;                  // Typical amount per occurrence
    std::string frequency;          // "monthly", "weekly", "annual", "irregular"
    int occurrence_count;
    std::string first_seen;
    std::string last_seen;
};

struct SubscriptionReport {
    json subscriptions;             // [{vendor, amount, frequency, ...}]
    double monthly_total;
    double annual_total;
    json new_subscriptions;
    json cancelled;
    json price_changes;
};

Result<AgentResult> run_subscription_manager(Book& book, const dhall::AgentConfig& config,
                                              AgentStateDB& state);

// ========================================================================
// bill-pay (stub with approval flow)
// ========================================================================

struct BillPayReport {
    json bills_due;                 // [{payee, amount, due_date, account}]
    json pending_approval;          // [{approval_id, payee, amount}]
    json executed;                  // [{payee, amount, transaction_guid}]
    double total_due;
    double total_executed;
};

Result<AgentResult> run_bill_pay(Book& book, const dhall::AgentConfig& config,
                                  AgentStateDB& state);

// ========================================================================
// bank-feed-importer
// ========================================================================

struct BankFeedImportReport {
    json import_results;             // [{account, total_parsed, imported, duplicates}]
    int total_imported;
    int total_duplicates;
    int total_errors;
    int accounts_processed;
};

Result<AgentResult> run_bank_feed_importer(Book& book, const dhall::AgentConfig& config,
                                            AgentStateDB& state);

// ========================================================================
// reconciler
// ========================================================================

struct ReconcilerReport {
    json reconciled_accounts;        // [{account, splits_reconciled, balanced, difference}]
    json transfer_matches;           // [{from, to, amount, similarity}]
    int accounts_reconciled;
    int transfers_matched;
};

Result<AgentResult> run_reconciler(Book& book, const dhall::AgentConfig& config,
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
