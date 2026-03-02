#include "gnucash/agent.h"
#include "gnucash/audit.h"
#include "gnucash/types.h"
#include <algorithm>
#include <cmath>
#include <numeric>
#include <sstream>
#include <map>

namespace gnucash {
namespace agent {

// ========================================================================
// Vendor Pattern Matching (shared)
// ========================================================================

static std::string to_upper(const std::string& s) {
    std::string result = s;
    std::transform(result.begin(), result.end(), result.begin(), ::toupper);
    return result;
}

std::vector<MatchResult> match_vendors(
    const std::vector<Transaction>& transactions,
    const std::vector<VendorPattern>& patterns) {

    std::vector<MatchResult> results;

    for (const auto& txn : transactions) {
        std::string desc_upper = to_upper(txn.description);

        for (const auto& pattern : patterns) {
            std::string pat_upper = to_upper(pattern.pattern);
            if (desc_upper.find(pat_upper) != std::string::npos) {
                results.push_back({
                    txn.guid,
                    txn.description,
                    pattern.pattern,
                    pattern.category,
                    pattern.confidence
                });
                break;  // First match wins
            }
        }
    }

    return results;
}

std::vector<VendorPattern> parse_vendor_patterns(const json& config_json) {
    std::vector<VendorPattern> patterns;
    if (!config_json.contains("vendor_patterns")) return patterns;

    for (const auto& p : config_json["vendor_patterns"]) {
        VendorPattern vp;
        vp.pattern = p.value("pattern", "");
        vp.category = p.value("category", "");
        vp.confidence = p.value("confidence", 0.95);
        if (!vp.pattern.empty()) {
            patterns.push_back(std::move(vp));
        }
    }
    return patterns;
}

// ========================================================================
// spend-monitor Implementation
// ========================================================================

Result<AgentResult> run_spend_monitor(Book& book, const dhall::AgentConfig& config,
                                       AgentStateDB& state) {
    // Get last check timestamp from state
    auto last_check_result = state.get("last_check_timestamp");
    std::string since;
    if (last_check_result.is_ok() && last_check_result.unwrap().has_value()) {
        since = last_check_result.unwrap().value();
    }

    // Get all transactions (optionally filtered by date)
    auto transactions = since.empty() ?
        book.get_transactions() :
        book.get_transactions(since);

    // Get account tree for path resolution (account_tree populates full_path)
    auto accounts = book.account_tree();
    std::map<std::string, std::string> guid_to_path;
    for (const auto& acct : accounts) {
        guid_to_path[acct.guid] = acct.full_path;
    }

    // Parse vendor patterns from config
    // For now, use hardcoded patterns from config.tools context
    // In production, these come from the Dhall config JSON
    std::vector<VendorPattern> patterns;
    // We'll use the patterns from the config's vendor_patterns field
    // parsed by the caller; for now build from description matching
    // TODO: Wire full Dhall JSON parsing for vendor_patterns

    // Match vendors
    auto matches = match_vendors(transactions, patterns);

    // Compute category totals from all splits
    std::map<std::string, double> category_totals;
    std::map<std::string, double> vendor_totals;
    int categorized = 0;
    int uncategorized = 0;

    for (const auto& txn : transactions) {
        // Check if this transaction matched a vendor pattern
        bool matched = false;
        for (const auto& m : matches) {
            if (m.transaction_guid == txn.guid) {
                double amount = txn.splits.empty() ? 0.0 :
                    std::abs(txn.splits[0].value.to_double());
                vendor_totals[m.matched_pattern] += amount;
                category_totals[m.category] += amount;
                categorized++;
                matched = true;
                break;
            }
        }

        if (!matched) {
            // Tally by account path
            for (const auto& split : txn.splits) {
                auto it = guid_to_path.find(split.account_guid);
                if (it != guid_to_path.end()) {
                    std::string path = it->second;
                    // Only count expense accounts
                    if (path.find("Expense") != std::string::npos ||
                        path.find("expense") != std::string::npos) {
                        double amount = std::abs(split.value.to_double());
                        category_totals[path] += amount;
                    }
                }
            }
            uncategorized++;
        }
    }

    // Simple anomaly detection: flag categories > 2 stddev from mean
    json anomalies = json::array();
    if (category_totals.size() > 2) {
        std::vector<double> values;
        for (const auto& [cat, total] : category_totals) {
            values.push_back(total);
        }
        double sum = std::accumulate(values.begin(), values.end(), 0.0);
        double mean = sum / values.size();
        double sq_sum = std::inner_product(values.begin(), values.end(),
                                            values.begin(), 0.0);
        double stddev = std::sqrt(sq_sum / values.size() - mean * mean);

        if (stddev > 0) {
            for (const auto& [cat, total] : category_totals) {
                double factor = (total - mean) / stddev;
                if (factor > 2.0) {
                    anomalies.push_back({
                        {"category", cat},
                        {"amount", std::round(total * 100) / 100},
                        {"mean", std::round(mean * 100) / 100},
                        {"stddev", std::round(stddev * 100) / 100},
                        {"factor", std::round(factor * 100) / 100}
                    });
                }
            }
        }
    }

    // Build report
    SpendReport report;
    report.transactions_processed = static_cast<int>(transactions.size());
    report.categorized_count = categorized;
    report.uncategorized_count = uncategorized;
    report.category_totals = json(category_totals);
    report.vendor_totals = json(vendor_totals);
    report.anomalies = anomalies;

    // Update state
    state.set("last_check_timestamp", audit::now_iso8601());
    state.set("last_report", report.category_totals.dump());

    AgentResult result;
    result.agent_name = "spend-monitor";
    result.run_timestamp = audit::now_iso8601();
    result.records_processed = report.transactions_processed;
    result.actions_taken = 0;  // Read-only agent
    result.report = {
        {"period", report.period},
        {"transactions_processed", report.transactions_processed},
        {"categorized", report.categorized_count},
        {"uncategorized", report.uncategorized_count},
        {"category_totals", report.category_totals},
        {"vendor_totals", report.vendor_totals},
        {"anomalies", report.anomalies}
    };

    return Result<AgentResult>::ok(std::move(result));
}

// ========================================================================
// report-generator Implementation
// ========================================================================

Result<AgentResult> run_report_generator(Book& book, const dhall::AgentConfig& config,
                                          AgentStateDB& state) {
    auto accounts = book.account_tree();  // account_tree populates full_path

    // Build trial balance
    json trial_balance = json::array();
    double total_debit = 0;
    double total_credit = 0;

    // Income statement accumulators
    double total_income = 0;
    double total_expenses = 0;

    // Balance sheet accumulators
    double total_assets = 0;
    double total_liabilities = 0;
    double total_equity = 0;

    for (const auto& acct : accounts) {
        if (acct.name == "Root Account" || acct.name.empty()) continue;

        double amount = book.get_account_balance(acct.guid);
        if (std::abs(amount) < 0.005) continue;  // Skip zero balances

        // Trial balance entry
        double debit = amount > 0 ? amount : 0;
        double credit = amount < 0 ? -amount : 0;

        // Account type determines sign convention
        // Assets and Expenses have normal debit balance
        // Liabilities, Equity, Income have normal credit balance
        AccountType atype = acct.type;
        bool debit_normal = (atype == AccountType::ASSET || atype == AccountType::EXPENSE ||
                              atype == AccountType::BANK || atype == AccountType::CASH ||
                              atype == AccountType::RECEIVABLE || atype == AccountType::MUTUAL ||
                              atype == AccountType::STOCK);

        if (debit_normal) {
            debit = std::abs(amount);
            credit = 0;
        } else {
            credit = std::abs(amount);
            debit = 0;
        }

        total_debit += debit;
        total_credit += credit;

        trial_balance.push_back({
            {"account", acct.full_path.empty() ? acct.name : acct.full_path},
            {"type", account_type_to_string(acct.type)},
            {"debit", std::round(debit * 100) / 100},
            {"credit", std::round(credit * 100) / 100}
        });

        // Classify for financial statements
        if (atype == AccountType::INCOME) {
            total_income += std::abs(amount);
        } else if (atype == AccountType::EXPENSE) {
            total_expenses += std::abs(amount);
        } else if (atype == AccountType::ASSET || atype == AccountType::BANK ||
                   atype == AccountType::CASH || atype == AccountType::RECEIVABLE ||
                   atype == AccountType::MUTUAL || atype == AccountType::STOCK) {
            total_assets += std::abs(amount);
        } else if (atype == AccountType::LIABILITY || atype == AccountType::CREDIT ||
                   atype == AccountType::PAYABLE) {
            total_liabilities += std::abs(amount);
        } else if (atype == AccountType::EQUITY) {
            total_equity += std::abs(amount);
        }
    }

    double net_income = total_income - total_expenses;
    std::string now = audit::now_iso8601();

    // Build financial report
    json income_statement = {
        {"total_income", std::round(total_income * 100) / 100},
        {"total_expenses", std::round(total_expenses * 100) / 100},
        {"net_income", std::round(net_income * 100) / 100}
    };

    json balance_sheet = {
        {"total_assets", std::round(total_assets * 100) / 100},
        {"total_liabilities", std::round(total_liabilities * 100) / 100},
        {"total_equity", std::round(total_equity * 100) / 100},
        {"retained_earnings", std::round(net_income * 100) / 100},
        {"balanced", std::abs(total_assets -
            (total_liabilities + total_equity + net_income)) < 0.01}
    };

    // Store report in state
    state.set("last_report_date", now);

    AgentResult result;
    result.agent_name = "report-generator";
    result.run_timestamp = now;
    result.records_processed = static_cast<int>(accounts.size());
    result.actions_taken = 0;  // Read-only
    result.report = {
        {"generated_at", now},
        {"trial_balance", trial_balance},
        {"trial_balance_check", {
            {"total_debit", std::round(total_debit * 100) / 100},
            {"total_credit", std::round(total_credit * 100) / 100},
            {"balanced", std::abs(total_debit - total_credit) < 0.01}
        }},
        {"income_statement", income_statement},
        {"balance_sheet", balance_sheet}
    };

    return Result<AgentResult>::ok(std::move(result));
}

// ========================================================================
// transaction-categorizer Implementation
// ========================================================================

Result<AgentResult> run_categorizer(Book& book, const dhall::AgentConfig& config,
                                     AgentStateDB& state) {
    // Get all transactions
    auto transactions = book.get_transactions();
    auto accounts = book.account_tree();  // account_tree populates full_path

    // Build account lookup
    std::map<std::string, Account> guid_to_account;
    for (const auto& acct : accounts) {
        guid_to_account[acct.guid] = acct;
    }

    // Find uncategorized transactions (splits posting to Imbalance accounts)
    std::vector<Transaction> uncategorized;
    for (const auto& txn : transactions) {
        for (const auto& split : txn.splits) {
            auto it = guid_to_account.find(split.account_guid);
            if (it != guid_to_account.end()) {
                std::string name_upper = to_upper(it->second.name);
                if (name_upper.find("IMBALANCE") != std::string::npos) {
                    uncategorized.push_back(txn);
                    break;
                }
            }
        }
    }

    // Check which ones we've already processed
    int already_processed = 0;
    std::vector<Transaction> to_process;
    for (const auto& txn : uncategorized) {
        auto processed = state.get("processed:" + txn.guid);
        if (processed.is_ok() && processed.unwrap().has_value()) {
            already_processed++;
        } else {
            to_process.push_back(txn);
        }
    }

    // Parse vendor patterns (empty for now, passed via config context)
    std::vector<VendorPattern> patterns;
    // In full implementation, patterns loaded from Dhall config JSON

    // Match vendors against uncategorized transactions
    auto matches = match_vendors(to_process, patterns);

    int auto_categorized = 0;
    int queued_for_review = 0;
    json categorizations = json::array();

    for (const auto& match : matches) {
        if (match.confidence >= 0.90) {
            // High confidence: auto-categorize
            // In full implementation, would call post_transaction to update splits
            auto_categorized++;
            categorizations.push_back({
                {"guid", match.transaction_guid},
                {"description", match.description},
                {"category", match.category},
                {"confidence", match.confidence},
                {"action", "auto_applied"},
                {"pattern", match.matched_pattern}
            });

            // Mark as processed
            state.set("processed:" + match.transaction_guid, match.category);
        } else {
            // Low confidence: queue for review
            state.enqueue_review(match.transaction_guid, match.category,
                                  match.confidence, "Vendor match: " + match.matched_pattern);
            queued_for_review++;
            categorizations.push_back({
                {"guid", match.transaction_guid},
                {"description", match.description},
                {"category", match.category},
                {"confidence", match.confidence},
                {"action", "queued_for_review"},
                {"pattern", match.matched_pattern}
            });
        }
    }

    // Transactions with no pattern match at all
    int unmatched = static_cast<int>(to_process.size()) - static_cast<int>(matches.size());
    for (const auto& txn : to_process) {
        bool found = false;
        for (const auto& m : matches) {
            if (m.transaction_guid == txn.guid) { found = true; break; }
        }
        if (!found) {
            // No pattern match -- queue for manual review with zero confidence
            state.enqueue_review(txn.guid, "", 0.0,
                                  "No vendor pattern match: " + txn.description);
            queued_for_review++;
        }
    }

    AgentResult result;
    result.agent_name = "transaction-categorizer";
    result.run_timestamp = audit::now_iso8601();
    result.records_processed = static_cast<int>(to_process.size());
    result.actions_taken = auto_categorized;
    result.report = {
        {"total_uncategorized", static_cast<int>(uncategorized.size())},
        {"already_processed", already_processed},
        {"newly_processed", static_cast<int>(to_process.size())},
        {"auto_categorized", auto_categorized},
        {"queued_for_review", queued_for_review},
        {"unmatched", unmatched},
        {"categorizations", categorizations}
    };

    return Result<AgentResult>::ok(std::move(result));
}

// ========================================================================
// Agent Dispatch
// ========================================================================

Result<AgentResult> run_agent(const std::string& agent_name,
                               Book& book,
                               const dhall::AgentConfig& config,
                               AgentStateDB& state) {
    if (agent_name == "spend-monitor") {
        return run_spend_monitor(book, config, state);
    } else if (agent_name == "report-generator") {
        return run_report_generator(book, config, state);
    } else if (agent_name == "transaction-categorizer") {
        return run_categorizer(book, config, state);
    } else {
        return Result<AgentResult>::err("Unknown agent: " + agent_name);
    }
}

} // namespace agent
} // namespace gnucash
