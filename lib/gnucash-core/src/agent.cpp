#include "gnucash/agent.h"
#include "gnucash/audit.h"
#include "gnucash/types.h"
#include "gnucash/slots.h"
#include "gnucash/bank_feed.h"
#include "gnucash/reconcile.h"
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
// invoice-generator Implementation
// ========================================================================

Result<AgentResult> run_invoice_generator(Book& book, const dhall::AgentConfig& config,
                                           AgentStateDB& state) {
    auto accounts = book.account_tree();

    // Find RECEIVABLE accounts
    std::vector<Account> receivable_accounts;
    std::map<std::string, std::string> guid_to_path;
    for (const auto& acct : accounts) {
        guid_to_path[acct.guid] = acct.full_path;
        if (acct.type == AccountType::RECEIVABLE) {
            receivable_accounts.push_back(acct);
        }
    }

    json invoices = json::array();
    double total_outstanding = 0;
    int overdue_count = 0;
    std::string now = audit::now_iso8601();
    std::string today = now.substr(0, 10);  // YYYY-MM-DD

    for (const auto& acct : receivable_accounts) {
        double balance = book.get_account_balance(acct.guid);
        if (std::abs(balance) < 0.005) continue;

        // Get splits for this receivable account
        auto splits = book.get_splits_for_account(acct.guid);

        json items = json::array();
        std::string last_date;
        for (const auto& split : splits) {
            double amount = split.value.to_double();
            if (std::abs(amount) < 0.005) continue;

            // Find the transaction for this split
            auto txn_opt = book.get_transaction(split.tx_guid);
            std::string desc = txn_opt.has_value() ? txn_opt->description : "";
            std::string date = txn_opt.has_value() ? txn_opt->post_date : "";

            items.push_back({
                {"description", desc},
                {"amount", std::round(amount * 100) / 100},
                {"date", date}
            });

            if (date > last_date) last_date = date;
        }

        // Consider overdue if last activity > 30 days ago
        bool overdue = false;
        if (!last_date.empty() && last_date.substr(0, 10) < today) {
            // Simple 30-day overdue check
            overdue = true;  // Simplified: any past-date receivable with balance
            overdue_count++;
        }

        total_outstanding += std::abs(balance);

        invoices.push_back({
            {"customer", acct.name},
            {"account_path", acct.full_path},
            {"items", items},
            {"subtotal", std::round(std::abs(balance) * 100) / 100},
            {"balance", std::round(balance * 100) / 100},
            {"overdue", overdue},
            {"last_activity", last_date}
        });
    }

    // Store report in state
    state.set("last_invoice_run", now);
    state.set("last_outstanding_total",
              std::to_string(static_cast<int64_t>(total_outstanding * 100)));

    AgentResult result;
    result.agent_name = "invoice-generator";
    result.run_timestamp = now;
    result.records_processed = static_cast<int>(receivable_accounts.size());
    result.actions_taken = 0;  // REVIEW tier - queued, not executed
    result.report = {
        {"invoices", invoices},
        {"total_outstanding", std::round(total_outstanding * 100) / 100},
        {"overdue_count", overdue_count},
        {"invoice_count", static_cast<int>(invoices.size())},
        {"generated_at", now}
    };

    return Result<AgentResult>::ok(std::move(result));
}

// ========================================================================
// tax-estimator Implementation
// ========================================================================

// Simple progressive tax bracket estimator (US federal approximation 2026)
static double estimate_federal_tax(double taxable_income) {
    struct Bracket { double upper; double rate; };
    // Simplified US brackets (single filer approximation)
    static const Bracket brackets[] = {
        {11600,  0.10},
        {47150,  0.12},
        {100525, 0.22},
        {191950, 0.24},
        {243725, 0.32},
        {609350, 0.35},
        {1e18,   0.37}
    };

    double tax = 0;
    double prev_upper = 0;
    for (const auto& b : brackets) {
        if (taxable_income <= prev_upper) break;
        double bracket_income = std::min(taxable_income, b.upper) - prev_upper;
        if (bracket_income > 0) {
            tax += bracket_income * b.rate;
        }
        prev_upper = b.upper;
    }
    return tax;
}

Result<AgentResult> run_tax_estimator(Book& book, const dhall::AgentConfig& config,
                                       AgentStateDB& state) {
    auto accounts = book.account_tree();

    double gross_income = 0;
    double deductions = 0;  // Track deductible expenses

    // Categorize income and expenses
    json income_breakdown = json::object();
    json deduction_breakdown = json::object();

    for (const auto& acct : accounts) {
        if (acct.name == "Root Account" || acct.name.empty()) continue;

        double balance = book.get_account_balance(acct.guid);
        if (std::abs(balance) < 0.005) continue;

        double amount = std::abs(balance);

        if (acct.type == AccountType::INCOME) {
            gross_income += amount;
            std::string category = acct.full_path.empty() ? acct.name : acct.full_path;
            income_breakdown[category] = std::round(amount * 100) / 100;
        } else if (acct.type == AccountType::EXPENSE) {
            // Common deductible expense categories
            std::string path_upper = to_upper(acct.full_path);
            bool deductible = (path_upper.find("BUSINESS") != std::string::npos ||
                               path_upper.find("OFFICE") != std::string::npos ||
                               path_upper.find("PROFESSIONAL") != std::string::npos ||
                               path_upper.find("INSURANCE") != std::string::npos ||
                               path_upper.find("MEDICAL") != std::string::npos ||
                               path_upper.find("CHARITY") != std::string::npos ||
                               path_upper.find("EDUCATION") != std::string::npos ||
                               path_upper.find("SAAS") != std::string::npos ||
                               path_upper.find("CLOUD") != std::string::npos);
            if (deductible) {
                deductions += amount;
                std::string category = acct.full_path.empty() ? acct.name : acct.full_path;
                deduction_breakdown[category] = std::round(amount * 100) / 100;
            }
        }
    }

    // Standard deduction floor (2026 estimate)
    const double standard_deduction = 15700.0;
    double total_deductions = std::max(deductions, standard_deduction);
    bool itemizing = deductions > standard_deduction;

    double taxable_income = std::max(0.0, gross_income - total_deductions);
    double estimated_tax = estimate_federal_tax(taxable_income);
    double quarterly_payment = estimated_tax / 4.0;
    double effective_rate = gross_income > 0 ? estimated_tax / gross_income : 0;

    // Compare with previous estimate
    auto prev_estimate = state.get("last_estimated_tax");
    double prev_tax = 0;
    if (prev_estimate.is_ok() && prev_estimate.unwrap().has_value()) {
        try { prev_tax = std::stod(prev_estimate.unwrap().value()); }
        catch (...) { prev_tax = 0; }
    }
    double change = estimated_tax - prev_tax;

    std::string now = audit::now_iso8601();

    // Store state
    state.set("last_estimated_tax", std::to_string(estimated_tax));
    state.set("last_tax_run", now);

    AgentResult result;
    result.agent_name = "tax-estimator";
    result.run_timestamp = now;
    result.records_processed = static_cast<int>(accounts.size());
    result.actions_taken = 0;  // Read-only
    result.report = {
        {"gross_income", std::round(gross_income * 100) / 100},
        {"deductions", std::round(total_deductions * 100) / 100},
        {"itemizing", itemizing},
        {"taxable_income", std::round(taxable_income * 100) / 100},
        {"estimated_tax", std::round(estimated_tax * 100) / 100},
        {"quarterly_payment", std::round(quarterly_payment * 100) / 100},
        {"effective_rate", std::round(effective_rate * 10000) / 10000},
        {"income_breakdown", income_breakdown},
        {"deduction_breakdown", deduction_breakdown},
        {"change_from_last", std::round(change * 100) / 100},
        {"generated_at", now}
    };

    return Result<AgentResult>::ok(std::move(result));
}

// ========================================================================
// subscription-manager Implementation
// ========================================================================

// Normalize vendor name for grouping
static std::string normalize_vendor(const std::string& description) {
    std::string upper = to_upper(description);
    // Strip common suffixes: #NNN, PAYMENT, *NNN, etc.
    auto pos = upper.find('#');
    if (pos != std::string::npos) upper = upper.substr(0, pos);
    pos = upper.find('*');
    if (pos != std::string::npos) upper = upper.substr(0, pos);

    // Trim trailing spaces
    while (!upper.empty() && upper.back() == ' ') upper.pop_back();
    return upper;
}

// Detect frequency from timestamps
static std::string detect_frequency(const std::vector<std::string>& dates) {
    if (dates.size() < 2) return "one-time";

    // Parse dates and compute intervals in days
    std::vector<int> intervals;
    for (size_t i = 1; i < dates.size(); i++) {
        // Simple date subtraction using YYYY-MM-DD format
        // Parse year, month from dates
        int y1 = std::stoi(dates[i-1].substr(0, 4));
        int m1 = std::stoi(dates[i-1].substr(5, 2));
        int d1 = std::stoi(dates[i-1].substr(8, 2));
        int y2 = std::stoi(dates[i].substr(0, 4));
        int m2 = std::stoi(dates[i].substr(5, 2));
        int d2 = std::stoi(dates[i].substr(8, 2));

        // Approximate days between dates
        int days = (y2 - y1) * 365 + (m2 - m1) * 30 + (d2 - d1);
        if (days > 0) intervals.push_back(days);
    }

    if (intervals.empty()) return "irregular";

    double avg = std::accumulate(intervals.begin(), intervals.end(), 0.0) / intervals.size();

    // Classify by average interval
    if (avg <= 10) return "weekly";
    if (avg >= 25 && avg <= 35) return "monthly";
    if (avg >= 80 && avg <= 100) return "quarterly";
    if (avg >= 340 && avg <= 400) return "annual";
    return "irregular";
}

Result<AgentResult> run_subscription_manager(Book& book, const dhall::AgentConfig& config,
                                              AgentStateDB& state) {
    // Get all transactions
    auto transactions = book.get_transactions();
    auto accounts = book.account_tree();

    std::map<std::string, std::string> guid_to_path;
    for (const auto& acct : accounts) {
        guid_to_path[acct.guid] = acct.full_path;
    }

    // Group transactions by normalized vendor
    struct VendorGroup {
        std::string original_name;          // First occurrence name
        std::vector<double> amounts;
        std::vector<std::string> dates;     // Sorted
        std::string account_path;           // Most common expense account
    };

    std::map<std::string, VendorGroup> vendor_groups;

    for (const auto& txn : transactions) {
        if (txn.description.empty()) continue;

        std::string vendor = normalize_vendor(txn.description);
        if (vendor.empty()) continue;

        // Find the expense split amount
        double amount = 0;
        std::string expense_path;
        for (const auto& split : txn.splits) {
            double val = split.value.to_double();
            if (val > 0) {
                amount = val;
                auto it = guid_to_path.find(split.account_guid);
                if (it != guid_to_path.end()) expense_path = it->second;
            }
        }
        if (amount < 0.005) continue;

        auto& group = vendor_groups[vendor];
        if (group.original_name.empty()) group.original_name = txn.description;
        group.amounts.push_back(amount);
        group.dates.push_back(txn.post_date.substr(0, 10));
        if (!expense_path.empty()) group.account_path = expense_path;
    }

    // Sort dates within each group
    for (auto& [vendor, group] : vendor_groups) {
        std::sort(group.dates.begin(), group.dates.end());
    }

    // Identify subscriptions: recurring (>= 2 occurrences with regular interval)
    json subscriptions = json::array();
    double monthly_total = 0;
    double annual_total = 0;

    for (const auto& [vendor, group] : vendor_groups) {
        if (group.amounts.size() < 2) continue;

        std::string freq = detect_frequency(group.dates);
        if (freq == "one-time" || freq == "irregular") continue;

        // Calculate average amount
        double sum = std::accumulate(group.amounts.begin(), group.amounts.end(), 0.0);
        double avg_amount = sum / group.amounts.size();

        // Check amount consistency (within 20% of average)
        bool consistent = true;
        for (double a : group.amounts) {
            if (std::abs(a - avg_amount) / avg_amount > 0.20) {
                consistent = false;
                break;
            }
        }
        if (!consistent) continue;

        avg_amount = std::round(avg_amount * 100) / 100;

        // Calculate monthly equivalent
        double monthly = avg_amount;
        if (freq == "weekly") monthly = avg_amount * 4.33;
        else if (freq == "quarterly") monthly = avg_amount / 3.0;
        else if (freq == "annual") monthly = avg_amount / 12.0;

        monthly_total += monthly;
        annual_total += monthly * 12;

        subscriptions.push_back({
            {"vendor", vendor},
            {"display_name", group.original_name},
            {"amount", avg_amount},
            {"frequency", freq},
            {"occurrence_count", static_cast<int>(group.amounts.size())},
            {"first_seen", group.dates.front()},
            {"last_seen", group.dates.back()},
            {"monthly_equivalent", std::round(monthly * 100) / 100},
            {"account", group.account_path}
        });
    }

    // Compare with previous run for new/cancelled/changed subscriptions
    json new_subs = json::array();
    json cancelled = json::array();
    json price_changes = json::array();

    auto prev_subs_str = state.get("subscriptions");
    if (prev_subs_str.is_ok() && prev_subs_str.unwrap().has_value()) {
        try {
            json prev = json::parse(prev_subs_str.unwrap().value());
            std::map<std::string, double> prev_map;
            for (const auto& s : prev) {
                prev_map[s.value("vendor", "")] = s.value("amount", 0.0);
            }

            // Detect new
            for (const auto& s : subscriptions) {
                std::string v = s.value("vendor", "");
                if (prev_map.find(v) == prev_map.end()) {
                    new_subs.push_back({{"vendor", v}, {"amount", s["amount"]}});
                } else {
                    double prev_amt = prev_map[v];
                    double curr_amt = s.value("amount", 0.0);
                    if (std::abs(curr_amt - prev_amt) / prev_amt > 0.05) {
                        price_changes.push_back({
                            {"vendor", v},
                            {"old_amount", prev_amt},
                            {"new_amount", curr_amt}
                        });
                    }
                }
            }

            // Detect cancelled (in prev but not current)
            std::map<std::string, double> curr_map;
            for (const auto& s : subscriptions) {
                curr_map[s.value("vendor", "")] = s.value("amount", 0.0);
            }
            for (const auto& [v, amt] : prev_map) {
                if (curr_map.find(v) == curr_map.end()) {
                    cancelled.push_back({{"vendor", v}, {"last_amount", amt}});
                }
            }
        } catch (...) {
            // Previous state corrupt, treat as first run
        }
    }

    std::string now = audit::now_iso8601();

    // Store state
    state.set("subscriptions", subscriptions.dump());
    state.set("last_subscription_run", now);

    AgentResult result;
    result.agent_name = "subscription-manager";
    result.run_timestamp = now;
    result.records_processed = static_cast<int>(transactions.size());
    result.actions_taken = 0;  // Read-only
    result.report = {
        {"subscriptions", subscriptions},
        {"subscription_count", static_cast<int>(subscriptions.size())},
        {"monthly_total", std::round(monthly_total * 100) / 100},
        {"annual_total", std::round(annual_total * 100) / 100},
        {"new_subscriptions", new_subs},
        {"cancelled", cancelled},
        {"price_changes", price_changes},
        {"generated_at", now}
    };

    return Result<AgentResult>::ok(std::move(result));
}

// ========================================================================
// bill-pay Implementation (stub with approval flow)
// ========================================================================

Result<AgentResult> run_bill_pay(Book& book, const dhall::AgentConfig& config,
                                  AgentStateDB& state) {
    auto accounts = book.account_tree();

    // Find PAYABLE and LIABILITY accounts with balances
    std::map<std::string, std::string> guid_to_path;
    json bills_due = json::array();
    double total_due = 0;

    for (const auto& acct : accounts) {
        guid_to_path[acct.guid] = acct.full_path;

        if (acct.type == AccountType::PAYABLE ||
            acct.type == AccountType::LIABILITY ||
            acct.type == AccountType::CREDIT) {

            double balance = book.get_account_balance(acct.guid);
            if (std::abs(balance) < 0.005) continue;

            double amount = std::abs(balance);
            total_due += amount;

            // Get last transaction date as proxy for due date
            auto splits = book.get_splits_for_account(acct.guid);
            std::string last_date;
            for (const auto& split : splits) {
                auto txn = book.get_transaction(split.tx_guid);
                if (txn.has_value() && txn->post_date > last_date) {
                    last_date = txn->post_date;
                }
            }

            bills_due.push_back({
                {"payee", acct.name},
                {"account_path", acct.full_path},
                {"amount", std::round(amount * 100) / 100},
                {"last_activity", last_date},
                {"account_guid", acct.guid}
            });
        }
    }

    // Check for previously submitted approval requests
    // In production, this would query ApprovalDB
    // For now, track in agent state
    json pending_approval = json::array();
    json executed = json::array();
    double total_executed = 0;

    // Check state for any pending bill payment approvals
    auto pending_str = state.get("pending_payments");
    if (pending_str.is_ok() && pending_str.unwrap().has_value()) {
        try {
            json pending = json::parse(pending_str.unwrap().value());
            for (const auto& p : pending) {
                std::string status = p.value("status", "pending");
                if (status == "pending") {
                    pending_approval.push_back(p);
                } else if (status == "approved") {
                    // In production: execute the payment via post_transaction
                    // For stub: mark as executed
                    json exec = p;
                    exec["status"] = "executed";
                    exec["executed_at"] = audit::now_iso8601();
                    executed.push_back(exec);
                    total_executed += p.value("amount", 0.0);
                }
            }
        } catch (...) {
            // State corrupt, reset
        }
    }

    // Create new payment requests for bills due
    // Each bill gets a pending payment entry (approval flow)
    json new_pending = json::array();
    for (const auto& bill : bills_due) {
        std::string payee = bill.value("payee", "");
        std::string key = "bill_submitted:" + payee;

        auto already = state.get(key);
        if (already.is_ok() && already.unwrap().has_value()) continue;

        json payment_request = {
            {"payee", payee},
            {"amount", bill["amount"]},
            {"account_guid", bill.value("account_guid", "")},
            {"status", "pending"},
            {"requested_at", audit::now_iso8601()}
        };
        new_pending.push_back(payment_request);
        pending_approval.push_back(payment_request);

        state.set(key, "pending");
    }

    // Store updated pending list
    state.set("pending_payments", pending_approval.dump());

    std::string now = audit::now_iso8601();
    state.set("last_bill_pay_run", now);

    AgentResult result;
    result.agent_name = "bill-pay";
    result.run_timestamp = now;
    result.records_processed = static_cast<int>(bills_due.size());
    result.actions_taken = static_cast<int>(executed.size());
    result.report = {
        {"bills_due", bills_due},
        {"bills_due_count", static_cast<int>(bills_due.size())},
        {"pending_approval", pending_approval},
        {"pending_count", static_cast<int>(pending_approval.size())},
        {"executed", executed},
        {"executed_count", static_cast<int>(executed.size())},
        {"total_due", std::round(total_due * 100) / 100},
        {"total_executed", std::round(total_executed * 100) / 100},
        {"generated_at", now}
    };

    return Result<AgentResult>::ok(std::move(result));
}

// ========================================================================
// bank-feed-importer
// ========================================================================

Result<AgentResult> run_bank_feed_importer(Book& book, const dhall::AgentConfig& config,
                                            AgentStateDB& state) {
    AgentResult result;
    result.agent_name = "bank-feed-importer";

    auto accounts = book.account_tree();
    ensure_slots_table(book.raw_db());

    BankFeedImportReport report;
    report.total_imported = 0;
    report.total_duplicates = 0;
    report.total_errors = 0;
    report.accounts_processed = 0;
    report.import_results = json::array();

    // Scan bank/credit accounts for import status
    for (const auto& acct : accounts) {
        if (acct.type != AccountType::BANK &&
            acct.type != AccountType::CREDIT &&
            acct.type != AccountType::CASH)
            continue;

        auto splits = book.get_splits_for_account(acct.guid);
        if (splits.empty()) continue;

        int imported = 0;
        int unreconciled = 0;
        for (const auto& s : splits) {
            auto slot = get_slot(book.raw_db(), s.guid, "online_id");
            if (slot) imported++;
            if (s.reconcile_state == ReconcileState::NOT_RECONCILED)
                unreconciled++;
        }

        report.import_results.push_back({
            {"account_path", acct.full_path},
            {"account_guid", acct.guid},
            {"total_splits", static_cast<int>(splits.size())},
            {"imported_count", imported},
            {"unreconciled_count", unreconciled}
        });

        report.total_imported += imported;
        report.accounts_processed++;
    }

    result.report = {
        {"import_results", report.import_results},
        {"total_imported", report.total_imported},
        {"total_duplicates", report.total_duplicates},
        {"total_errors", report.total_errors},
        {"accounts_processed", report.accounts_processed}
    };
    result.records_processed = report.accounts_processed;

    // Store run state
    state.set("last_run", result.run_timestamp.empty() ? "now" : result.run_timestamp);
    state.set("accounts_processed", std::to_string(report.accounts_processed));

    return Result<AgentResult>::ok(result);
}

// ========================================================================
// reconciler
// ========================================================================

Result<AgentResult> run_reconciler(Book& book, const dhall::AgentConfig& config,
                                    AgentStateDB& state) {
    AgentResult result;
    result.agent_name = "reconciler";

    auto accounts = book.account_tree();
    ReconcilerReport report;
    report.accounts_reconciled = 0;
    report.transfers_matched = 0;
    report.reconciled_accounts = json::array();
    report.transfer_matches = json::array();

    // Find bank/credit accounts to report status
    std::vector<const Account*> bank_accounts;
    for (const auto& acct : accounts) {
        if (acct.type == AccountType::BANK ||
            acct.type == AccountType::CREDIT ||
            acct.type == AccountType::CASH) {
            bank_accounts.push_back(&acct);
        }
    }

    for (const auto* acct : bank_accounts) {
        auto splits = book.get_splits_for_account(acct->guid);
        int unreconciled = 0;
        for (const auto& s : splits) {
            if (s.reconcile_state == ReconcileState::NOT_RECONCILED)
                unreconciled++;
        }

        report.reconciled_accounts.push_back({
            {"account_path", acct->full_path},
            {"total_splits", static_cast<int>(splits.size())},
            {"unreconciled", unreconciled}
        });
    }
    report.accounts_reconciled = static_cast<int>(bank_accounts.size());

    // Find cross-institution matches between bank accounts
    for (size_t i = 0; i < bank_accounts.size() && i < 10; i++) {
        for (size_t j = i + 1; j < bank_accounts.size() && j < 10; j++) {
            auto matches = find_cross_institution_matches(
                book, bank_accounts[i]->guid, bank_accounts[j]->guid,
                "2000-01-01", "2099-12-31", 3, 0.5);

            for (const auto& m : matches) {
                report.transfer_matches.push_back({
                    {"from_account", bank_accounts[i]->full_path},
                    {"to_account", bank_accounts[j]->full_path},
                    {"amount", m.amount.to_double()},
                    {"date_a", m.date_a},
                    {"date_b", m.date_b},
                    {"similarity", m.similarity}
                });
                report.transfers_matched++;
            }
        }
    }

    result.report = {
        {"reconciled_accounts", report.reconciled_accounts},
        {"transfer_matches", report.transfer_matches},
        {"accounts_reconciled", report.accounts_reconciled},
        {"transfers_matched", report.transfers_matched}
    };
    result.records_processed = report.accounts_reconciled;

    state.set("last_run", result.run_timestamp.empty() ? "now" : result.run_timestamp);

    return Result<AgentResult>::ok(result);
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
    } else if (agent_name == "invoice-generator") {
        return run_invoice_generator(book, config, state);
    } else if (agent_name == "tax-estimator") {
        return run_tax_estimator(book, config, state);
    } else if (agent_name == "subscription-manager") {
        return run_subscription_manager(book, config, state);
    } else if (agent_name == "bill-pay") {
        return run_bill_pay(book, config, state);
    } else if (agent_name == "bank-feed-importer") {
        return run_bank_feed_importer(book, config, state);
    } else if (agent_name == "reconciler") {
        return run_reconciler(book, config, state);
    } else {
        return Result<AgentResult>::err("Unknown agent: " + agent_name);
    }
}

} // namespace agent
} // namespace gnucash
