# Week 6: Agent Development Plan

**Status**: Planning
**Dependencies**: Week 5 complete (MCP server, audit trail, Dhall configs)
**Gate**: G3 -- agents produce useful output, all actions audited

---

## Architecture Decision: What Is an "Agent"?

An agent is **not** a standalone daemon. It is a **Claude Code MCP session** with:

1. A `--agent` flag selecting a Dhall config (tool filtering)
2. C++ business logic in gnucash-bridge that implements agent-specific commands
3. Agent state stored in a per-agent SQLite database alongside the book

This approach keeps everything in one binary (`gnucash-bridge`/`gnucash-mcp`),
avoids building a separate scheduler/daemon, and leverages Claude Code as the
"runtime" for LLM-powered agents (transaction-categorizer).

**Agent execution modes**:
- **Claude Code session**: User talks to agent via MCP tools in Claude Code
- **CLI one-shot**: `gnucash-mcp --agent spend-monitor.dhall --run` produces report
- **Future (Week 7+)**: Schedule executor (cron, systemd timer) calling CLI one-shot

---

## Phase 6.1: Agent Runtime Core (Day 1-2)

### Task 6.1.1: Agent State Database

Per-agent state in `{book_path}.agent.{name}.db`:

```sql
CREATE TABLE agent_state (
    key TEXT PRIMARY KEY,
    value TEXT NOT NULL,
    updated_at TEXT NOT NULL
);

-- spend-monitor: last_check_timestamp, subscription_registry
-- report-generator: last_report_date, report_history
-- transaction-categorizer: processed_transaction_guids, learned_patterns
```

### Task 6.1.2: Agent Execution Framework

Add to gnucash-bridge:
- `--run` flag: execute agent's primary action once and exit
- Agent base class/interface in C++:
  ```cpp
  struct AgentResult {
      json report;           // Agent output (report, categorization, etc.)
      int records_processed;
      int actions_taken;
  };

  // Agent dispatch: load config, run action, log results
  Result<AgentResult> run_agent(const dhall::AgentConfig& config, Book& book);
  ```

### Task 6.1.3: MCP Tools for Agent Operations

New MCP tools (added to registry):
- `gnucash_agent_run` -- Execute agent's primary action
- `gnucash_agent_status` -- Get agent state (last run, records processed)
- `gnucash_agent_report` -- Get last report from an agent

---

## Phase 6.2: spend-monitor Agent (Day 2-3)

### Task 6.2.1: Vendor Pattern Matching

```cpp
struct MatchResult {
    std::string transaction_guid;
    std::string vendor_pattern;    // What matched
    std::string category;          // Target account path
    double confidence;
};

std::vector<MatchResult> match_vendors(
    const std::vector<Transaction>& transactions,
    const std::vector<VendorPattern>& patterns);
```

Case-insensitive substring match on transaction description against
vendor patterns from Dhall config.

### Task 6.2.2: Spend Analysis

```cpp
struct SpendReport {
    std::string period;            // "2026-03"
    json category_totals;          // {"Expenses:SaaS:AI": 150.00, ...}
    json vendor_totals;            // {"ANTHROPIC": 150.00, ...}
    json anomalies;                // Spending >2 stddev from trailing avg
    int transactions_processed;
    int uncategorized_count;
};

SpendReport analyze_spending(Book& book, const AgentConfig& config,
                              const std::string& since);
```

### Task 6.2.3: Anomaly Detection

Simple statistical approach:
- Track 30-day rolling average per category
- Flag anything >2 standard deviations above mean
- Report includes: amount, category, percent above average, last 3 values

---

## Phase 6.3: report-generator Agent (Day 3-4)

### Task 6.3.1: Financial Statement Engine

Build on existing `trial_balance` infrastructure:

```cpp
struct FinancialReport {
    json trial_balance;     // All accounts with balances
    json income_statement;  // Revenue - Expenses = Net Income
    json balance_sheet;     // Assets = Liabilities + Equity
    std::string period;     // "2026-Q1", "2026-03", etc.
    std::string generated_at;
};

FinancialReport generate_report(Book& book, const std::string& period_start,
                                 const std::string& period_end);
```

### Task 6.3.2: Report Formatting

- JSON output (primary): structured data for Claude Code consumption
- Text output: formatted tables for terminal display
- Report stored in agent state DB for retrieval via `gnucash_agent_report`

### Task 6.3.3: Comparative Reports

```cpp
struct ComparativeReport {
    FinancialReport current;
    FinancialReport previous;
    json deltas;            // Per-account change amount and percentage
};
```

---

## Phase 6.4: transaction-categorizer Agent (Day 4-5)

### Task 6.4.1: Deterministic Categorization

1. Query uncategorized transactions (splits posting to Imbalance accounts)
2. Run vendor pattern matching (reuse spend-monitor patterns)
3. High-confidence matches (>=0.90): auto-categorize
4. Low-confidence matches (<0.90): queue for review
5. Unmatched: flag as "needs review"

### Task 6.4.2: Review Queue

```sql
-- In agent state DB
CREATE TABLE review_queue (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    transaction_guid TEXT NOT NULL,
    suggested_category TEXT,
    confidence REAL,
    reason TEXT,
    status TEXT DEFAULT 'pending',  -- pending, approved, rejected
    created_at TEXT NOT NULL
);
```

New MCP tools:
- `gnucash_review_queue` -- List pending reviews
- `gnucash_review_approve` -- Approve a categorization
- `gnucash_review_reject` -- Reject, provide correct category

### Task 6.4.3: Auto-Apply High-Confidence Matches

For matches >=0.90, automatically:
1. Create correcting transaction (move from Imbalance to correct account)
2. Log to audit trail with vendor pattern + confidence as reasoning
3. Store in agent state for tracking

**Note**: LLM-powered categorization (Claude API calls) deferred to Week 7+.
Week 6 focuses on deterministic pattern matching only.

---

## Phase 6.5: Testing Framework (Day 5-6)

### Task 6.5.1: Agent Test Fixtures

Create `lib/gnucash-core/test/fixtures/agent-test.gnucash`:
- 20 accounts (existing with-accounts.gnucash base)
- 50+ transactions with known vendor names
- Mix of categorized and uncategorized
- Known anomaly (one large AWS charge)

### Task 6.5.2: Agent Catch2 Tests

```
test/test_agent_spend_monitor.cpp   -- vendor matching, spend analysis, anomaly detection
test/test_agent_report.cpp          -- financial statements, comparative reports
test/test_agent_categorizer.cpp     -- pattern matching, review queue, auto-apply
test/test_agent_runtime.cpp         -- agent state, execution framework
```

### Task 6.5.3: Integration Test

End-to-end test:
1. Open test fixture book
2. Run spend-monitor -> verify report output
3. Run report-generator -> verify financial statements balance
4. Run transaction-categorizer -> verify categorization + review queue
5. Verify audit trail has entries for all three agents

---

## Phase 6.6: Justfile & Nix Updates (Day 6)

### Task 6.6.1: Agent Justfile Recipes

```just
agent-run agent book:        # Run agent once (CLI one-shot)
agent-test:                  # Run all agent tests
agent-status agent book:     # Show agent state
agent-report agent book:     # Get last agent report
agent-review book:           # Show review queue
```

### Task 6.6.2: Update Nix Derivation

- Ensure gnucashMcp includes new agent runtime code
- Update gnucashCoreTests with new test files
- Verify `nix build .#gnucashMcp` still builds

---

## Gate G3 Criteria

| Criterion | Verification | Priority |
|-----------|-------------|----------|
| MCP server responds to Claude Code queries | `just mcp-test-all` | Must |
| spend-monitor produces report from fixture data | `just agent-run spend-monitor test/fixtures/agent-test.gnucash` | Must |
| report-generator: trial balance sums to zero | Catch2 assertion in test_agent_report.cpp | Must |
| transaction-categorizer categorizes known vendors | Catch2 assertion in test_agent_categorizer.cpp | Must |
| All agent actions in audit trail | Integration test verifies audit entries | Must |
| Agent tests pass (Catch2) | `ctest --output-on-failure` | Must |
| Review queue works | Test approve/reject cycle | Should |
| Anomaly detection flags outlier | Fixture has known outlier | Should |

---

## Known Deferred Items (Week 7+)

- LLM-powered categorization (Claude API integration)
- Schedule executor (cron/systemd timer integration)
- Authorization tier enforcement at runtime
- Multi-agent coordination (triggers between agents)
- Learning from corrections (auto-update Dhall patterns)
- Report output to HTML/PDF (via R gnucashr package)

---

## Estimated Effort

| Phase | Days | Key Deliverables |
|-------|------|-----------------|
| 6.1 Agent Runtime Core | 2 | State DB, execution framework, 3 new MCP tools |
| 6.2 spend-monitor | 1.5 | Vendor matching, spend analysis, anomaly detection |
| 6.3 report-generator | 1.5 | Financial statements, text formatting, comparatives |
| 6.4 transaction-categorizer | 1.5 | Pattern matching, review queue, auto-apply |
| 6.5 Testing Framework | 1.5 | Fixtures, Catch2 tests, integration test |
| 6.6 Justfile & Nix | 0.5 | Recipes, derivation update |
| **Total** | **~8 days** | |
