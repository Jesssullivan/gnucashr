# Phase 3: Agent Framework (Weeks 5-6)

**Goal**: Build the agent framework, MCP server, and deploy the first three agents:
spend-monitor, report-generator, and transaction-categorizer.

---

## Week 5: Agent Framework + MCP Server

### Tasks

- [ ] **5.1 Build MCP server on gnucash-core C++ lib**
  - **Context**: gnucash-mcp (ninetails-io) was forked and evaluated; it is a
    rudimentary Python/piecash implementation that will be superseded by our C++
    backend for performance, correctness, and tighter integration with gnucashr.
    The fork is disassociated from upstream and serves as reference only.
  - Build MCP server as a C++ stdio server using gnucash-core lib:
    ```
    Core tools (from C++ lib):
      gnucash_list_accounts       -- List all accounts with balances
      gnucash_get_transactions    -- Query transactions by date/account
      gnucash_trial_balance       -- Generate trial balance as of date
      gnucash_income_statement    -- P&L for date range
      gnucash_balance_sheet       -- Balance sheet as of date
      gnucash_post_transaction    -- Create new transaction (requires auth)
      gnucash_create_account      -- Create account (requires auth)
      gnucash_import_ofx          -- Import OFX bank statement
      gnucash_search              -- Full-text search across descriptions/memos

    Agent-specific tools:
      gnucash_categorize          -- Assign category to transaction (with LLM)
      gnucash_detect_subscriptions -- Find recurring transactions
      gnucash_spend_summary       -- Categorized spend report
      gnucash_tax_summary         -- Tax-categorized expense report
    ```
  - Auth check: each tool call validates against Dhall-defined capability envelope
  - JSON-RPC 2.0 over stdio (standard MCP transport)
  - **Success**: Claude Code can use the MCP server to query a GnuCash book

- [ ] **5.2 Agent loop architecture**
  - Define the core agent loop pattern:
    ```
    loop:
      1. Observe: Read current state from GnuCash DB
      2. Analyze: LLM processes observations against rules (Dhall config)
      3. Plan: Generate proposed actions
      4. Validate: Check actions against authorization rules
      5. Execute: Apply authorized actions (or queue for approval)
      6. Log: Record all actions in audit trail
      7. Sleep: Wait for next cycle (configurable interval)
    ```
  - Implement as reusable framework agents can extend
  - Configurable via Dhall (cycle interval, retry policy, error handling)
  - **Success**: Framework compiles and runs a no-op agent loop

- [ ] **5.3 Audit trail system**
  - Append-only log of all agent actions:
    ```json
    {
      "timestamp": "2026-03-15T10:30:00Z",
      "agent": "spend-monitor",
      "action": "categorize_transaction",
      "transaction_guid": "abc123...",
      "old_category": "Uncategorized",
      "new_category": "Expenses:SaaS:Compute",
      "authorization": "auto",
      "confidence": 0.95,
      "reasoning": "Matched 'Modal Labs' vendor pattern"
    }
    ```
  - Store in SQLite (separate from GnuCash DB) or structured log files
  - Queryable: "show me all actions agent X took in the last 7 days"
  - **Success**: Audit log captures all test agent actions

- [ ] **5.4 Dhall agent configuration**
  - Per-agent config files:
    ```dhall
    -- dhall/agents/spend-monitor.dhall
    let Agent = ./types/Agent.dhall

    in Agent::{
      , name = "spend-monitor"
      , description = "Monitors SaaS and compute spending"
      , schedule = Agent.Schedule.Hourly 1
      , authorization = Agent.AuthLevel.Auto
      , capabilities =
        { read_accounts = True
        , read_transactions = True
        , categorize = True
        , write_transactions = False
        , max_amount = None Natural
        }
      , vendor_patterns =
        [ { pattern = "MODAL LABS", category = "Expenses:SaaS:Compute" }
        , { pattern = "VERCEL", category = "Expenses:SaaS:Hosting" }
        , { pattern = "GITHUB", category = "Expenses:SaaS:DevTools" }
        , { pattern = "ANTHROPIC", category = "Expenses:SaaS:AI" }
        , { pattern = "AWS", category = "Expenses:Cloud:AWS" }
        ]
      }
    ```
  - **Success**: All agent configs compile; `just dhall-check` passes

- [ ] **5.5 Agent state management**
  - Each agent maintains state between cycles:
    - Last processed transaction date
    - Known subscriptions and their status
    - Running spend totals per category
    - Alert thresholds and escalation state
  - State stored in agent-specific SQLite or JSON files
  - State is recoverable (agent can rebuild from GnuCash DB if state is lost)
  - **Success**: Agent can stop and restart without losing progress

### User Interaction Point
> After 5.1-5.5: Add the MCP server to your Claude Code config:
> ```json
> {"mcpServers": {"gnucash": {"command": "gnucash-core-cli", "args": ["mcp", "--book", "/path/to/book.gnucash"]}}}
> ```
> Try asking Claude to "show me my account balances" or "what did I spend on SaaS last month?"
> This validates the MCP integration before we build autonomous agents on top.

---

## Week 6: First Three Agents

### Tasks

- [ ] **6.1 spend-monitor agent**
  - **Purpose**: Track and categorize spending, detect anomalies, report trends
  - **Capabilities** (Auto tier):
    - Read all transactions from last cycle
    - Match vendor names against Dhall patterns
    - Categorize uncategorized transactions
    - Calculate spending totals per category
    - Detect anomalies (spending >2 std dev from mean)
    - Generate spend report (JSON + optional HTML)
  - **Triggers**: Scheduled (hourly), on-demand via MCP
  - **Output**: Spend report with categories, trends, anomalies
  - **Test plan**:
    - [ ] Correctly categorizes known vendors (Modal, Vercel, GitHub, AWS, Anthropic)
    - [ ] Detects spending anomaly when injecting outlier transaction
    - [ ] Handles empty transaction set gracefully
    - [ ] Produces valid JSON report
    - [ ] Audit trail captures all categorization actions

- [ ] **6.2 report-generator agent**
  - **Purpose**: Generate financial reports on schedule or on-demand
  - **Capabilities** (Auto tier):
    - Generate trial balance, income statement, balance sheet
    - Monthly/quarterly/annual reporting periods
    - Multi-format output: JSON, HTML (gt tables), PDF (via R + gnucashr)
    - Comparative reports (this month vs last month, YoY)
    - Budget vs actual variance
  - **Triggers**: Scheduled (daily/weekly/monthly), on-demand
  - **Output**: Report files in configured format
  - **Test plan**:
    - [ ] Trial balance sums to zero
    - [ ] Income statement revenue - expenses = net income
    - [ ] Balance sheet assets = liabilities + equity
    - [ ] Comparative report shows correct delta
    - [ ] HTML output renders correctly

- [ ] **6.3 transaction-categorizer agent**
  - **Purpose**: Auto-categorize incoming transactions using LLM + rules
  - **Capabilities** (Auto tier with Review escalation):
    - Match transactions against Dhall vendor patterns (deterministic)
    - For unmatched: use LLM to suggest category based on description
    - High-confidence matches (>0.9) auto-apply
    - Low-confidence matches (<0.9) queue for human review
    - Learn from corrections (update Dhall patterns)
  - **Triggers**: On new transaction import, scheduled batch
  - **Output**: Categorized transactions, review queue
  - **Test plan**:
    - [ ] Known vendors categorized without LLM
    - [ ] Unknown vendors get reasonable LLM suggestions
    - [ ] Low-confidence suggestions queued for review
    - [ ] Learning: after correction, pattern added to Dhall config
    - [ ] No categories assigned without audit trail entry

- [ ] **6.4 Agent testing framework**
  - Test harness for agents:
    - Fixture GnuCash databases with known transactions
    - Mock clock for scheduled triggers
    - Assertion helpers for audit trail verification
    - Snapshot testing for report output
  - Integration tests run all three agents against shared fixture DB
  - **Success**: `just agents::test` runs all agent tests; `just agents::integration` runs E2E

- [ ] **6.5 Agent Justfile recipes**
  ```just
  # agents/justfile

  # Run a specific agent once
  run agent book:
    gnucash-agent run {{agent}} --book {{book}} --config dhall/agents/{{agent}}.dhall

  # Run all agents in daemon mode
  daemon book:
    gnucash-agent daemon --book {{book}} --config dhall/agents/

  # Test all agents
  test:
    gnucash-agent test --fixtures lib/gnucash-core/test/fixtures/

  # Integration test
  integration:
    gnucash-agent integration-test

  # Show agent status
  status:
    gnucash-agent status

  # Show audit trail
  audit agent="" days="7":
    gnucash-agent audit --agent {{agent}} --days {{days}}
  ```

### User Interaction Point
> After Week 6: Run the spend-monitor against your real GnuCash book:
> `just agents::run spend-monitor /path/to/book.gnucash`
> Review the output:
> - Are vendor categorizations correct?
> - Are spending totals accurate?
> - Check the audit trail: `just agents::audit spend-monitor`
> This is the moment of truth -- does the agent produce useful financial intelligence?

---

## Go/No-Go Gate G3

| Criterion | How to Verify | Required? |
|-----------|--------------|-----------|
| MCP server responds to Claude Code queries | Test in Claude Code session | Yes |
| spend-monitor reads real book, produces report | `just agents::run spend-monitor` | Yes |
| report-generator produces valid financial reports | Manual review of output | Yes |
| transaction-categorizer handles known + unknown vendors | Test suite passes | Yes |
| All agent actions in audit trail | `just agents::audit` shows entries | Yes |
| No writes without authorization check | Code review of auth checks | Yes |
| Agent tests pass | `just agents::test` | Yes |

**Decision**: If spend-monitor produces useful reports from real data, proceed to security
hardening. If categorization accuracy is below 80%, iterate on patterns before proceeding.
