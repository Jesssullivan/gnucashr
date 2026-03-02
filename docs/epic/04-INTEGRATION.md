# Phase 4: Integration, Security, Identity (Weeks 7-8)

**Goal**: Add security sidecar, agent identity management, remaining agents (invoice,
tax, bill-pay), and E2E testing. Prepare for production use.

---

## Week 7: Security Sidecar + Identity

### Tasks

- [ ] **7.1 Security sidecar design**
  - Sidecar process that monitors agent actions:
    ```
    Agent Process <--> Sidecar <--> GnuCash DB
                         |
                    Audit Log
                    Rate Limiter
                    Auth Checker
                    Anomaly Detector
    ```
  - All DB writes go through sidecar (agents cannot write directly)
  - Sidecar validates:
    - Agent has capability for requested operation (Dhall config)
    - Transaction amount within agent's max_amount
    - Rate limit not exceeded (operations per hour)
    - No duplicate transactions
    - Double-entry invariant maintained (splits sum to zero)
  - **Success**: Agent write attempt without capability is blocked and logged

- [ ] **7.2 Implement rate limiting**
  - Per-agent rate limits defined in Dhall:
    ```dhall
    { reads_per_hour = 1000
    , writes_per_hour = 50
    , categorizations_per_hour = 200
    , reports_per_hour = 10
    }
    ```
  - Token bucket algorithm
  - Burst allowance for batch operations (import 100 transactions at once)
  - Rate limit exceeded triggers alert, not hard failure (configurable)
  - **Success**: Rate-limited agent queues excess operations

- [ ] **7.3 Agent identity management (RemoteJuggler integration)**
  - Each agent has a distinct identity:
    ```dhall
    { agent_id = "spend-monitor-v1"
    , gpg_key = "agent-spend-monitor@gnucashr.local"
    , git_author = "GnuCashR Spend Monitor <spend-monitor@gnucashr.local>"
    , created_at = "2026-03-01"
    , capabilities_hash = "sha256:abc123..."  -- hash of capability config
    }
    ```
  - All agent-generated artifacts (reports, configs) are GPG-signed
  - Git commits by agents use agent-specific author identity
  - KeePassXC integration for credential storage (via keyring R package)
  - **Success**: Agent commits are attributable; GPG signatures verify

- [ ] **7.4 Human-in-the-loop approval flow**
  - For Review/Approve tier operations:
    1. Agent proposes action (stored in approval queue)
    2. Notification sent (email, Slack webhook, or CLI prompt)
    3. Human reviews and approves/denies
    4. Approved action executed with human's authorization recorded
    5. Denied action logged with reason
  - Approval queue: SQLite table with pending actions
  - CLI interface: `just agents::approve` shows pending, allows approve/deny
  - Timeout: unreviewed actions expire after configurable period
  - **Success**: bill-pay agent queues payment, human approves, payment logged

- [ ] **7.5 Anomaly detection in sidecar**
  - Detect suspicious agent behavior:
    - Unusual transaction amounts (statistical outlier detection)
    - Unexpected account access patterns
    - Rapid-fire operations (potential runaway loop)
    - Attempts to access accounts outside capability scope
  - Alert on anomaly; auto-pause agent on critical anomaly
  - **Success**: Injecting anomalous behavior triggers alert and pause

### User Interaction Point
> After 7.1-7.5: Try to make an agent do something it shouldn't:
> - Configure spend-monitor to try writing a transaction (should be blocked)
> - Set a rate limit of 5/hour and trigger 10 operations (should queue excess)
> - Review the approval flow: `just agents::approve`
> These security boundaries are what make autonomous financial agents safe.

---

## Week 8: Remaining Agents + E2E Testing

### Tasks

- [ ] **8.1 invoice-generator agent**
  - **Purpose**: Create professional invoices from GnuCash data
  - **Capabilities** (Review tier):
    - Read customer/vendor data from GnuCash
    - Generate invoice from template (Dhall-defined)
    - Output: PDF, HTML, JSON
    - Track invoice status (draft, sent, paid, overdue)
    - Integrate with GnuCash's native invoice/bill tracking
  - **Authorization**: Review tier (human reviews before sending)
  - **Test plan**:
    - [ ] Generates invoice with correct totals
    - [ ] PDF renders correctly
    - [ ] Invoice data matches GnuCash records
    - [ ] Status tracking works across agent restarts

- [ ] **8.2 tax-estimator agent**
  - **Purpose**: Calculate tax estimates from GnuCash data
  - **Capabilities** (Auto tier):
    - Categorize expenses by tax category (Dhall tax table)
    - Calculate quarterly estimated tax payments
    - Track deductible expenses
    - Generate tax summary report
    - Flag transactions that need tax category review
  - **Authorization**: Auto for calculations, Review for category changes
  - **Test plan**:
    - [ ] Correct tax category mapping
    - [ ] Quarterly estimates match manual calculation
    - [ ] Deductible expense report is complete
    - [ ] Handles multiple tax jurisdictions (configurable)

- [ ] **8.3 subscription-manager agent**
  - **Purpose**: Track and manage recurring subscriptions
  - **Capabilities** (Auto + Approve hybrid):
    - Detect recurring transactions (same vendor, similar amount, regular interval)
    - Track subscription inventory (what services, how much, when)
    - Calculate total subscription burn rate
    - Flag unused/underused subscriptions
    - Recommend cancellations (Auto)
    - Execute cancellation (Approve - future, requires vendor API integration)
  - **Authorization**: Auto for tracking/flagging, Approve for any action
  - **Test plan**:
    - [ ] Detects monthly subscriptions from transaction patterns
    - [ ] Correctly calculates burn rate
    - [ ] Handles annual subscriptions
    - [ ] Flags subscriptions with no recent usage (if usage data available)

- [ ] **8.4 bill-pay agent (stub with approval flow)**
  - **Purpose**: Schedule and execute bill payments
  - **Capabilities** (Approve tier):
    - Read scheduled transactions from GnuCash
    - Propose upcoming bill payments
    - Queue payments for human approval
    - Record approved payments in GnuCash
    - (Future: integration with bank APIs via Plaid/similar)
  - **Authorization**: All operations require Approve
  - **Test plan**:
    - [ ] Reads scheduled transactions correctly
    - [ ] Proposes payments in approval queue
    - [ ] Approved payment creates correct GnuCash transaction
    - [ ] Denied payment is logged and not executed

- [ ] **8.5 End-to-end integration test suite**
  - Full pipeline test:
    1. Start with empty GnuCash book
    2. Import OFX bank statement (50+ transactions)
    3. Run transaction-categorizer (should categorize ~80%)
    4. Run spend-monitor (should produce spend report)
    5. Run report-generator (trial balance, income statement)
    6. Run tax-estimator (should produce tax summary)
    7. Run subscription-manager (should detect recurring charges)
    8. Verify all audit trail entries
    9. Verify GnuCash DB integrity (open in GnuCash desktop)
  - **Success**: Full pipeline runs without errors; all reports accurate

- [ ] **8.6 Performance benchmarks**
  - Benchmark against real-world GnuCash books:
    - Small (100 transactions): all agents complete in <10s
    - Medium (5,000 transactions): all agents complete in <60s
    - Large (50,000 transactions): all agents complete in <5min
  - C++ lib benchmarks for read/write operations
  - Memory usage profiling
  - **Success**: Performance meets targets for medium-size books

- [ ] **8.7 Documentation**
  - User guide: setting up agents with your GnuCash book
  - Agent developer guide: creating new agents
  - Security model documentation
  - Dhall configuration reference
  - MCP server API reference
  - **Success**: Another developer can add a new agent following the guide

- [ ] **8.8 CRAN submission preparation (R package)**
  - Verify R package still CRAN-ready in monorepo layout
  - Version bump to 0.3.0 (reflects monorepo reorganization)
  - Update vignettes to reference agent integration
  - Final R CMD check --as-cran pass
  - **Success**: Ready to submit to CRAN (separate milestone)

### User Interaction Point
> After Week 8: This is the full demo moment. Run the complete pipeline:
> `just agents::daemon /path/to/your/real/book.gnucash`
> Let it run for a day. Review:
> - Spend report: accurate? Useful?
> - Tax estimates: reasonable?
> - Subscription tracking: found all your recurring charges?
> - Audit trail: comprehensive? Trustworthy?
> This determines if we're ready for daily use vs need another iteration.

---

## Go/No-Go Gate G4

| Criterion | How to Verify | Required? |
|-----------|--------------|-----------|
| Sidecar blocks unauthorized writes | Inject unauthorized write, verify blocked | Yes |
| Audit trail complete for all agent actions | `just agents::audit` review | Yes |
| Approval flow works for high-risk ops | Submit bill-pay, approve via CLI | Yes |
| Rate limiting active | Exceed rate limit, verify queuing | Yes |
| E2E test suite passes | `just test::e2e` | Yes |
| All 7 agents functional | `just agents::status` | Yes |
| GnuCash DB integrity preserved | Open in GnuCash desktop after agent run | Yes |
| Performance targets met | Benchmark results | Nice-to-have |
| R package CRAN-ready | `just gnucashr::check` | Nice-to-have |

**Decision**: If security boundaries hold and agents produce useful financial intelligence
from real data, the platform is ready for daily personal use. CRAN submission and
additional agent capabilities become follow-on work.
