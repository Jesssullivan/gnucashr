# Week 8: Remaining Agents + E2E

**Timeline**: Week 8 of 8-week epic
**Goal**: Complete agent suite (7 agents total), E2E tests, documentation

---

## 8.1 invoice-generator Agent (REVIEW tier)

**Purpose**: Generate invoice summaries from receivable accounts.

**Algorithm**:
1. Get all RECEIVABLE accounts via `account_tree()`
2. Get transactions with outstanding receivable splits
3. Group by customer (account name or parent path)
4. Generate invoice line items from splits
5. Compute totals, tax estimates, due dates
6. Queue for review (REVIEW tier = human verifies before sending)

**Report**: `{ invoices: [...], total_outstanding, overdue_count }`

**Dhall Config**: 7 tools (read-only + audit), REVIEW tier, OnDemand schedule

---

## 8.2 tax-estimator Agent (AUTO tier)

**Purpose**: Estimate tax obligations from income/expense data.

**Algorithm**:
1. Get all INCOME and EXPENSE accounts
2. Sum income by category (salary, freelance, investment, etc.)
3. Sum deductible expenses by category
4. Apply estimated tax brackets (configurable rates)
5. Project quarterly estimated tax payment
6. Compare to previous estimates in state DB

**Report**: `{ gross_income, deductions, taxable_income, estimated_tax, quarterly_payment, effective_rate }`

**Dhall Config**: 6 tools (read-only), AUTO tier, Daily schedule

---

## 8.3 subscription-manager Agent (AUTO tier)

**Purpose**: Detect recurring transactions (subscriptions) and track spending.

**Algorithm**:
1. Get all transactions (last 6 months)
2. Group by vendor (description normalization)
3. For each vendor group:
   - Count occurrences
   - Check interval regularity (monthly, weekly, annual)
   - Check amount consistency (within 10% tolerance)
4. Flag recurring patterns as subscriptions
5. Calculate monthly/annual subscription cost
6. Detect new, cancelled, and price-changed subscriptions vs. last run

**Report**: `{ subscriptions: [...], monthly_total, annual_total, new: [...], cancelled: [...], price_changes: [...] }`

**Dhall Config**: 6 tools (read-only), AUTO tier, Daily schedule

---

## 8.4 bill-pay Agent (APPROVE tier, stub)

**Purpose**: Schedule and execute bill payments (stub with approval flow).

**Algorithm**:
1. Get PAYABLE and LIABILITY accounts
2. Identify upcoming bills from transaction patterns
3. For each bill due:
   - Create ApprovalRequest in approval queue
   - Include payment details (amount, payee, due date)
4. Check previously approved requests
5. Execute approved payments via `post_transaction()`
6. Report on pending, approved, rejected, and executed payments

**Report**: `{ bills_due: [...], pending_approval: [...], executed: [...], total_due, total_executed }`

**Dhall Config**: 7 tools (read + write), APPROVE tier, Daily schedule

---

## 8.5 E2E Integration Tests

Catch2 tests covering:
- All 7 agents run against seeded fixture
- Security enforcement blocks unauthorized writes
- Approval workflow end-to-end (create → approve → execute)
- Agent state persistence across runs
- Rate limiter integration

---

## 8.6-8.8 Documentation & Polish

- Update PROGRESS.md with final metrics
- Update MEMORY.md
- Justfile recipes for new agents
- Dhall type-checking for new configs

---

## File Plan

### New Files
- `dhall/agents/invoice-generator.dhall`
- `dhall/agents/tax-estimator.dhall`
- `dhall/agents/subscription-manager.dhall`
- `dhall/agents/bill-pay.dhall`

### Modified Files
- `lib/gnucash-core/include/gnucash/agent.h` - Add 4 new agent declarations
- `lib/gnucash-core/src/agent.cpp` - Implement 4 new agents + dispatch
- `lib/gnucash-core/test/test_agent.cpp` - Add tests for new agents
- `lib/gnucash-core/CMakeLists.txt` - No changes needed (agents in library)
- `justfile` - Add recipes for new agents
- `docs/epic/PROGRESS.md` - Update metrics
