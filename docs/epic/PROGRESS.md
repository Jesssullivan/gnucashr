# Epic Progress Tracker

**Started**: 2026-02-24
**Target Completion**: 2026-04-21 (8 weeks)

---

## Phase 1: Foundation (Weeks 1-2) -- COMPLETE

### Week 1: Monorepo Restructure -- COMPLETE
- [x] 1.1 Move R package into packages/gnucashr/
- [x] 1.2 Create root Justfile (18 recipes)
- [x] 1.3 Update flake.nix for monorepo (all src paths updated)
- [x] 1.4 Update CI for monorepo layout (all 6 workflows + gitlab-ci)
- [x] 1.5 Update .Rbuildignore and .gitignore

### Week 2: Dhall Bootstrap -- COMPLETE
- [x] 2.1 Bootstrap Dhall configuration layer (5 types, package.dhall)
- [x] 2.2 Define agent authorization rules in Dhall (15 rules, 3 tiers)
- [x] 2.3 Define account mapping templates in Dhall (SaaS, tax, income)
- [x] 2.4 Justfile package-level recipes (packages/gnucashr/justfile)
- [x] 2.5 Bazel MODULE.bazel update for monorepo (done in Week 1)
- [x] 2.6 Documentation: CONTRIBUTING.md update (monorepo workflow)

### Gate G1: [x] PASSED (local)
- [x] R CMD check passes from packages/gnucashr/ (0 errors, 3 warnings, 3 notes)
- [x] Nix build produces tarball (nix flake show verified)
- [x] Justfile lists all recipes (21 recipes)
- [x] Dhall types compile (just dhall-check passes)
- [ ] CI green on both platforms (needs commit + push)

---

## Phase 2: GnuCash Interface (Weeks 3-4) -- COMPLETE

### Week 3: Standalone C++ Library -- COMPLETE
- [x] 3.1 Design lib/gnucash-core/ C++ library (headers: result.h, fraction.h, guid.h, types.h, book.h)
- [x] 3.2 Implement GnuCash SQLite schema reader (book.cpp: ~725 lines, all CRUD)
- [x] 3.3 Implement Result<T,E> in C++ (with void specialization, map, bind, match)
- [x] 3.4 C++ test suite with Catch2 (5 test files: result, fraction, guid, types, book)
- [x] 3.5 Nix derivation for C++ library (packages.gnucashCore, packages.gnucashCoreTests)
- [x] 3.6 CMakeLists.txt build system (cmake + pkg-config + Catch2)
- [x] 3.7 Justfile C++ recipes (cpp-configure, cpp-build, cpp-test, cpp-clean, cpp-nix-*)

### Week 4: Enhanced Write Operations
- [x] 4.1 Transaction posting in C++ lib (book.cpp: post_transaction with validation)
- [x] 4.2 Account creation in C++ lib (book.cpp: create_account with validation)
- [x] 4.3 JSON API for C++ lib (gnucash-bridge: 18 methods, stdin/stdout JSON-lines)
- [x] 4.4 Wire C++ lib back into R package via Rcpp (gc_* functions, vendored sources)
- [x] 4.5 OFX import in C++ lib (ofx.h/ofx.cpp, v1 SGML + v2 XML, 8 tests)
- [x] 4.6 Dhall schema for GnuCash operations (GnuCashOperation.dhall, 14 ops, 6 types)

### Gate G2: [x] PASSED
- [x] C++ lib reads GnuCash SQLite without R
- [x] C++ lib writes transactions GnuCash opens
- [x] JSON API works via stdin/stdout (gnucash-bridge, 18 methods)
- [x] R package still passes check (586/586 tests, 0 failures)
- [x] C++ test suite passes (91/91 tests, 0.36s)

---

## Phase 3: Agent Framework (Weeks 5-6) -- COMPLETE

### Week 5: Agent Framework + MCP Server -- COMPLETE
**Detailed Plan**: See `docs/epic/05-WEEK5-PLAN.md` (7-day implementation guide)

- [x] 5.1 JSON-RPC 2.0 protocol layer (mcp.cpp: ~800 lines, 20 MCP tools, protocol auto-detection)
- [x] 5.2 MCP tool schema (20 tools: 15 read, 4 write, 1 audit)
- [x] 5.3 Audit trail system (audit.cpp: ~650 lines, Aperture-compatible SQLite, before/after state capture)
- [x] 5.4 Dhall agent configurations (3 example agents: spend-monitor [6 tools], report-generator [8 tools], transaction-categorizer [6 tools])
- [x] 5.5 Tool filtering via Dhall (dhall_config.cpp: --agent CLI arg, filtering in get_tool_definitions)
- [x] 5.6 Integration testing with Claude Code (3 test scripts: protocol, audit, agent filtering)
- [x] 5.7 Documentation (MCP_ARCHITECTURE.md [410 lines], AGENT_DEVELOPMENT_GUIDE.md [complete])
- [x] 5.8 Nix integration (gnucashMcp derivation: Release build + bundled Dhall configs, `nix build .#gnucashMcp` verified)

### Week 6: First Three Agents -- COMPLETE
**Detailed Plan**: See `docs/epic/06-WEEK6-PLAN.md`

- [x] 6.1 Agent state database (agent_state.h/cpp: per-agent SQLite, key-value store + review queue)
- [x] 6.2 Agent execution framework (agent.h/cpp: AgentResult, VendorPattern, MatchResult, run_agent dispatch)
- [x] 6.3 spend-monitor agent (vendor matching, category totals by account path, anomaly detection >2 stddev)
- [x] 6.4 report-generator agent (trial balance, income statement, balance sheet with debit/credit sign conventions)
- [x] 6.5 transaction-categorizer agent (find Imbalance splits, pattern match, review queue, auto-apply >0.90)
- [x] 6.6 Agent Catch2 tests (20 tests: vendor matching, state DB, agent runs, dispatch, financial statement verification)
- [x] 6.7 Build system updates (CMakeLists.txt: agent sources in library, Justfile: 4 agent recipes)

### Gate G3: [x] PASSED
- [x] MCP server responds to Claude Code queries (20 tools, JSON-RPC 2.0)
- [x] spend-monitor produces report (category totals, anomaly detection)
- [x] report-generator: trial balance sums to zero (verified in Catch2)
- [x] transaction-categorizer categorizes and queues reviews (review queue tested)
- [x] Agent tests pass (149/149 Catch2 tests, 0 failures)

---

## Phase 4: Integration (Weeks 7-8) -- COMPLETE

### Week 7: Security + Identity -- COMPLETE
**Detailed Plan**: See `docs/epic/07-WEEK7-PLAN.md`

- [x] 7.1 Identity provider (identity.h/cpp: Identity struct, resolve_identity with CLI/env/system fallback)
- [x] 7.2 Security policy & authorization enforcement (security.h/cpp: SecurityPolicy, AuthorizationRule, classify_tool, security_check)
- [x] 7.3 Rate limiting (RateLimiter: sliding window per agent:operation, configurable max_per_hour)
- [x] 7.4 Human-in-the-loop approval queue (approval.h/cpp: ApprovalDB with SQLite, create/approve/reject/query)
- [x] 7.5 Anomaly detection (check_transaction_anomaly, extract_amount_cents, severity scoring)
- [x] 7.6 MCP dispatch integration (security_check inserted in handle_tools_call, --identity/--enforce CLI flags)
- [x] 7.7 Security Catch2 tests (36 tests: identity, classification, rate limiter, security check, amount extraction, anomaly, rules, approval queue)

### Week 8: Remaining Agents + E2E -- COMPLETE
**Detailed Plan**: See `docs/epic/08-WEEK8-PLAN.md`

- [x] 8.1 invoice-generator agent (REVIEW tier: receivable accounts, invoice summaries, overdue detection)
- [x] 8.2 tax-estimator agent (AUTO tier: progressive tax brackets, deduction detection, quarterly estimates)
- [x] 8.3 subscription-manager agent (AUTO tier: recurring transaction detection, vendor normalization, change tracking)
- [x] 8.4 bill-pay agent (APPROVE tier: payable detection, approval flow stub, pending payment requests)
- [x] 8.5 E2E integration test suite (all 7 agents dispatch, 14 new Catch2 tests)
- [x] 8.6 Dhall configs for all 7 agents (invoice-generator, tax-estimator, subscription-manager, bill-pay)
- [x] 8.7 Documentation (Week 8 plan, PROGRESS.md, MEMORY.md)
- [ ] 8.8 CRAN submission preparation (deferred -- R package unchanged this phase)

### Gate G3.5: [x] PASSED
- [x] Security enforcement blocks unauthorized writes (security_check → DENY/REQUIRE_APPROVAL)
- [x] Identity resolution works (CLI → env → system)
- [x] Rate limiting enforced per agent:operation
- [x] Approval queue stores and resolves requests
- [x] Anomaly detection flags large transactions
- [x] All 185 C++ tests passing (149 existing + 36 security)

### Gate G4: [x] PASSED
- [x] All 7 agents functional and tested
- [x] E2E test: all 7 agents dispatch successfully
- [x] 199 C++ tests passing (185 + 14 new agent tests)
- [x] Dhall configs for all 7 agents type-check
- [ ] CRAN submission (deferred)

---

## Phase 5: Bank Feed Import Pipeline (Weeks 9-11) -- COMPLETE

### Week 9: Foundation Layer -- COMPLETE
- [x] 9.1 Slots table module (slots.h/cpp: ensure_slots_table, get/set/delete slots, find_split_by_fitid, check_fitids)
- [x] 9.2 Book::update_split() and raw_db() accessor (split recategorization, SQLite handle exposure)
- [x] 9.3 CSV parser (csv.h/cpp: 5 format presets [PayPal, Stripe, Venmo, Apple Card, generic], auto-detect, date normalization)
- [x] 9.4 OFX amount_fraction field (Fraction parsing directly from TRNAMT string, no double precision loss)
- [x] 9.5 Fraction::from_string() (parses "$-45.23", "(45.23)", "1,234.56" to exact Fraction)
- [x] 9.6 Week 9 tests (36 new Catch2 tests: slots, csv, fraction, book update_split, OFX fraction)

### Week 10: Import Pipeline -- COMPLETE
- [x] 10.1 Bank feed import engine (bank_feed.h/cpp: import_ofx, import_csv, check_duplicates, FITID dedup, synthetic FITID)
- [x] 10.2 MCP tool: gnucash_import_ofx (WRITE, parse + dedup + post transactions)
- [x] 10.3 MCP tool: gnucash_import_csv (WRITE, format auto-detect, generic format overrides)
- [x] 10.4 MCP tool: gnucash_check_duplicates (READ, batch FITID check)
- [x] 10.5 MCP tools: gnucash_set_slot (WRITE) + gnucash_get_slots (READ)
- [x] 10.6 MCP tool: gnucash_update_split (WRITE, recategorize with before/after audit)
- [x] 10.7 Week 10 tests (13 bank_feed tests: OFX import, CSV import, dedup, amounts, edge cases)

### Week 11: Agents, Reconciliation, Plugin -- COMPLETE
- [x] 11.1 Reconciliation engine (reconcile.h/cpp: reconcile_account, find_cross_institution_matches, description similarity)
- [x] 11.2 MCP tools: gnucash_reconcile_account (WRITE), gnucash_match_imported (READ), gnucash_bank_feed_status (READ)
- [x] 11.3 bank-feed-importer agent (REVIEW tier, 7 tools, 7 vendor patterns)
- [x] 11.4 reconciler agent (APPROVE tier, 7 tools)
- [x] 11.5 Claude Code .mcp.json plugin
- [x] 11.6 Build system updates (CMakeLists.txt, justfile, PROGRESS.md)
- [x] 11.7 Week 11 tests (8 reconcile tests, 4 agent tests, 5 MCP tool tests)

### Gate G5: [x] PASSED
- [x] 262 C++ tests passing (199 existing + 63 new)
- [x] 29 MCP tools (20 existing + 9 new: 5 write, 4 read)
- [x] 9 agents (7 existing + bank-feed-importer, reconciler)
- [x] All amounts use Fraction (no double precision loss in import path)
- [x] FITID dedup works for both OFX and CSV imports
- [x] Cross-institution transfer matching (amount inversion + date window + description similarity)
- [x] Dhall configs for all 9 agents type-check

---

## Metrics Dashboard

| Metric | Target | Current | Status |
|--------|--------|---------|--------|
| R CMD check | 0 ERR, 0 WARN | 0 ERR, 3 WARN, 3 NOTE | Passing |
| Test count (R) | 21 files, 4692 lines | 21/4692 | Baseline |
| Test count (C++) | 250+ tests | 262 | All passing |
| MCP test scripts | 3+ | 3 | Passing |
| MCP tools | 29 | 29 | 19 read, 9 write, 1 audit |
| Agent implementations | 9 | 9 | +bank-feed-importer, +reconciler |
| Agent configs (Dhall) | 9 | 9 | Tool filtering via Dhall |
| Dhall configs | All compile | 19 files | All pass (agents + types) |
| C++ library modules | 10+ | 14 | +slots, csv, bank_feed, reconcile |
| Code coverage | >80% | ~80% (R only) | Baseline |
| GnuCash schema tables mapped | 10+ | 7 (via R+slots) | Partial |
| Justfile recipes | 20+ | 43 | Root (31) + package (12) |
| CI pipelines green | Both platforms | Yes (current layout) | Baseline |

---

## Decision Log

| Date | Decision | Rationale |
|------|----------|-----------|
| 2026-02-24 | Epic created | Vision for agentic financial management on GnuCash |
| 2026-02-24 | gnucash-mcp forked, will supersede | Rudimentary Python MCP; C++ MCP in Week 5 |
| 2026-02-24 | Week 1 complete | Monorepo restructure, CI, Justfile all verified |
| 2026-02-24 | Week 2 complete | Dhall bootstrap, 12 config files, templates |
| 2026-02-25 | gnucash-core library built | 65/65 Catch2 tests passing, full CRUD ops |
| 2026-02-25 | Result<T,E> uses optional | std::variant fails with T==E and non-default-constructible T |
| 2026-02-25 | gnucash-bridge JSON API | stdin/stdout JSON-lines, 18 methods, nlohmann_json |
| 2026-02-25 | R package wired to gnucash-core | Vendored sources, 14 gc_* Rcpp exports, 586 R tests pass |
| 2026-02-25 | OFX parser standalone | Extracted from R Rcpp, handles v1 SGML + v2 XML, 8 Catch2 tests |
| 2026-02-25 | GnuCashOperation Dhall type | 14 operations, 6 sub-types, maps to JSON API methods |
| 2026-02-25 | Phase 2 complete | 91 C++ tests, 586 R tests, all gates passed |
| 2026-02-25 | Week 5 plan created | MCP server C++ implementation, Aperture-ready audit, 7-day timeline |
| 2026-02-25 | MCP protocol layer complete | JSON-RPC 2.0 over stdin/stdout, 20 tools, protocol auto-detection |
| 2026-02-25 | Audit trail complete | Aperture-compatible SQLite schema, before/after state, duration timing |
| 2026-02-25 | Agent configs complete | 3 Dhall agents with tool filtering, spend-monitor (6 tools), report-generator (8 tools), transaction-categorizer (6 tools + Review tier) |
| 2026-02-25 | Week 5 Phase 5.1-5.4 complete | MCP server ready, all tests passing, comprehensive documentation |
| 2026-02-25 | Week 5 complete | gnucashMcp Nix derivation (Release + Dhall configs), dhall_config.cpp warning fixed, all 91 C++ tests passing |
| 2026-03-02 | Week 6 complete | 3 agents implemented (spend-monitor, report-generator, transaction-categorizer), agent state DB, review queue, 149 C++ tests |
| 2026-03-02 | Agents are not daemons | Agents run as MCP sessions with --agent + --run flags, one binary (gnucash-bridge) |
| 2026-03-02 | account_tree() required | get_accounts() doesn't populate full_path; agents must use account_tree() for path-based operations |
| 2026-03-02 | Week 7 complete | Security sidecar as inline interceptor, identity provider, rate limiter, approval queue, anomaly detection, 185 C++ tests |
| 2026-03-02 | Sidecar is inline | Not a separate process; security_check() called in MCP dispatch pipeline, keeps single-binary design |
| 2026-03-02 | Security opt-in | --enforce flag activates authorization enforcement; without it, all operations allowed (backward compatible) |
| 2026-03-02 | Week 8 complete | 4 new agents (invoice-generator, tax-estimator, subscription-manager, bill-pay), 7 Dhall configs, 199 C++ tests |
| 2026-03-02 | 8-week epic complete | All 4 phases done: monorepo, GnuCash interface, agent framework, integration. 7 agents, security sidecar, 199 tests |
| 2026-03-02 | Phase 5 started | Bank feed import pipeline: slots, CSV, bank_feed, reconcile modules |
| 2026-03-02 | Slots table lazy creation | CREATE IF NOT EXISTS works with both test fixtures and real GnuCash files |
| 2026-03-02 | FITID on splits not txns | GnuCash convention: online_id stored on splits via slots, scoped to account |
| 2026-03-02 | Amount parsing avoids double | Fraction::from_string() parses "$-45.23" directly to Fraction(-4523,100) |
| 2026-03-02 | OFX backward-compat | New amount_fraction alongside existing double amount field |
| 2026-03-02 | CSV format auto-detect | Headers matched against PayPal/Stripe/Venmo/Apple Card patterns |
| 2026-03-02 | Phase 5 complete | 4 new modules, 9 new MCP tools, 2 new agents, 63 new tests, .mcp.json plugin |

---

## Blockers & Risks (Active)

| ID | Description | Owner | Status |
|----|-------------|-------|--------|
| B1 | R CMD check error (missing Author/Maintainer) | -- | Known fix: run devtools::document() |
| | | | |
