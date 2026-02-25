# Epic Progress Tracker

**Started**: 2026-02-24
**Target Completion**: 2026-04-21 (8 weeks)

---

## Phase 1: Foundation (Weeks 1-2) -- IN PROGRESS

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

## Phase 2: GnuCash Interface (Weeks 3-4) -- NOT STARTED

### Week 3: Standalone C++ Library
- [ ] 3.1 Design lib/gnucash-core/ C++ library
- [ ] 3.2 Implement GnuCash SQLite schema reader
- [ ] 3.3 Implement Result<T,E> in C++
- [ ] 3.4 C++ test suite with Catch2
- [ ] 3.5 Nix derivation for C++ library

### Week 4: Enhanced Write Operations
- [ ] 4.1 Transaction posting in C++ lib
- [ ] 4.2 Account creation in C++ lib
- [ ] 4.3 JSON API for C++ lib (agent bridge)
- [ ] 4.4 Wire C++ lib back into R package via Rcpp
- [ ] 4.5 OFX import in C++ lib
- [ ] 4.6 Dhall schema for GnuCash operations

### Gate G2: [ ] PENDING
- [ ] C++ lib reads GnuCash SQLite without R
- [ ] C++ lib writes transactions GnuCash opens
- [ ] JSON API works via stdin/stdout
- [ ] R package still passes check
- [ ] C++ test suite passes

---

## Phase 3: Agent Framework (Weeks 5-6) -- NOT STARTED

### Week 5: Agent Framework + MCP Server
- [ ] 5.1 MCP server wrapping gnucash-core
- [ ] 5.2 Agent loop architecture
- [ ] 5.3 Audit trail system
- [ ] 5.4 Dhall agent configuration
- [ ] 5.5 Agent state management

### Week 6: First Three Agents
- [ ] 6.1 spend-monitor agent
- [ ] 6.2 report-generator agent
- [ ] 6.3 transaction-categorizer agent
- [ ] 6.4 Agent testing framework
- [ ] 6.5 Agent Justfile recipes

### Gate G3: [ ] PENDING
- [ ] MCP server responds to Claude Code queries
- [ ] spend-monitor produces useful report from real data
- [ ] All agent actions in audit trail
- [ ] Agent tests pass

---

## Phase 4: Integration (Weeks 7-8) -- NOT STARTED

### Week 7: Security + Identity
- [ ] 7.1 Security sidecar design
- [ ] 7.2 Implement rate limiting
- [ ] 7.3 Agent identity management
- [ ] 7.4 Human-in-the-loop approval flow
- [ ] 7.5 Anomaly detection in sidecar

### Week 8: Remaining Agents + E2E
- [ ] 8.1 invoice-generator agent
- [ ] 8.2 tax-estimator agent
- [ ] 8.3 subscription-manager agent
- [ ] 8.4 bill-pay agent (stub with approval flow)
- [ ] 8.5 E2E integration test suite
- [ ] 8.6 Performance benchmarks
- [ ] 8.7 Documentation
- [ ] 8.8 CRAN submission preparation

### Gate G4: [ ] PENDING
- [ ] Sidecar blocks unauthorized writes
- [ ] Audit trail complete
- [ ] Approval flow works
- [ ] E2E tests pass
- [ ] All 7 agents functional

---

## Metrics Dashboard

| Metric | Target | Current | Status |
|--------|--------|---------|--------|
| R CMD check | 0 ERR, 0 WARN | 0 ERR, 3 WARN, 3 NOTE | Passing |
| Test count (R) | 21 files, 4692 lines | 21/4692 | Baseline |
| Test count (C++) | 50+ tests | 0 | Not started |
| Agent count | 7 | 0 | Not started |
| Dhall configs | All compile | 12 files | All pass |
| Code coverage | >80% | ~80% (R only) | Baseline |
| GnuCash schema tables mapped | 10+ | 6 (via R) | Partial |
| Justfile recipes | 20+ | 18 | Root complete |
| CI pipelines green | Both platforms | Yes (current layout) | Baseline |

---

## Decision Log

| Date | Decision | Rationale |
|------|----------|-----------|
| 2026-02-24 | Epic created | Vision for agentic financial management on GnuCash |
| 2026-02-24 | gnucash-mcp forked, will supersede | Rudimentary Python MCP; C++ MCP in Week 5 |
| 2026-02-24 | Week 1 complete | Monorepo restructure, CI, Justfile all verified |

---

## Blockers & Risks (Active)

| ID | Description | Owner | Status |
|----|-------------|-------|--------|
| B1 | R CMD check error (missing Author/Maintainer) | -- | Known fix: run devtools::document() |
| | | | |
