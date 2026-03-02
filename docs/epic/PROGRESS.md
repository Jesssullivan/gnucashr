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

## Phase 3: Agent Framework (Weeks 5-6) -- IN PROGRESS

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
| Test count (C++) | 50+ tests | 91 | All passing |
| MCP test scripts | 3+ | 3 | Passing |
| MCP tools | 20 | 20 | 15 read, 4 write, 1 audit |
| Agent configs | 7 | 3 | spend-monitor, report-generator, transaction-categorizer |
| Dhall configs | All compile | 17 files | All pass (agents + types) |
| Code coverage | >80% | ~80% (R only) | Baseline |
| GnuCash schema tables mapped | 10+ | 6 (via R) | Partial |
| Justfile recipes | 20+ | 36 | Root (24) + package (12) |
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

---

## Blockers & Risks (Active)

| ID | Description | Owner | Status |
|----|-------------|-------|--------|
| B1 | R CMD check error (missing Author/Maintainer) | -- | Known fix: run devtools::document() |
| | | | |
