# gnucashr Monorepo Epic: Agentic Financial Management

**Epic Duration**: 8 weeks (Feb 24 - Apr 21, 2026)
**Epic Owner**: Jess Sullivan
**Status**: Planning

---

## Vision

Transform gnucashr from a standalone R package into a **monorepo platform for agentic
financial management**, using GnuCash's portable SQLite/XML database as the solid,
auditable backend for autonomous agent loops that monitor spending, manage subscriptions,
generate reports/invoices, and eventually pay bills.

## Strategic Thesis

1. **GnuCash is the ideal agent backend**: Double-entry bookkeeping enforces correctness.
   SQLite is portable, file-based, git-friendly. The schema is stable (20+ years).
   No cloud dependency, no vendor lock-in, full data ownership.

2. **Low-risk financial ops are agent-ready**: Categorizing transactions, generating
   reports, monitoring SaaS spend, tracking subscriptions, calculating tax estimates,
   creating invoices -- these are high-frequency, low-risk tasks where agents excel.

3. **High-risk ops need human-in-the-loop**: Paying bills, cancelling subscriptions,
   moving money -- these require explicit human approval via a tiered authorization model.

4. **gnucashr R package remains the analytical core**: The existing R package (10K+ lines,
   21 C++ exports, monadic error handling, lazy forecasting AST) provides the computation
   engine. Agent loops orchestrate it.

## Architecture

```
gnucashr/                          # Monorepo root
  justfile                         # Central command runner (replaces Makefile)
  flake.nix                        # Nix flake (builds everything)
  MODULE.bazel                     # Bazel module (C++ hermetic builds)

  packages/
    gnucashr/                      # R package (CRAN-submittable)
      DESCRIPTION
      R/
      src/                         # C++ (Rcpp, RcppParallel)
      tests/
      vignettes/

  agents/
    spend-monitor/                 # SaaS/compute spend tracking agent
    subscription-manager/          # Subscription lifecycle agent
    report-generator/              # Financial report agent
    invoice-generator/             # Invoice creation agent
    tax-estimator/                 # Tax calculation agent
    transaction-categorizer/       # Auto-categorization agent
    bill-pay/                      # Bill payment agent (human-in-the-loop)

  lib/
    gnucash-core/                  # C++ library: GnuCash DB read/write (standalone)
    dhall/                         # Dhall configs: agent rules, account mappings, tax tables
    proto/                         # Agent communication protocol definitions

  tools/
    sidecar/                       # Agent security sidecar (audit, rate-limit, approval)
    identity/                      # Agent identity management (RemoteJuggler integration)

  docs/
    epic/                          # This planning directory
```

## Key Design Decisions

| Decision | Choice | Rationale |
|----------|--------|-----------|
| Command runner | Justfile | Language-agnostic, module system, parallel recipes |
| Config language | Dhall | Type-safe, no injection risk, composable, git-friendly |
| Agent interface | MCP (Model Context Protocol) | Standard, Claude-native, extensible |
| Backend DB | GnuCash SQLite | Portable, auditable, 20-year stable schema |
| Build system | Nix + Bazel | Nix for reproducibility, Bazel for C++ hermeticity |
| Authorization | Tiered (auto/review/approve) | Low-risk auto, medium review, high approve |
| Primary languages | R, C++, Dhall | R for analysis, C++ for perf, Dhall for config |

## Sprint Map

| Week | Phase | Focus | Deliverable |
|------|-------|-------|-------------|
| 1-2 | Foundation | Monorepo restructure, Justfile, Dhall bootstrap | [01-FOUNDATION.md](01-FOUNDATION.md) |
| 3-4 | Interface | Standalone C++ lib, enhanced GnuCash ops | [02-GNUCASH-INTERFACE.md](02-GNUCASH-INTERFACE.md) |
| 5-6 | Agents | Agent framework, first 3 agents, MCP server | [03-AGENT-FRAMEWORK.md](03-AGENT-FRAMEWORK.md) |
| 7-8 | Integration | Security sidecar, identity, E2E testing | [04-INTEGRATION.md](04-INTEGRATION.md) |

## Go/No-Go Gates

| Gate | Week | Criteria | Decider |
|------|------|----------|---------|
| G1: Restructure | 2 | R package passes R CMD check from new location; `just check` works | Owner |
| G2: C++ Lib | 4 | Standalone C++ reads/writes GnuCash SQLite without R | Owner |
| G3: First Agent | 6 | spend-monitor agent reads real GnuCash book, produces report | Owner |
| G4: Security | 8 | Sidecar blocks unauthorized financial ops; audit trail complete | Owner |

## Risk Register

| Risk | Likelihood | Impact | Mitigation |
|------|-----------|--------|------------|
| R CMD check breaks during restructure | Medium | High | Keep packages/gnucashr/ as clean R package; .Rbuildignore monorepo files |
| Agent hallucinates financial data | Medium | Critical | All agent reads go through validated C++ paths; checksums on DB |
| Dhall learning curve | Low | Medium | Start with simple configs; Dhall playground for iteration |
| Scope creep into bill-pay too early | High | Medium | bill-pay is Week 7-8 and gated on security sidecar |
| GnuCash DB schema changes in 6.0 | Low | Medium | Pin to 5.x schema; abstraction layer in C++ lib |

## Related Projects

- **gnucash-mcp**: Forked from ninetails-io; rudimentary Python/piecash, superseded by our C++ MCP
- **RemoteJuggler**: Agent identity management, Chapel+Rust+Nix patterns, Justfile reference
- **ruvnet ecosystem**: Claude-Flow orchestration, FACT financial data retrieval, Agentic Payments
- **OpenClaw/IronClaw**: OpenClaw (cautionary tale: no security), IronClaw (WASM sandbox, RBAC)
- **OpenCost MCP**: Kubernetes cost monitoring via MCP (compute spend tracking)
- **piecash2**: Python GnuCash SQLAlchemy ORM (reference for our C++ lib design)
- **GnuCash 5.14**: Latest stable (Dec 2025), Python bindings via SWIG, 6.0 planned Mar 2027
- **Plaid**: Bank data API with recurring transaction detection for subscription tracking
- **FinGPT/FinRobot**: Open-source financial LLM and agent frameworks

## Files in This Epic

| File | Purpose |
|------|---------|
| [00-OVERVIEW.md](00-OVERVIEW.md) | This file - epic overview and architecture |
| [01-FOUNDATION.md](01-FOUNDATION.md) | Weeks 1-2: Monorepo structure, Justfile, Dhall |
| [02-GNUCASH-INTERFACE.md](02-GNUCASH-INTERFACE.md) | Weeks 3-4: Standalone C++ lib, enhanced ops |
| [03-AGENT-FRAMEWORK.md](03-AGENT-FRAMEWORK.md) | Weeks 5-6: Agent loops, MCP server, first agents |
| [04-INTEGRATION.md](04-INTEGRATION.md) | Weeks 7-8: Security, identity, E2E |
| [PROGRESS.md](PROGRESS.md) | Living progress tracker with todos |
| [RESEARCH.md](RESEARCH.md) | Research findings and references |
