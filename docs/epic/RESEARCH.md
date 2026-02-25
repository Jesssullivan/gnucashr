# Research Findings

Compiled from 6 parallel research agents on 2026-02-24.

---

## 1. GnuCash Upstream

### Current State
- **Latest stable**: GnuCash 5.14 (Dec 21, 2025)
- **Next release**: 5.15 planned Mar 29, 2026
- **GnuCash 6.0**: Tentatively March 2027
- **Backends**: XML (default, gzip-compressed), SQLite3, MySQL, PostgreSQL

### SQLite Schema (Key Tables)
| Table | Purpose | Key Columns |
|-------|---------|-------------|
| `accounts` | Chart of accounts | guid, name, account_type, commodity_guid, parent_guid |
| `transactions` | Transaction headers | guid, currency_guid, post_date, enter_date, description |
| `splits` | Transaction legs | guid, tx_guid, account_guid, value_num/denom, quantity_num/denom |
| `commodities` | Currencies & securities | guid, namespace, mnemonic, fullname, fraction |
| `prices` | Price history | guid, commodity_guid, currency_guid, date, value_num/denom |
| `slots` | Key-value metadata | obj_guid, name, slot_type, string_val, numeric_val_num/denom |
| `budgets` | Budget definitions | guid, name, description, num_periods |
| `budget_amounts` | Budget line items | budget_guid, account_guid, period_num, amount_num/denom |
| `schedxactions` | Scheduled transactions | guid, name, enabled, start_date, freq_type |
| `recurrences` | Recurrence rules | obj_guid, recurrence_mult, recurrence_period_type |
| `versions` | Schema version tracking | table_name, table_version |

### XML Format
- Root: `<gnc-v2>` with namespaces for trn, act, split, cmdty, ts, price, slot
- Gzip-compressed by default
- RELAX NG schema: `gnucash-v2.rnc`
- All entities identified by GUIDs

### Python Ecosystem
- **Native bindings**: SWIG-generated, requires GnuCash installation, supports XML + SQL
- **piecash2**: SQLAlchemy 2 ORM, SQL-only, pip-installable, actively maintained
- **Guile/Scheme**: Custom reports, `gnc:` namespace functions

### Existing GnuCash Agent Tools (CRITICAL FINDING)

**gnucash-mcp** (https://github.com/ninetails-io/gnucash-mcp):
- MCP server for GnuCash with **52 tools** across 7 modules
- Modules: core, reconciliation, reporting, budgets, scheduling, investments, admin
- Can be reduced to 15 core tools for simpler deployments
- Audit logging with timestamps
- This is the key bridge between AI agents and GnuCash
- **We should evaluate extending this rather than building from scratch**

**gnucash-rest** (https://github.com/loftx/gnucash-rest):
- Python REST API for GnuCash files

**gnucash-api** (https://github.com/jjuanda/gnucash-api):
- Alternative REST API

### Key Insight for Our Project
The SQLite backend is the ideal target for agent integration:
- Standard SQL access (no SWIG, no GnuCash installation needed)
- Atomic transactions (ACID)
- File-based (portable, git-friendly for backups)
- Schema is stable and well-documented via piecash's object model docs
- gnucash-mcp already provides an MCP interface we can build on

---

## 2. Agentic Financial Modeling (2025-2026)

### ruvnet Ecosystem (Reuven Cohen)
The most prolific open-source contributor in the agentic financial space:

| Project | Purpose | Key Feature |
|---------|---------|-------------|
| **Claude-Flow** | Agent orchestration | Master/worker, 10x perf vs single agent |
| **Agentic-Flow v2** | Multi-agent swarms | 66 self-learning agents, 213 MCP tools |
| **FACT** | Financial data retrieval | Sub-100ms, 90% cost reduction vs RAG |
| **Agentic Payments** | Autonomous transactions | Ed25519 signatures, Byzantine fault tolerance |
| **Neural Trader** | Algorithmic trading | 58+ MCP tools, NHITS/NBEATSx forecasting |
| **RuVector** | Vector DB | 150x perf, HNSW + SIMD, 16K queries/sec |
| **ReasoningBank** | Persistent agent memory | Bayesian updating, local SQLite storage |
| **AgentDB** | Agent data store | Vector search + graph DB combined |
| **rUv-dev** | SPARC methodology | Specification, Pseudocode, Architecture, Refinement, Code |
| **QuDAG** | Quantum-resistant infra | Post-quantum crypto for agent swarms |

### FACT Architecture (Most Relevant)
FACT replaces traditional RAG with prompt-caching + deterministic MCP tool execution:
- Cache hits: <50ms response time
- Cache misses: <140ms
- 85% faster than RAG for complex analytics
- 1000+ concurrent queries/minute
- No vector stores needed
- Audit trails built-in

### Key Patterns
1. **MCP as agent interface**: Standardized tool protocol for financial operations
2. **Persistent memory**: Agents learn from past interactions (ReasoningBank)
3. **Tiered authorization**: Auto/Review/Approve based on risk level
4. **Audit everything**: Every action logged with reasoning and confidence

### OpenClaw (formerly Clawdbot/Moltbot)
- **What**: Personal AI assistant, 215K+ GitHub stars, fastest-growing OSS repo in history
- **Creator**: Peter Steinberger (Austrian dev, joined OpenAI Feb 15, 2026)
- **Architecture**: Gateway control plane (Node.js) + Agent runtime loop + Skills ecosystem
- **Financial ops**: Portfolio monitoring, dollar-cost averaging, tax-loss harvesting, backtesting
- **Critical failure**: Massive security vulnerabilities -- 314 malicious skills published,
  infostealers harvesting passwords, crypto wallets, KeePass vaults
- **No RBAC, no approval workflows, no segregation of duties**
- **Status**: Transferred to independent foundation after Steinberger left
- URL: https://github.com/openclaw/openclaw

### IronClaw (NEAR AI)
- **What**: Security-first Rust reimplementation, direct response to OpenClaw's failures
- **Key differences from OpenClaw**:
  - **WASM sandboxing** (not Docker) -- capability-based permissions per tool
  - **Credential injection at host boundary** -- secrets never exposed to LLM
  - **Prompt injection defense** -- multi-layer detection, content sanitization
  - **PostgreSQL 15+ with pgvector** (not SQLite), AES-256-GCM encryption
  - **Audit trails designed for FINRA/SEC/SOX** from inception
  - **Multi-agent approval workflows** with human authorization gates
- **Status**: 671 stars, 85 forks (mid-Feb 2026)
- URL: https://github.com/nearai/ironclaw

### Key Lesson for gnucashr
OpenClaw's failure is a cautionary tale: financial agents without proper security
boundaries, audit trails, and approval workflows are actively dangerous. IronClaw's
architecture (WASM sandbox, capability-based permissions, credential injection,
approval gates) should inform our security sidecar design.

---

## 3. Monorepo Patterns

### Justfile as Orchestrator
- **Module system**: `mod <name> '<path>'` for per-directory justfiles
- **Parallel execution**: `just --parallel recipe1 recipe2`
- **Dotenv loading**: `set dotenv-load` for environment variables
- **Language-agnostic**: Works across R, C++, Dhall, Nix, Python
- **Discoverable**: `just --list` shows all recipes with descriptions

### Bazel + Nix Integration
- **rules_nixpkgs**: Bridge between Bazel and Nix toolchains
- **Nix for environment**: Dev shells with all dependencies
- **Bazel for builds**: Hermetic, cacheable C++ compilation
- **Coexistence pattern**: Nix manages the overall environment, Bazel manages individual build targets
- **R packages in Bazel**: No `rules_r` with bzlmod support; use Nix for R package builds

### Dhall for Configuration
- **Type safety**: Catches config errors at compile time, not runtime
- **No injection**: Dhall is a total language (no side effects, no network access)
- **Composable**: Import and merge configurations across files
- **JSON/YAML export**: `dhall-to-json`, `dhall-to-yaml` for tool consumption
- **Financial use case**: Tax tables, account mappings, authorization rules, vendor patterns

### Recommended Directory Structure
```
gnucashr/
  justfile                    # Root orchestrator
  flake.nix                   # Nix flake (all derivations)
  MODULE.bazel                # Bazel module definition
  .bazelrc                    # Bazel settings
  .envrc                      # direnv -> nix develop

  packages/
    gnucashr/                 # R package (CRAN-submittable subtree)
      justfile                # R-specific recipes
      DESCRIPTION
      R/ src/ tests/ man/ vignettes/ inst/
      BUILD.bazel             # Rcpp targets

  lib/
    gnucash-core/             # Standalone C++ library
      justfile
      include/                # Public headers
      src/                    # Implementation
      test/                   # C++ tests + fixtures
      BUILD.bazel

  agents/
    justfile                  # Agent recipes
    framework/                # Shared agent loop code
    spend-monitor/
    report-generator/
    transaction-categorizer/
    invoice-generator/
    tax-estimator/
    subscription-manager/
    bill-pay/

  dhall/
    package.dhall             # Root config
    types/                    # Type definitions
    agents/                   # Per-agent configs
    tax/                      # Tax tables
    templates/                # Account templates

  tools/
    sidecar/                  # Security sidecar
    identity/                 # Agent identity management

  docs/
    epic/                     # Planning (this directory)
```

### CRAN Package in Monorepo
- Keep `packages/gnucashr/` as a clean, self-contained R package directory
- `.Rbuildignore` excludes monorepo-level files
- `R CMD build packages/gnucashr` produces standard tarball
- CI runs R CMD check from within `packages/gnucashr/`
- Version in DESCRIPTION stays CRAN-compatible

---

## 4. Current Codebase State

### gnucashr Package (Detailed Audit)

**Scale**: 29 R files (10,605 lines), 6 C++ files (1,397 lines), 21 test files (4,692 lines)

**Key Classes**:
- `GnuCashDB` (R6) - Core database interface (SQLite + XML)
- `BookCollection` (R6) - Multi-book consolidation
- `LazyForecast` (R6) - Monadic lazy evaluation with AST
- `Budget`, `Commodity`, `Lot`, `Price`, `ScheduledTransaction` (R6)

**Unique Features**:
- Result monad (`ok()`/`err()`) - Rust-inspired error handling
- Writer monad (`logged()`) - Audit trail for operations
- Property-based testing with `hedgehog` (4 PBT test files)
- RcppParallel Worker pattern for Monte Carlo and scenarios

**Build Systems Present**:
- Nix flake (primary): 5 cacheable derivations, Attic cache
- Bazel (secondary): C++ compilation validation only
- renv (disabled): Present but bypassed by Nix

**CRAN Status**:
- Win-builder: PASS
- Mac-builder: PASS (0 errors, 0 warnings, 0 notes)
- Version 0.2.0.9000 (dev); needs 0.2.0 for submission
- Known issue: `Authors@R` needs `devtools::document()` before check

**Adjacent Repos**:
- **RemoteJuggler**: Chapel+Rust+Nix, has Justfile (262+ lines), same Attic cache
- **attic-cache**: Infrastructure for Nix binary cache

---

## 5. Agent Security & Identity

### Agent Authentication Patterns
- **Service accounts with scoped credentials**: Each agent gets unique API keys
- **OAuth 2.0 Device Flow**: For agents that need to authenticate to services
- **API key rotation**: Automated rotation via credential manager
- **GPG signing**: All agent-generated artifacts signed with agent-specific keys

### Sidecar Architecture for Agents
- Borrowed from Kubernetes sidecar pattern (Istio/Envoy)
- Sidecar intercepts all agent I/O to external systems
- Provides: auth checking, rate limiting, audit logging, anomaly detection
- Agent cannot bypass sidecar (architectural enforcement)

### Financial Authorization Tiers
| Tier | Operations | Approval | Latency |
|------|-----------|----------|---------|
| Auto | Read, categorize, report, calculate | None | Immediate |
| Review | Modify categories, create invoices, update budgets | Async human review | Minutes-hours |
| Approve | Pay bills, cancel subscriptions, transfer funds | Explicit human approval | Hours-days |

### SOX/GAAP Considerations
- All financial modifications must have audit trail
- Segregation of duties: agent that proposes != agent that approves
- Immutable audit log (append-only, tamper-evident)
- Periodic reconciliation: agent state vs GnuCash DB vs bank statements

### RemoteJuggler Integration Points
- Agent identity registry (which agent has which capabilities)
- Credential vault integration (KeePassXC via keyring)
- Git identity switching for agent commits
- GPG key management for agent signatures

---

## 6. SaaS/Compute Spend Monitoring

### Existing Tools & Patterns

| Tool | Domain | Key Feature |
|------|--------|-------------|
| **OpenCost** | Kubernetes costs | MCP server for AI agent queries |
| **Kubecost 3.0** | K8s cost management | ClickHouse backend, auto-rightsizing |
| **Finout** | K8s + SaaS costs | Virtual tagging, MegaBill unified view |
| **CAST AI** | K8s optimization | 50-90% cost reduction via automation |
| **Amnic** | FinOps OS | AI agents for anomaly detection, optimization |
| **CloudZero** | Unit economics | Cost per customer/transaction |
| **nOps** | AWS optimization | $3B managed spend, 50% autonomous reduction |

### OpenCost MCP Server (Most Relevant)
- Runs on port 8081
- Built into official Helm chart
- Tools: `get_allocation_costs`, `get_cloud_costs`, `get_asset_costs`
- Supports namespace/pod/node aggregation
- AI agents can query cost data via natural language
- Directly relevant to our compute spend monitoring agent

### Subscription Detection Patterns
1. **Recurring transaction detection**: Same vendor, similar amount, regular interval
2. **Vendor matching**: Pattern matching against known SaaS vendors
3. **Amount tracking**: Monthly burn rate calculation
4. **Usage correlation**: Link subscription cost to usage metrics (if available)

### Cloud Cost API Access
- **AWS**: Cost Explorer API, 18-month forecasting (Nov 2025)
- **GCP**: Cloud Billing API, BigQuery export
- **Azure**: Cost Management API, OpenCost integration in AKS
- **Modal**: Per-second billing, transparent pricing ($0.0000131/core/sec)
- **Vercel**: Spend management API, budget enforcement

### Tax Calculation Patterns
- Categorize expenses by tax schedule line item
- Quarterly estimated tax calculation
- Deductible expense tracking
- Multi-jurisdiction support (state, federal)
- Integration with tax table (Dhall-definable)

### Invoice Generation
- Template-based (Dhall templates -> HTML/PDF)
- Customer/vendor data from GnuCash
- Status tracking (draft, sent, paid, overdue)
- GnuCash has native invoice support (slots-based)

---

## References

### GnuCash
- GnuCash releases: https://github.com/Gnucash/gnucash/releases
- GnuCash XML format: https://wiki.gnucash.org/wiki/GnuCash_XML_format
- GnuCash Python bindings: https://wiki.gnucash.org/wiki/Python_Bindings
- piecash2: https://pypi.org/project/piecash2/
- piecash object model: https://piecash.readthedocs.io/en/stable/object_model.html

### Agentic Financial
- ruvnet GitHub: https://github.com/ruvnet
- Claude-Flow: https://github.com/ruvnet/claude-flow
- FACT: https://github.com/ruvnet/FACT
- Agentic-Flow: https://github.com/ruvnet/agentic-flow
- Agentic Payments: https://ruv.io/agentic-payments

### GnuCash Agent Tools
- gnucash-mcp: https://github.com/ninetails-io/gnucash-mcp (52 tools, 7 modules)
- gnucash-rest: https://github.com/loftx/gnucash-rest
- piecash: https://github.com/sdementen/piecash

### SaaS/Subscription/Cost APIs
- Plaid (bank data, recurring txn detection): https://plaid.com/docs/
- Dwolla (ACH payments): https://developers.dwolla.com
- Chargebee (subscription management): https://apidocs.chargebee.com
- Avalara (tax calculation): https://developer.avalara.com
- Stripe Tax: https://docs.stripe.com/tax
- OpenFisca (open-source tax rules): https://github.com/openfisca/openfisca-core

### SaaS Spend Monitoring
- Zylo: https://zylo.com (Gartner Leader, $2T+ spend analyzed)
- Torii: https://www.toriihq.com (real-time SaaS discovery)
- Cledara: https://www.cledara.com (payment-integrated, one-click cancel)
- Vendr: https://www.vendr.com (AI negotiation agents)

### Infrastructure
- OpenCost: https://opencost.io (CNCF, has MCP server)
- Kubecost 3.0: https://www.kubecost.com (ClickHouse backend)
- Modal pricing: https://modal.com/pricing
- Justfile: https://github.com/casey/just
- Dhall: https://dhall-lang.org
- Bazel: https://bazel.build

### Open-Source Financial Agent Frameworks
- CrewAI: https://github.com/crewAIInc/crewAI (multi-agent orchestration)
- FinGPT: https://github.com/AI4Finance-Foundation/FinGPT (financial LLM)
- FinRobot: https://github.com/AI4Finance-Foundation/FinRobot (agent platform)
- FinRL: https://github.com/AI4Finance-Foundation/FinRL (RL for trading)

### FinOps Standards
- FOCUS 1.3: FinOps Open Cost and Usage Specification (Dec 2025)
- FinOps Foundation: https://www.finops.org
