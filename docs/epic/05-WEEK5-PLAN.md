# Week 5: MCP Server + Audit Trail - Detailed Implementation Plan

**Goal**: Build production-grade MCP server in C++ on gnucash-core, add Aperture-ready audit trail, prepare for agent development

**Success Criteria**:
1. Claude Code can query GnuCash book via MCP
2. All 19 gnucash-bridge methods exposed as MCP tools
3. Audit trail captures all operations (Aperture-compatible architecture)
4. Dhall configs control tool availability per agent persona
5. Test suite validates MCP protocol compliance
6. Ready for Week 6+ agent development (multi-week phase)

---

## Pre-Week 5: Repository Setup

### Task 0.1: Import gnucash-mcp as Reference Subtree

**Objective**: Bring in ninetails-io/gnucash-mcp as a reference implementation (not active code)

**Actions**:
```bash
# Add as git subtree in docs/reference/
git subtree add --prefix=docs/reference/gnucash-mcp \
  https://github.com/Jesssullivan/gnucash-mcp main --squash

# Document in CLAUDE.md
echo "Python reference implementation at docs/reference/gnucash-mcp/" >> CLAUDE.md
```

**Rationale**: Preserve audit logging patterns, tool definitions, multi-currency spec as design reference. Won't be compiled or run—pure documentation.

**Deliverable**: `docs/reference/gnucash-mcp/` directory with README explaining its role

**Estimated Time**: 30 minutes

---

## Phase 5.1: JSON-RPC 2.0 Protocol Layer (Days 1-2)

### Task 5.1.1: Design MCP Protocol Wrapper

**Objective**: Extend gnucash-bridge with JSON-RPC 2.0 + MCP lifecycle methods

**Current State**: `gnucash-bridge` uses custom JSON-lines protocol:
```json
{"method": "open", "params": {"path": "..."}, "id": 1}
→ {"result": {...}, "id": 1}
```

**Target State**: MCP-compliant JSON-RPC 2.0:
```json
{"jsonrpc": "2.0", "method": "tools/call", "params": {"name": "gnucash_open", "arguments": {...}}, "id": 1}
→ {"jsonrpc": "2.0", "result": {...}, "id": 1}
```

**Design Decisions**:
- **Protocol detection**: First line from stdin determines mode (MCP vs legacy)
  - MCP: `{"jsonrpc": "2.0", "method": "initialize", ...}`
  - Legacy: `{"method": "open", ...}` (backward compat)
- **Dual dispatch**: Route to `mcp_dispatch()` or `legacy_dispatch()` based on protocol
- **MCP methods**: `initialize`, `initialized`, `tools/list`, `tools/call`, `ping`, `shutdown`

**Files**:
- `lib/gnucash-core/src/mcp_protocol.h` - MCP types (ToolDefinition, ServerInfo, etc.)
- `lib/gnucash-core/src/mcp_protocol.cpp` - MCP lifecycle handlers
- `lib/gnucash-core/src/json_api.cpp` - Add protocol detection + dual dispatch

**Deliverable**: gnucash-bridge responds to MCP `initialize` handshake

**Estimated Time**: 8 hours (1 day)

---

### Task 5.1.2: Implement MCP Tool Schema

**Objective**: Map all 19 gnucash-bridge methods to MCP tool definitions

**MCP Tool Structure**:
```json
{
  "name": "gnucash_get_accounts",
  "description": "List all accounts with optional filters",
  "inputSchema": {
    "type": "object",
    "properties": {
      "account_type": {"type": "string", "enum": ["ASSET", "LIABILITY", ...]},
      "hidden": {"type": "boolean"}
    }
  }
}
```

**Tool Mapping** (19 tools):
1. `gnucash_open` — Open book (required first call)
2. `gnucash_close` — Close book
3. `gnucash_info` — Book metadata
4. `gnucash_list_accounts` — All accounts
5. `gnucash_account_tree` — Hierarchical account list
6. `gnucash_get_account` — Single account by GUID
7. `gnucash_get_account_by_path` — Lookup by path string
8. `gnucash_get_transactions` — Query transactions (date range)
9. `gnucash_get_balance` — Account balance as of date
10. `gnucash_trial_balance` — All account balances
11. `gnucash_get_commodities` — Currency/security list
12. `gnucash_get_prices` — Price database
13. `gnucash_create_account` — Create new account (write)
14. `gnucash_post_transaction` — Create transaction (write)
15. `gnucash_delete_transaction` — Remove transaction (write)
16. `gnucash_void_transaction` — Void with reversal (write)
17. `gnucash_parse_ofx` — Parse OFX file content
18. `gnucash_get_splits` — Splits for account
19. `gnucash_get_transaction` — Single transaction by GUID

**Tool Classification** (for Dhall configs):
- **Read** (tier: Auto): 1-12, 17-19
- **Write** (tier: Approve): 13-16

**Files**:
- `lib/gnucash-core/src/mcp_tools.cpp` — Tool schema definitions
- `lib/gnucash-core/test/test_mcp_protocol.cpp` — MCP handshake tests

**Deliverable**: `tools/list` returns all 19 tool schemas

**Estimated Time**: 6 hours

---

### Task 5.1.3: Implement MCP tools/call Handler

**Objective**: Route `tools/call` requests to gnucash-bridge dispatch

**Flow**:
```
Claude Code
  ↓ {"jsonrpc": "2.0", "method": "tools/call", "params": {"name": "gnucash_get_accounts", "arguments": {}}}
gnucash-bridge (MCP layer)
  ↓ Extract tool name, validate schema, transform to legacy JSON
  ↓ {"method": "get_accounts", "params": {}, "id": 1}
gnucash-bridge (legacy dispatch)
  ↓ Execute via existing json_api.cpp handlers
  ↓ {"result": [...], "id": 1}
MCP layer
  ↓ Wrap in JSON-RPC envelope
  ↓ {"jsonrpc": "2.0", "result": {"content": [{"type": "text", "text": "..."}]}, "id": 1}
Claude Code
```

**Error Handling**:
- Unknown tool → `{"error": {"code": -32601, "message": "Method not found"}}`
- Invalid arguments → `{"error": {"code": -32602, "message": "Invalid params"}}`
- Internal error → `{"error": {"code": -32603, "message": "Internal error"}}`

**Files**:
- `lib/gnucash-core/src/mcp_protocol.cpp` — `handle_tools_call()`
- `lib/gnucash-core/test/test_mcp_tools.cpp` — Tool call tests

**Deliverable**: All 19 tools callable via MCP protocol

**Estimated Time**: 6 hours

---

### Task 5.1.4: MCP Client Test Suite

**Objective**: Validate MCP protocol compliance with automated tests

**Test Coverage**:
1. **Lifecycle**: initialize → tools/list → tools/call → shutdown
2. **Tool execution**: Call each of 19 tools with valid/invalid params
3. **Error handling**: Unknown method, invalid JSON, internal errors
4. **Concurrent calls**: Multiple tools/call with same `id` collision detection
5. **Book state**: Tools fail gracefully when book not open

**Test Harness**:
- C++ tests using Catch2 (extend `test/test_json_api.cpp`)
- Subprocess tests: spawn gnucash-bridge, drive via stdin/stdout
- Fixture books: minimal.gnucash, with-accounts.gnucash

**Files**:
- `lib/gnucash-core/test/test_mcp_client.cpp` — Full protocol test
- `lib/gnucash-core/test/mcp_test_client.h` — Helper class for MCP I/O

**Deliverable**: 25+ tests for MCP protocol compliance

**Estimated Time**: 8 hours (1 day)

---

## Phase 5.2: Audit Trail System (Days 3-4)

### Task 5.2.1: Audit Schema Design (Aperture-Compatible)

**Objective**: Design audit log that can integrate with Tailscale Aperture for policy enforcement

**Aperture Context**:
- Tailscale Aperture provides audit logging and policy enforcement for identity-aware proxies
- Future integration: gnucash-mcp sits behind Aperture, all requests authenticated
- Audit log format must include: identity, resource, operation, before/after state, decision

**Audit Schema** (SQLite table):
```sql
CREATE TABLE audit_log (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    timestamp TEXT NOT NULL,              -- ISO 8601 UTC

    -- Identity (Aperture-compatible)
    user_email TEXT,                      -- Tailscale identity email
    user_name TEXT,                       -- Display name
    node_name TEXT,                       -- Tailscale node hostname

    -- Operation
    tool_name TEXT NOT NULL,              -- MCP tool name
    classification TEXT NOT NULL,         -- "read" | "write"
    operation TEXT,                       -- "create" | "update" | "delete" | "void"
    entity_type TEXT,                     -- "transaction" | "account" | "split"

    -- Request
    request_id TEXT,                      -- MCP call id
    arguments_json TEXT,                  -- JSON serialized params

    -- Resource
    book_path TEXT NOT NULL,              -- GnuCash file path
    entity_guid TEXT,                     -- Affected entity GUID

    -- State
    before_state_json TEXT,               -- Snapshot before operation
    after_state_json TEXT,                -- Snapshot after operation

    -- Result
    result_status TEXT NOT NULL,          -- "success" | "error"
    error_message TEXT,                   -- If failed

    -- Authorization (future)
    authorization_level TEXT,             -- "auto" | "approve" | "review"
    approval_guid TEXT,                   -- Reference to approval record

    -- Metadata
    duration_ms INTEGER,                  -- Execution time
    reasoning TEXT                        -- LLM reasoning (for categorization, etc.)
);

CREATE INDEX idx_audit_timestamp ON audit_log(timestamp);
CREATE INDEX idx_audit_tool ON audit_log(tool_name);
CREATE INDEX idx_audit_user ON audit_log(user_email);
CREATE INDEX idx_audit_entity ON audit_log(entity_guid);
```

**Design Decisions**:
- **Separate SQLite database**: `{book_path}.audit.db` (not in GnuCash book)
- **Append-only**: No updates/deletes to audit records
- **Identity fields nullable**: Populated when running behind Aperture, NULL for local dev
- **JSON state capture**: Store full before/after as JSON for diff tooling
- **Reasoning field**: For agent actions, stores LLM explanation

**Files**:
- `lib/gnucash-core/include/gnucash/audit.h` — Audit API
- `lib/gnucash-core/src/audit.cpp` — SQLite audit implementation
- `lib/gnucash-core/test/fixtures/audit-test.sql` — Schema for tests

**Deliverable**: Audit database schema + C++ API

**Estimated Time**: 6 hours

---

### Task 5.2.2: Audit Logger Implementation

**Objective**: Capture all MCP tool calls in audit trail

**Audit Flow**:
```
MCP tools/call received
  ↓
Extract: tool_name, arguments, request_id
  ↓
[Before snapshot] — Query current entity state (if write operation)
  ↓
Execute tool via dispatch
  ↓
[After snapshot] — Query new entity state (if write operation)
  ↓
Write audit record: timestamp, tool, before/after, result, duration
  ↓
Return result to client
```

**Snapshot Strategy**:
- **Transactions**: Full splits, amounts, accounts
- **Accounts**: Name, type, parent, balance
- **Splits**: Account, value, quantity, reconcile state

**Performance**: Async writes to audit DB (non-blocking)

**Files**:
- `lib/gnucash-core/src/audit.cpp` — `log_tool_call()`, `capture_snapshot()`
- `lib/gnucash-core/src/mcp_protocol.cpp` — Inject audit logging into `handle_tools_call()`

**Deliverable**: All tool calls logged with before/after state

**Estimated Time**: 8 hours (1 day)

---

### Task 5.2.3: Audit Query Tool

**Objective**: Expose audit log via MCP tool for introspection

**Tool Definition**:
```json
{
  "name": "gnucash_audit_log",
  "description": "Query audit trail with filters",
  "inputSchema": {
    "type": "object",
    "properties": {
      "since": {"type": "string", "format": "date-time"},
      "until": {"type": "string", "format": "date-time"},
      "tool_name": {"type": "string"},
      "classification": {"type": "string", "enum": ["read", "write"]},
      "user_email": {"type": "string"},
      "entity_guid": {"type": "string"},
      "limit": {"type": "integer", "default": 100}
    }
  }
}
```

**Output Format** (compact text):
```
2026-02-25 14:32:15  [WRITE]  gnucash_post_transaction
  user: jess@example.com
  entity: a1b2c3d4... (transaction)
  Safeway grocery run
    Expenses:Groceries      47.50
    Liabilities:Visa       -47.50
  duration: 12ms
```

**Files**:
- `lib/gnucash-core/src/audit.cpp` — `query_audit_log()`
- `lib/gnucash-core/src/mcp_tools.cpp` — Register `gnucash_audit_log` tool
- `lib/gnucash-core/test/test_audit.cpp` — Audit query tests

**Deliverable**: Queryable audit trail via MCP

**Estimated Time**: 4 hours

---

### Task 5.2.4: Audit Export (Aperture Integration Prep)

**Objective**: Export audit log in Aperture-compatible format

**Aperture Log Format** (JSONL):
```json
{"timestamp":"2026-02-25T14:32:15Z","user":"jess@example.com","node":"macbook.tail1234.ts.net","resource":"gnucash://book.gnucash/transaction/a1b2c3d4","action":"create","decision":"allow","metadata":{"tool":"gnucash_post_transaction","duration_ms":12}}
```

**Export Tool**:
```bash
gnucash-bridge audit-export --book /path/to/book.gnucash \
  --since 2026-02-01 --format aperture > audit.jsonl
```

**Files**:
- `lib/gnucash-core/src/audit_export.cpp` — Aperture format converter
- `lib/gnucash-core/src/main.cpp` — Add `audit-export` subcommand

**Deliverable**: Audit logs exportable to Aperture format

**Estimated Time**: 4 hours

---

## Phase 5.3: Dhall Agent Configurations (Day 5)

### Task 5.3.1: Agent Persona Dhall Types

**Objective**: Define Dhall types for agent configuration

**Types** (`dhall/types/Agent.dhall`):
```dhall
let Agent =
      { Type =
          { name : Text
          , description : Text
          , tools : List Text                  -- Tool names agent can use
          , authorization_level : ../AuthorizationLevel.dhall
          , vendor_patterns : List ../VendorPattern.dhall
          , schedule : Optional Schedule
          }
      , default =
          { schedule = None Schedule
          , authorization_level = ../AuthorizationLevel.dhall.Auto
          , vendor_patterns = [] : List ../VendorPattern.dhall
          }
      }

let Schedule =
      < Hourly : Natural        -- Every N hours
      | Daily : Natural         -- Every N days at midnight
      | OnDemand                -- Triggered manually
      >

let VendorPattern =
      { pattern : Text          -- Regex or substring
      , category : Text         -- Account path
      , confidence : Double     -- 0.0-1.0
      }

in { Agent, Schedule, VendorPattern }
```

**Files**:
- `dhall/types/Agent.dhall` — Agent persona type
- `dhall/types/VendorPattern.dhall` — Categorization rule
- `dhall/types.dhall` — Re-export Agent types

**Deliverable**: Type-safe agent config schema

**Estimated Time**: 3 hours

---

### Task 5.3.2: Example Agent Configurations

**Objective**: Create Dhall configs for the three Week 6 agents

**spend-monitor** (`dhall/agents/spend-monitor.dhall`):
```dhall
let Agent = ../types/Agent.dhall

in Agent::{
  , name = "spend-monitor"
  , description = "Monitor and categorize SaaS/compute spending"
  , tools =
    [ "gnucash_list_accounts"
    , "gnucash_get_transactions"
    , "gnucash_get_balance"
    , "gnucash_post_transaction"      -- For categorization updates
    , "gnucash_audit_log"
    ]
  , authorization_level = ../types/AuthorizationLevel.dhall.Auto
  , vendor_patterns =
    [ { pattern = "MODAL LABS", category = "Expenses:SaaS:Compute", confidence = 0.99 }
    , { pattern = "VERCEL", category = "Expenses:SaaS:Hosting", confidence = 0.95 }
    , { pattern = "GITHUB", category = "Expenses:SaaS:DevTools", confidence = 0.98 }
    , { pattern = "ANTHROPIC", category = "Expenses:SaaS:AI", confidence = 0.99 }
    , { pattern = "AWS", category = "Expenses:Cloud:AWS", confidence = 0.90 }
    ]
  , schedule = Some (Agent.Schedule.Hourly 1)
  }
```

**report-generator** (`dhall/agents/report-generator.dhall`):
```dhall
let Agent = ../types/Agent.dhall

in Agent::{
  , name = "report-generator"
  , description = "Generate financial reports on schedule"
  , tools =
    [ "gnucash_list_accounts"
    , "gnucash_get_transactions"
    , "gnucash_trial_balance"
    , "gnucash_get_balance"
    , "gnucash_get_commodities"
    ]
  , authorization_level = ../types/AuthorizationLevel.dhall.Auto
  , schedule = Some (Agent.Schedule.Daily 1)
  }
```

**transaction-categorizer** (`dhall/agents/transaction-categorizer.dhall`):
```dhall
let Agent = ../types/Agent.dhall

in Agent::{
  , name = "transaction-categorizer"
  , description = "Auto-categorize transactions with LLM + rules"
  , tools =
    [ "gnucash_get_transactions"
    , "gnucash_post_transaction"      -- Update transaction categories
    , "gnucash_audit_log"
    ]
  , authorization_level = ../types/AuthorizationLevel.dhall.Review  -- Human review for low confidence
  , vendor_patterns = ./spend-monitor.dhall.vendor_patterns         -- Reuse patterns
  , schedule = Some Agent.Schedule.OnDemand
  }
```

**Files**:
- `dhall/agents/spend-monitor.dhall`
- `dhall/agents/report-generator.dhall`
- `dhall/agents/transaction-categorizer.dhall`

**Deliverable**: 3 agent configs, all type-check

**Estimated Time**: 2 hours

---

### Task 5.3.3: Tool Filtering via Dhall Config

**Objective**: gnucash-bridge only advertises tools specified in agent config

**Flow**:
```bash
# Launch bridge with agent config
gnucash-bridge mcp --book book.gnucash --agent dhall/agents/spend-monitor.dhall

# MCP tools/list only returns 5 tools (not all 19)
["gnucash_list_accounts", "gnucash_get_transactions", "gnucash_get_balance",
 "gnucash_post_transaction", "gnucash_audit_log"]
```

**Implementation**:
- Parse Dhall config on startup
- Filter `TOOL_DEFINITIONS` array based on `agent.tools` list
- Return filtered set in `tools/list` response

**Files**:
- `lib/gnucash-core/src/dhall_config.cpp` — Dhall parser (call `dhall` CLI)
- `lib/gnucash-core/src/mcp_protocol.cpp` — Apply filter in `handle_tools_list()`
- `lib/gnucash-core/test/test_dhall_filter.cpp` — Tool filtering tests

**Deliverable**: Tool filtering controlled by Dhall config

**Estimated Time**: 4 hours

---

## Phase 5.4: Testing & Documentation (Day 6)

### Task 5.4.1: Integration Test with Claude Code

**Objective**: Verify MCP server works end-to-end in Claude Code

**Setup**:
1. Add to `~/.config/claude/config.json`:
```json
{
  "mcpServers": {
    "gnucash": {
      "command": "/home/jsullivan2/git/gnucashr/lib/gnucash-core/build/gnucash-bridge",
      "args": ["mcp", "--book", "/path/to/test.gnucash"],
      "env": {}
    }
  }
}
```

2. Launch Claude Code
3. Test queries:
   - "Show me all my accounts"
   - "What did I spend on groceries last month?"
   - "Create a test transaction for $10 at Safeway"
   - "Show me the audit log for today"

**Success Criteria**:
- All 4 queries return correct data
- Audit log captures all operations
- No protocol errors in logs

**Files**:
- `docs/MCP_TESTING.md` — Testing guide
- `scripts/test-mcp-live.sh` — Automated Claude Code test

**Deliverable**: Working MCP integration with Claude Code

**Estimated Time**: 3 hours

---

### Task 5.4.2: Update Justfile with MCP Recipes

**Objective**: Add convenience commands for MCP development

**Recipes**:
```just
# Build MCP server
mcp-build:
  nix develop --command bash -c "cd lib/gnucash-core && mkdir -p build && cd build && cmake .. && cmake --build . -j$(nproc)"

# Run MCP server in test mode
mcp-test book:
  lib/gnucash-core/build/gnucash-bridge mcp --book {{book}} --debug

# Run MCP protocol tests
mcp-test-suite:
  nix develop --command bash -c "cd lib/gnucash-core/build && ctest -R mcp"

# Export audit log
mcp-audit book since="7 days ago":
  lib/gnucash-core/build/gnucash-bridge audit-export --book {{book}} --since "{{since}}"

# Type-check Dhall agent configs
dhall-agents:
  dhall type --file dhall/agents/spend-monitor.dhall
  dhall type --file dhall/agents/report-generator.dhall
  dhall type --file dhall/agents/transaction-categorizer.dhall
```

**Files**:
- `justfile` — Add mcp-* recipes

**Deliverable**: Streamlined MCP development workflow

**Estimated Time**: 1 hour

---

### Task 5.4.3: Week 5 Documentation

**Objective**: Document MCP architecture for Week 6+ agent work

**Documents**:
1. **MCP_ARCHITECTURE.md**:
   - Protocol flow diagrams
   - Tool catalog (19 tools)
   - Audit trail schema
   - Dhall config system
   - Aperture integration design

2. **AGENT_DEVELOPMENT_GUIDE.md**:
   - How to create a new agent config
   - Tool selection best practices
   - Authorization tier guidelines
   - Vendor pattern syntax
   - Testing agent configs

3. **AUDIT_TRAIL_SPEC.md**:
   - Database schema
   - Query examples
   - Aperture export format
   - Privacy considerations

**Files**:
- `docs/MCP_ARCHITECTURE.md`
- `docs/AGENT_DEVELOPMENT_GUIDE.md`
- `docs/AUDIT_TRAIL_SPEC.md`

**Deliverable**: Comprehensive docs for Week 6 work

**Estimated Time**: 4 hours

---

### Task 5.4.4: Update PROGRESS.md

**Objective**: Mark Week 5 complete, update metrics

**Updates**:
- Phase 3 Week 5: All tasks marked [x]
- Gate G3 partial: "MCP server responds to Claude Code" ✓
- Metrics: Test count (C++): 91 → ~120 (add MCP + audit tests)
- Decision log: MCP C++ implementation, Aperture-compatible audit

**Files**:
- `docs/epic/PROGRESS.md`

**Deliverable**: Updated progress tracker

**Estimated Time**: 30 minutes

---

## Phase 5.5: Nix Integration (Day 7)

### Task 5.5.1: Add Audit DB Dependency to Nix

**Objective**: Ensure audit.cpp can link against SQLite

**Changes**:
- `flake.nix`: Audit DB already uses same sqlite3 as gnucash-core (no new deps)
- CMakeLists.txt: Link `audit.cpp` into gnucash-core library

**Files**:
- `lib/gnucash-core/CMakeLists.txt` — Add audit.cpp to sources

**Deliverable**: Nix build includes audit system

**Estimated Time**: 1 hour

---

### Task 5.5.2: Nix Derivation for gnucash-bridge MCP Mode

**Objective**: Package MCP server for deployment

**Derivation** (`packages.gnucashMcp`):
```nix
gnucashMcp = pkgs.stdenv.mkDerivation {
  pname = "gnucash-mcp";
  version = "0.3.0";
  src = ./lib/gnucash-core;

  nativeBuildInputs = [ pkgs.cmake pkgs.pkg-config ];
  buildInputs = [ pkgs.sqlite pkgs.nlohmann_json ];

  buildPhase = ''
    cmake -DCMAKE_BUILD_TYPE=Release ..
    cmake --build . -j$NIX_BUILD_CORES
  '';

  installPhase = ''
    mkdir -p $out/bin
    cp gnucash-bridge $out/bin/
  '';

  meta = {
    description = "MCP server for GnuCash";
    license = pkgs.lib.licenses.mit;
  };
};
```

**Files**:
- `flake.nix` — Add `packages.gnucashMcp`

**Deliverable**: `nix build .#gnucashMcp` produces MCP server binary

**Estimated Time**: 2 hours

---

## Week 5 Summary

**Total Estimated Time**: 6-7 days (with buffer)

**Deliverables**:
1. ✅ gnucash-bridge extended with JSON-RPC 2.0 + MCP protocol
2. ✅ 19 MCP tools exposed (all gnucash-bridge methods)
3. ✅ Audit trail system (Aperture-compatible schema)
4. ✅ Dhall agent configs (3 example agents)
5. ✅ Tool filtering via Dhall
6. ✅ MCP test suite (120+ C++ tests)
7. ✅ Claude Code integration verified
8. ✅ Comprehensive documentation

**Gate G3 Progress**:
- [x] MCP server responds to Claude Code queries
- [ ] spend-monitor produces useful report (Week 6)
- [x] All agent actions in audit trail
- [ ] Agent tests pass (Week 6)

**Ready for Week 6+**: Agent development (multi-week phase)

---

## Risks & Mitigations

| Risk | Mitigation |
|------|-----------|
| JSON-RPC 2.0 spec complexity | Reference gnucash-mcp Python implementation, use nlohmann_json validation |
| Audit DB performance bottleneck | Async writes, batching, indexes on timestamp/tool/user |
| Dhall parsing overhead | Cache parsed configs, only re-parse on change |
| Claude Code MCP debugging | Add `--debug` flag with verbose protocol logging |
| Aperture integration unknowns | Design for future integration, don't block Week 5 on it |

---

## Post-Week 5: Transition to Week 6+

Week 6+ will be a **multi-week agent development phase** building on this foundation:
- spend-monitor agent (categorization, anomaly detection)
- report-generator agent (financial reports)
- transaction-categorizer agent (LLM-powered categorization)
- Agent testing framework
- Iterative improvements based on real-world usage

**Week 5 Success = Ready for Autonomous Agent Development**
