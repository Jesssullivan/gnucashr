# MCP Architecture - gnucashr Week 5

**Version**: 0.3.0
**Status**: Complete (Week 5)
**Date**: 2026-02-25

---

## Overview

gnucashr's MCP (Model Context Protocol) server enables Claude Code to interact with GnuCash books through a standards-compliant JSON-RPC 2.0 interface. The server provides 20 tools for financial data operations, comprehensive audit logging, and agent-based tool filtering.

## Architecture Diagram

```
┌─────────────────────────────────────────────────────────────────┐
│                         Claude Code                              │
│                    (or other MCP client)                         │
└────────────────────────┬────────────────────────────────────────┘
                         │ JSON-RPC 2.0 over stdin/stdout
                         │
┌────────────────────────▼────────────────────────────────────────┐
│                   gnucash-bridge (MCP Server)                    │
├──────────────────────────────────────────────────────────────────┤
│  Protocol Layer (mcp.cpp)                                        │
│  ├─ Protocol detection (MCP vs legacy)                           │
│  ├─ JSON-RPC 2.0 dispatch                                        │
│  ├─ Tool registry (20 tools)                                     │
│  └─ Agent config filtering (Dhall-based)                         │
├──────────────────────────────────────────────────────────────────┤
│  Audit Layer (audit.cpp)                                         │
│  ├─ SQLite audit database: {book}.audit.db                       │
│  ├─ Before/after state capture                                   │
│  ├─ Duration timing                                              │
│  └─ Aperture-compatible schema                                   │
├──────────────────────────────────────────────────────────────────┤
│  Core Operations (json_api.cpp, book.cpp)                        │
│  ├─ Book management (open, close, info)                          │
│  ├─ Account operations (get, tree, balance)                      │
│  ├─ Transaction operations (get, post, delete, void)             │
│  ├─ Commodity & price queries                                    │
│  └─ OFX parsing                                                  │
└────────────────────────┬────────────────────────────────────────┘
                         │ SQLite API
                         │
┌────────────────────────▼────────────────────────────────────────┐
│                  GnuCash SQLite Database                         │
│                    (book.gnucash)                                │
└──────────────────────────────────────────────────────────────────┘
```

---

## Components

### 1. MCP Protocol Layer (`lib/gnucash-core/src/mcp.cpp`)

**Responsibilities**:
- JSON-RPC 2.0 request/response handling
- MCP lifecycle: `initialize` → `tools/list` → `tools/call`
- Protocol auto-detection (MCP vs legacy JSON-lines)
- Tool registration and filtering
- Agent configuration management

**Key Functions**:
- `run_mcp_loop()` - Main stdin/stdout event loop
- `mcp_dispatch()` - Route MCP method calls
- `handle_tools_list()` - Return available tools (filtered by agent)
- `handle_tools_call()` - Execute tool and audit
- `get_tool_definitions()` - Tool registry with filtering

**Global State**:
```cpp
static std::optional<audit::AuditLogger> g_audit_logger;
static std::string g_book_path;
static std::optional<dhall::AgentConfig> g_agent_config;
```

### 2. Audit Trail System (`lib/gnucash-core/src/audit.cpp`)

**Responsibilities**:
- Append-only audit log in separate SQLite database
- Before/after state capture for write operations
- Aperture-compatible schema for future Tailscale integration
- Query interface with filters (time, tool, user, entity)
- Aperture JSONL export

**Schema**:
```sql
CREATE TABLE audit_log (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    timestamp TEXT NOT NULL,

    -- Identity (Aperture fields, nullable for local)
    user_email TEXT,
    user_name TEXT,
    node_name TEXT,

    -- Operation
    tool_name TEXT NOT NULL,
    classification TEXT NOT NULL,  -- "read" | "write"
    operation TEXT,                 -- "create" | "update" | "delete" | "void"
    entity_type TEXT,               -- "transaction" | "account" | ...

    -- Request/Resource
    request_id TEXT,
    arguments_json TEXT,
    book_path TEXT NOT NULL,
    entity_guid TEXT,

    -- State
    before_state_json TEXT,
    after_state_json TEXT,

    -- Result
    result_status TEXT NOT NULL,   -- "success" | "error"
    error_message TEXT,

    -- Metadata
    duration_ms INTEGER,
    reasoning TEXT
);
```

**Indexes**: timestamp, tool_name, user_email, entity_guid, classification

### 3. Dhall Configuration System (`lib/gnucash-core/src/dhall_config.cpp`)

**Responsibilities**:
- Parse agent configurations from Dhall files
- Extract tool allow-lists
- Call `dhall-to-json` CLI for parsing

**Agent Config Structure**:
```dhall
{ name : Text
, description : Text
, tools : List Text                    -- Tool names agent can access
, authorization_level : AuthorizationLevel
, vendor_patterns : List VendorPattern
, schedule : Optional Schedule
}
```

**Example Agents**:
1. **spend-monitor**: 6 tools, 10 vendor patterns, hourly
2. **report-generator**: 8 tools, daily
3. **transaction-categorizer**: 6 tools, on-demand, review tier

### 4. Core Operations (`lib/gnucash-core/src/book.cpp`)

**Responsibilities**:
- GnuCash SQLite schema access
- CRUD operations on accounts, transactions, splits
- Balance calculations and trial balance
- OFX parsing

**Key Classes**:
- `Book` - RAII SQLite connection manager
- `Result<T,E>` - Monadic error handling
- `Fraction` - Rational number arithmetic (num/denom)

---

## MCP Tool Catalog

### Read Operations (Auto Tier) - 15 tools

| Tool | Description | Params |
|------|-------------|--------|
| `gnucash_open` | Open a GnuCash book | `{path, read_only}` |
| `gnucash_close` | Close current book | `{}` |
| `gnucash_info` | Book metadata | `{}` |
| `gnucash_get_accounts` | List all accounts | `{}` |
| `gnucash_account_tree` | Hierarchical account tree | `{}` |
| `gnucash_get_account` | Get account by GUID | `{guid}` |
| `gnucash_get_account_by_path` | Lookup by path string | `{path}` |
| `gnucash_get_transactions` | Query transactions | `{from_date?, to_date?}` |
| `gnucash_get_transaction` | Get transaction by GUID | `{guid}` |
| `gnucash_get_splits` | Splits for account | `{account_guid}` |
| `gnucash_get_balance` | Account balance as of date | `{account_guid, as_of?}` |
| `gnucash_trial_balance` | All account balances | `{as_of?}` |
| `gnucash_get_commodities` | List currencies/securities | `{}` |
| `gnucash_get_prices` | Price database | `{}` |
| `gnucash_parse_ofx` | Parse OFX file content | `{content}` |

### Write Operations (Approve Tier) - 4 tools

| Tool | Description | Params |
|------|-------------|--------|
| `gnucash_create_account` | Create new account | `{name, type, parent_guid, ...}` |
| `gnucash_post_transaction` | Create transaction | `{description, post_date, splits, ...}` |
| `gnucash_delete_transaction` | Remove transaction | `{guid}` |
| `gnucash_void_transaction` | Void with reversal | `{guid, reason}` |

### Audit Tool - 1 tool

| Tool | Description | Params |
|------|-------------|--------|
| `gnucash_audit_log` | Query audit trail | `{since?, until?, tool_name?, ...}` |

---

## Protocol Flow

### 1. Initialization Handshake

```json
→ {"jsonrpc":"2.0","method":"initialize","params":{"protocolVersion":"2024-11-05"},"id":1}
← {"jsonrpc":"2.0","result":{"protocolVersion":"2024-11-05","capabilities":{"tools":true},"serverInfo":{"name":"gnucash-mcp","version":"0.3.0"}},"id":1}

→ {"jsonrpc":"2.0","method":"initialized"}
← (no response - notification)
```

### 2. Tool Discovery

```json
→ {"jsonrpc":"2.0","method":"tools/list","id":2}
← {"jsonrpc":"2.0","result":{"tools":[{name,description,inputSchema},...]},"id":2}
```

### 3. Tool Invocation (with Audit)

```json
→ {"jsonrpc":"2.0","method":"tools/call","params":{"name":"gnucash_get_accounts","arguments":{}},"id":3}
```

**Server-side processing**:
1. Start timer
2. Build audit record (timestamp, tool_name, arguments)
3. Classify operation (read vs write)
4. Dispatch to legacy handler (`json_api::dispatch()`)
5. Capture result or error
6. Calculate duration
7. Write to audit database
8. Return MCP response

```json
← {"jsonrpc":"2.0","result":{"content":[{"type":"text","text":"..."}]},"id":3}
```

---

## Agent Configuration & Tool Filtering

### Command-Line Usage

```bash
# All 20 tools available
gnucash-bridge

# Filtered to agent's tool list
gnucash-bridge --agent dhall/agents/spend-monitor.dhall
```

### Filtering Logic

1. Parse agent config with `dhall-to-json`
2. Store `AgentConfig` in global state (`g_agent_config`)
3. In `get_tool_definitions()`:
   - Build full tool list (20 tools)
   - If agent config loaded: filter to agent's `tools` list
   - Return filtered list
4. `tools/list` response contains only allowed tools
5. `tools/call` validates tool is in registry (implicit authorization)

### Example: spend-monitor Agent

**Config** (`dhall/agents/spend-monitor.dhall`):
```dhall
{ name = "spend-monitor"
, tools = [ "gnucash_get_accounts"
          , "gnucash_account_tree"
          , "gnucash_get_transactions"
          , "gnucash_get_balance"
          , "gnucash_trial_balance"
          , "gnucash_audit_log"
          ]
, authorization_level = Auto
, ...
}
```

**Result**: Only 6 tools advertised in `tools/list`, all others rejected

---

## Audit Trail Design

### Aperture Integration (Future)

When gnucash-mcp runs behind Tailscale Aperture:
1. Aperture populates identity fields: `user_email`, `user_name`, `node_name`
2. Audit records include authenticated identity
3. Policy enforcement based on Tailscale identity + tool + resource
4. Export audit trail to Aperture JSONL format:

```json
{"timestamp":"2026-02-25T18:56:02Z","user":"jess@example.com","node":"macbook.tail1234.ts.net","resource":"gnucash://book.gnucash/transaction/a1b2c3d4","action":"gnucash_post_transaction","decision":"allow","metadata":{"duration_ms":12}}
```

### Local Development

Without Aperture, identity fields are NULL:
- Still captures all operations
- Duration timing for performance analysis
- Before/after state for debugging
- Ready for Aperture when deployed

---

## Error Handling

### JSON-RPC 2.0 Error Codes

| Code | Meaning | Example |
|------|---------|---------|
| -32700 | Parse error | Invalid JSON |
| -32600 | Invalid request | Missing "method" field |
| -32601 | Method not found | Unknown method name |
| -32602 | Invalid params | Missing required param |
| -32603 | Internal error | Book not open, database error |

### Error Response Format

```json
{"jsonrpc":"2.0","error":{"code":-32603,"message":"no book open"},"id":5}
```

---

## Testing

### Manual Test Suites

- `test/test-mcp-session.sh` - Full MCP protocol lifecycle
- `test/test-audit-trail.sh` - Audit database creation and query
- `test/test-agent-filtering.sh` - Tool filtering with agent configs

### Justfile Recipes

```bash
just mcp-test            # Protocol tests
just mcp-test-audit      # Audit tests
just mcp-test-agent      # Agent filtering tests
just mcp-test-all        # All MCP tests
```

### Integration Test (Claude Code)

Add to `~/.config/claude/config.json`:
```json
{
  "mcpServers": {
    "gnucash": {
      "command": "/path/to/gnucash-bridge",
      "args": ["--agent", "dhall/agents/spend-monitor.dhall"],
      "env": {}
    }
  }
}
```

Then in Claude Code:
- "Show me my account balances"
- "What did I spend on SaaS last month?"
- "Show me the audit log"

---

## Performance

**Typical Tool Call Latency**:
- `gnucash_open`: ~50ms (one-time cost)
- `gnucash_get_accounts`: <1ms (cached in SQLite)
- `gnucash_get_transactions`: 1-10ms (depends on date range)
- `gnucash_trial_balance`: 5-20ms (calculates all balances)
- `gnucash_audit_log`: <5ms (indexed query)

**Audit Overhead**: <1ms per tool call (async write to separate database)

---

## Future Work (Week 6+)

### Phase 5.5: Nix Integration
- Hermetic build with `nix build .#gnucashMcp`
- Deployable binary

### Phase 3 Week 6: Agent Runtime
- spend-monitor: categorization + anomaly detection
- report-generator: financial reports
- transaction-categorizer: LLM-powered categorization

### Aperture Integration
- Deploy behind Tailscale Aperture proxy
- Identity-aware policy enforcement
- Audit trail with authenticated users

---

## References

- [MCP Specification](https://spec.modelcontextprotocol.io/)
- [JSON-RPC 2.0 Specification](https://www.jsonrpc.org/specification)
- [Tailscale Aperture](https://tailscale.com/kb/1288/aperture/)
- [Dhall Language](https://dhall-lang.org/)
- [GnuCash SQLite Schema](https://wiki.gnucash.org/wiki/SQL)
