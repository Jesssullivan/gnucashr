# Agent Development Guide - gnucashr

**Version**: 0.3.0
**Status**: Complete (Week 5)
**Date**: 2026-02-25

---

## Overview

This guide explains how to create new agent configurations for gnucashr's MCP server. Agents are personas that interact with GnuCash books through a filtered set of MCP tools, with specific authorization levels and scheduling patterns.

**Key Concepts**:
- **Agent**: A configured persona with specific tools, authorization tier, and behavior patterns
- **Tool Filtering**: Agents only see tools in their `tools` list
- **Authorization Levels**: Three tiers (Auto, Review, Approve) control risk exposure
- **Vendor Patterns**: Regex/substring patterns for transaction categorization
- **Schedules**: Hourly, Daily, or OnDemand execution patterns

---

## Quick Start

### 1. Create Agent Config

```bash
# Create new agent config
touch dhall/agents/my-agent.dhall

# Type-check it
dhall type --file dhall/agents/my-agent.dhall

# Test tool filtering
echo '{"jsonrpc":"2.0","method":"tools/list","id":1}' | \
  lib/gnucash-core/build/gnucash-bridge --agent dhall/agents/my-agent.dhall | \
  jq '.result.tools[] | .name'
```

### 2. Minimal Agent Template

```dhall
let Types = ../types.dhall

in  Types.Agent::{
    , name = "my-agent"
    , description = "Brief description of what this agent does"
    , tools = ["gnucash_get_accounts", "gnucash_get_transactions"]
    , authorization_level = Types.AuthorizationLevel.Auto
    }
```

### 3. Run Agent

```bash
# Start MCP server with agent config
just mcp-agent my-agent ~/path/to/book.gnucash

# Or use directly
cd lib/gnucash-core/build
./gnucash-bridge --agent ../../../dhall/agents/my-agent.dhall
```

---

## Dhall Type Reference

### Agent Type

```dhall
{ Type =
    { name : Text                           -- Agent name (kebab-case)
    , description : Text                    -- Human-readable purpose
    , tools : List Text                     -- MCP tool names (must match exactly)
    , authorization_level : AuthorizationLevel
    , vendor_patterns : List VendorPattern.Type
    , schedule : Optional Schedule
    }
, default =
    { schedule = None Schedule
    , authorization_level = AuthorizationLevel.Auto
    , vendor_patterns = [] : List VendorPattern.Type
    }
}
```

### AuthorizationLevel

```dhall
< Auto       -- Read-only, reports, categorization (all 15 read tools)
| Review     -- Creates artifacts needing review (can use write tools)
| Approve    -- Money movement, account changes (requires explicit approval)
>
```

**Authorization Tier Guidelines**:

| Tier | Tool Access | Use Cases | Example Agents |
|------|-------------|-----------|----------------|
| **Auto** | Read tools only | Monitoring, reports, categorization | spend-monitor, report-generator |
| **Review** | Read + write (review queue) | Transaction drafts, categorization changes | transaction-categorizer |
| **Approve** | All tools (explicit approval) | Money movement, reconciliation, account creation | (Week 6+ agents) |

### Schedule

```dhall
< Hourly : Natural       -- Every N hours (e.g., Hourly 1 = every hour)
| Daily : Natural        -- Every N days at midnight UTC
| OnDemand               -- Manual triggering only
>
```

**Schedule Guidelines**:
- **Hourly**: Monitoring agents (spend-monitor)
- **Daily**: Report generation, reconciliation
- **OnDemand**: Interactive agents, categorization on new transactions

### VendorPattern

```dhall
{ Type =
    { pattern : Text              -- Regex or substring (case-insensitive)
    , category : Text             -- Target account path (e.g., "Expenses:SaaS:AI")
    , confidence : Double         -- 0.0-1.0 confidence score
    }
, default =
    { confidence = 0.95 }
}
```

**Pattern Matching Strategy**:
- Use specific strings for high confidence (0.95-0.99): `"ANTHROPIC"`, `"MODAL LABS"`
- Use broader patterns for lower confidence (0.80-0.90): `"AWS.*EC2"`, `"GOOGLE"`
- Confidence >= 0.90: Auto-apply categorization
- Confidence < 0.90: Queue for human review

---

## MCP Tool Catalog

### Read Tools (Auto Tier) - 15 tools

**Book Management**:
- `gnucash_open` - Open a GnuCash book
- `gnucash_close` - Close current book
- `gnucash_info` - Book metadata

**Account Queries**:
- `gnucash_get_accounts` - List all accounts
- `gnucash_account_tree` - Hierarchical account tree
- `gnucash_get_account` - Get account by GUID
- `gnucash_get_account_by_path` - Lookup by path string

**Transaction Queries**:
- `gnucash_get_transactions` - Query transactions (date filters)
- `gnucash_get_transaction` - Get transaction by GUID
- `gnucash_get_splits` - Splits for account

**Balance & Reporting**:
- `gnucash_get_balance` - Account balance as of date
- `gnucash_trial_balance` - All account balances

**Commodity & Price Data**:
- `gnucash_get_commodities` - List currencies/securities
- `gnucash_get_prices` - Price database

**Import**:
- `gnucash_parse_ofx` - Parse OFX file content

### Write Tools (Approve/Review Tier) - 4 tools

- `gnucash_create_account` - Create new account
- `gnucash_post_transaction` - Create transaction
- `gnucash_delete_transaction` - Remove transaction
- `gnucash_void_transaction` - Void with reversal

### Audit Tool - 1 tool

- `gnucash_audit_log` - Query audit trail

---

## Agent Examples

### Example 1: spend-monitor (Monitoring)

**Purpose**: Monitor SaaS/cloud spending, detect anomalies

```dhall
let Types = ../types.dhall

in  Types.Agent::{
    , name = "spend-monitor"
    , description = "Monitor and categorize spending on SaaS, cloud, and compute services"
    , tools =
      [ "gnucash_get_accounts"
      , "gnucash_account_tree"
      , "gnucash_get_transactions"
      , "gnucash_get_balance"
      , "gnucash_trial_balance"
      , "gnucash_audit_log"
      ]
    , authorization_level = Types.AuthorizationLevel.Auto
    , vendor_patterns =
      [ { pattern = "MODAL LABS", category = "Expenses:SaaS:Compute", confidence = 0.99 }
      , { pattern = "ANTHROPIC", category = "Expenses:SaaS:AI", confidence = 0.99 }
      , { pattern = "AWS", category = "Expenses:Cloud:AWS", confidence = 0.90 }
      ]
    , schedule = Some (Types.Schedule.Hourly 1)
    }
```

**Tool Selection Rationale**:
- Account queries: Understand account structure
- Transactions: Pull spending data
- Balance/trial balance: Track spending trends
- Audit log: Self-monitoring (what did I do?)
- **No write tools**: Read-only monitoring

**Vendor Patterns**:
- High confidence (0.99) for distinctive names: `MODAL LABS`, `ANTHROPIC`
- Lower confidence (0.90) for ambiguous names: `AWS` (could be "SAWS", "AWNINGS")

### Example 2: report-generator (Reporting)

**Purpose**: Generate financial reports on schedule

```dhall
let Types = ../types.dhall

in  Types.Agent::{
    , name = "report-generator"
    , description = "Generate trial balance, income statement, balance sheet"
    , tools =
      [ "gnucash_get_accounts"
      , "gnucash_account_tree"
      , "gnucash_get_transactions"
      , "gnucash_get_balance"
      , "gnucash_trial_balance"
      , "gnucash_get_commodities"
      , "gnucash_get_prices"
      , "gnucash_info"
      ]
    , authorization_level = Types.AuthorizationLevel.Auto
    , schedule = Some (Types.Schedule.Daily 1)
    }
```

**Tool Selection Rationale**:
- All read tools for comprehensive reporting
- Commodities + prices: Multi-currency/investment reports
- Book info: Metadata for report headers
- **No vendor patterns**: Not doing categorization

**Schedule**: Daily at midnight UTC for consistent reporting

### Example 3: transaction-categorizer (Interactive)

**Purpose**: Auto-categorize transactions with human review

```dhall
let Types = ../types.dhall
let SpendMonitor = ./spend-monitor.dhall

in  Types.Agent::{
    , name = "transaction-categorizer"
    , description = "Automatically categorize transactions with pattern matching + LLM"
    , tools =
      [ "gnucash_get_accounts"
      , "gnucash_account_tree"
      , "gnucash_get_transactions"
      , "gnucash_get_transaction"
      , "gnucash_post_transaction"     -- For updating categories
      , "gnucash_audit_log"
      ]
    , authorization_level = Types.AuthorizationLevel.Review
    , vendor_patterns = SpendMonitor.vendor_patterns  -- Reuse patterns
    , schedule = Some Types.Schedule.OnDemand
    }
```

**Tool Selection Rationale**:
- Transaction queries: Find uncategorized transactions
- **Post transaction**: Update transaction splits with correct categories
- Audit log: Track categorization decisions
- **Review tier**: Human reviews low-confidence matches (<0.90)

**Pattern Reuse**: Import patterns from spend-monitor to avoid duplication

**Schedule**: OnDemand - triggered when new transactions appear

---

## Tool Selection Guidelines

### Start with Minimal Tools

**Anti-pattern**: Granting all 20 tools to every agent

**Best practice**: Start with 3-5 tools, add as needed

```dhall
-- BAD: Kitchen sink approach
, tools = [ "gnucash_get_accounts", "gnucash_account_tree", "gnucash_get_transactions",
            "gnucash_get_transaction", "gnucash_get_splits", "gnucash_get_balance",
            "gnucash_trial_balance", "gnucash_get_commodities", "gnucash_get_prices",
            "gnucash_create_account", "gnucash_post_transaction", ... ]

-- GOOD: Minimal sufficient set
, tools = [ "gnucash_get_accounts", "gnucash_get_transactions", "gnucash_trial_balance" ]
```

### Tool Dependencies

Some tools imply others:

| If you need... | You probably also need... | Why? |
|----------------|---------------------------|------|
| `get_transactions` | `get_accounts`, `account_tree` | Understand account structure |
| `post_transaction` | `get_account_by_path` | Resolve account paths to GUIDs |
| `get_balance` | `get_account` | Get account details first |
| `parse_ofx` | `post_transaction`, `get_accounts` | Import parsed transactions |

### Read vs Write Tools

**Golden Rule**: Only add write tools if the agent genuinely needs to modify data

| Agent Type | Tool Access | Authorization |
|------------|-------------|---------------|
| Monitoring/Reporting | Read only | Auto |
| Categorization/Drafts | Read + post_transaction | Review |
| Reconciliation/Import | Read + post/void/delete | Approve |
| Account Management | Read + create_account | Approve |

### Tool Filtering Validation

Test that your agent only sees expected tools:

```bash
# Check tool count and names
echo '{"jsonrpc":"2.0","method":"tools/list","id":1}' | \
  ./gnucash-bridge --agent dhall/agents/my-agent.dhall | \
  jq '.result.tools | length'  # Should match your tools list length

# List tool names
echo '{"jsonrpc":"2.0","method":"tools/list","id":1}' | \
  ./gnucash-bridge --agent dhall/agents/my-agent.dhall | \
  jq -r '.result.tools[].name'
```

---

## Vendor Pattern Development

### Pattern Testing Workflow

1. **Export transactions to analyze**:
   ```bash
   # Get all transactions from last 90 days
   echo '{"method":"get_transactions","params":{"from_date":"2025-11-25"},"id":1}' | \
     ./gnucash-bridge | jq '.result[] | .description'
   ```

2. **Identify common patterns**:
   - SaaS providers: `MODAL LABS`, `VERCEL`, `GITHUB`
   - Cloud providers: `AWS`, `GOOGLE CLOUD`, `AZURE`
   - Payment processors: `STRIPE`, `PAYPAL`

3. **Create patterns with confidence scores**:
   ```dhall
   , vendor_patterns =
     [ { pattern = "MODAL LABS"       -- Exact match
       , category = "Expenses:SaaS:Compute"
       , confidence = 0.99            -- Very confident
       }
     , { pattern = "AWS.*"            -- Regex pattern
       , category = "Expenses:Cloud:AWS"
       , confidence = 0.85            -- Less confident (many AWS services)
       }
     ]
   ```

4. **Test pattern matching**:
   ```bash
   # Manually test pattern application
   dhall-to-json --file dhall/agents/my-agent.dhall | \
     jq '.vendor_patterns[] | select(.pattern | test("MODAL"))'
   ```

### Confidence Score Guidelines

| Score | Meaning | Action | Example |
|-------|---------|--------|---------|
| 0.95-1.0 | Very confident | Auto-categorize immediately | `"ANTHROPIC"` → Expenses:SaaS:AI |
| 0.90-0.94 | Confident | Auto-categorize with audit log | `"GITHUB"` → Expenses:SaaS:DevTools |
| 0.80-0.89 | Somewhat confident | Queue for review | `"AWS"` → Expenses:Cloud:AWS |
| <0.80 | Uncertain | Require human review | `"AMZN"` → Could be AWS or Amazon retail |

### Pattern Composition

**Reuse patterns across agents**:

```dhall
-- dhall/agents/base-patterns.dhall
let Types = ../types.dhall

in  { saas_patterns =
      [ { pattern = "MODAL LABS", category = "Expenses:SaaS:Compute", confidence = 0.99 }
      , { pattern = "VERCEL", category = "Expenses:SaaS:Hosting", confidence = 0.95 }
      , { pattern = "GITHUB", category = "Expenses:SaaS:DevTools", confidence = 0.98 }
      ]
    , cloud_patterns =
      [ { pattern = "AWS", category = "Expenses:Cloud:AWS", confidence = 0.90 }
      , { pattern = "GOOGLE CLOUD", category = "Expenses:Cloud:GCP", confidence = 0.92 }
      ]
    }

-- dhall/agents/my-agent.dhall
let Types = ../types.dhall
let BasePatterns = ./base-patterns.dhall

in  Types.Agent::{
    , vendor_patterns = BasePatterns.saas_patterns # BasePatterns.cloud_patterns
    }
```

---

## Scheduling Strategies

### Hourly Agents

**Use case**: Near-real-time monitoring

```dhall
, schedule = Some (Types.Schedule.Hourly 1)  -- Every hour
```

**Best for**:
- Spending anomaly detection
- Alert generation
- Compliance monitoring

**Considerations**:
- Lightweight operations only (query, categorize, report)
- No heavy writes (reconciliation, bulk imports)

### Daily Agents

**Use case**: Scheduled reporting, maintenance

```dhall
, schedule = Some (Types.Schedule.Daily 1)  -- Every day at midnight UTC
```

**Best for**:
- Financial reports (trial balance, income statement)
- Daily reconciliation
- Batch categorization

**Considerations**:
- Runs at midnight UTC (adjust for local timezone)
- Can perform heavier operations

### OnDemand Agents

**Use case**: Interactive, event-driven

```dhall
, schedule = Some Types.Schedule.OnDemand
```

**Best for**:
- User-triggered actions (reconcile this account)
- Event-driven (new transaction imported)
- One-off tasks (fix categorization for Q4)

**Triggering mechanisms (Week 6+)**:
- User command: `gnucashr agent run transaction-categorizer`
- Webhook: New OFX import triggers categorizer
- Manual: Claude Code MCP call

---

## Testing & Validation

### Type-Check Agent Config

```bash
# Verify Dhall syntax and types
dhall type --file dhall/agents/my-agent.dhall

# Check all agent configs
just mcp-check-agents
```

### Test Tool Filtering

```bash
# Build bridge
just mcp-build

# Test without agent (should see all 20 tools)
echo '{"jsonrpc":"2.0","method":"tools/list","id":1}' | \
  lib/gnucash-core/build/gnucash-bridge | \
  jq '.result.tools | length'

# Test with agent (should see filtered list)
echo '{"jsonrpc":"2.0","method":"tools/list","id":1}' | \
  lib/gnucash-core/build/gnucash-bridge --agent dhall/agents/my-agent.dhall | \
  jq '.result.tools | length'
```

### Integration Test with Claude Code

Add to `~/.config/claude/config.json`:

```json
{
  "mcpServers": {
    "gnucash-my-agent": {
      "command": "/path/to/gnucash-bridge",
      "args": ["--agent", "/path/to/dhall/agents/my-agent.dhall"],
      "env": {}
    }
  }
}
```

Then in Claude Code:
- "Show me my account balances" (should work if agent has `get_accounts`)
- "Create a new account" (should fail if agent doesn't have `create_account`)

### Verify Audit Trail

```bash
# Run agent operation
just mcp-agent my-agent ~/test/book.gnucash

# Check audit log
sqlite3 ~/test/book.gnucash.audit.db \
  "SELECT tool_name, classification, result_status FROM audit_log ORDER BY timestamp DESC LIMIT 10"
```

---

## Common Patterns

### Pattern 1: Monitoring Agent

**Characteristics**:
- Read-only tools
- Auto authorization
- Hourly schedule
- Vendor patterns for categorization

**Template**:
```dhall
let Types = ../types.dhall

in  Types.Agent::{
    , name = "my-monitor"
    , description = "Monitor X and detect Y"
    , tools = ["gnucash_get_accounts", "gnucash_get_transactions", "gnucash_audit_log"]
    , authorization_level = Types.AuthorizationLevel.Auto
    , vendor_patterns = [ {pattern = "...", category = "...", confidence = 0.95} ]
    , schedule = Some (Types.Schedule.Hourly 1)
    }
```

### Pattern 2: Reporting Agent

**Characteristics**:
- Read-only tools (comprehensive)
- Auto authorization
- Daily schedule
- No vendor patterns

**Template**:
```dhall
let Types = ../types.dhall

in  Types.Agent::{
    , name = "my-reporter"
    , description = "Generate X reports"
    , tools =
      [ "gnucash_get_accounts", "gnucash_account_tree", "gnucash_get_transactions"
      , "gnucash_get_balance", "gnucash_trial_balance", "gnucash_get_commodities"
      , "gnucash_get_prices", "gnucash_info"
      ]
    , authorization_level = Types.AuthorizationLevel.Auto
    , schedule = Some (Types.Schedule.Daily 1)
    }
```

### Pattern 3: Interactive Agent

**Characteristics**:
- Read + limited write tools
- Review/Approve authorization
- OnDemand schedule
- Vendor patterns for smart behavior

**Template**:
```dhall
let Types = ../types.dhall

in  Types.Agent::{
    , name = "my-assistant"
    , description = "Help user with X tasks"
    , tools =
      [ "gnucash_get_accounts", "gnucash_get_transactions"
      , "gnucash_post_transaction"  -- Write operation
      ]
    , authorization_level = Types.AuthorizationLevel.Review  -- Human review
    , vendor_patterns = [ {pattern = "...", category = "...", confidence = 0.85} ]
    , schedule = Some Types.Schedule.OnDemand
    }
```

---

## Security & Best Practices

### 1. Principle of Least Privilege

**Only grant tools the agent actually needs**:
- Start with read tools
- Add write tools one at a time
- Test with minimal permissions first

### 2. Authorization Tier Selection

| Question | Yes → | No → |
|----------|-------|------|
| Does agent modify financial data? | Review/Approve | Auto |
| Are modifications reversible? | Review | Approve |
| Does agent move money between accounts? | Approve | Review |
| Does agent only read/report? | Auto | Review |

### 3. Vendor Pattern Safety

**Avoid overly broad patterns**:
```dhall
-- BAD: Matches too much
{ pattern = ".*", category = "Expenses:Unknown", confidence = 0.50 }

-- GOOD: Specific pattern with appropriate confidence
{ pattern = "MODAL LABS", category = "Expenses:SaaS:Compute", confidence = 0.99 }
```

### 4. Schedule Appropriateness

- **Hourly**: Only for lightweight monitoring (no writes)
- **Daily**: Safe for most operations
- **OnDemand**: Best for write operations

### 5. Audit Trail Review

**Always include `gnucash_audit_log` in agent tools** to enable self-monitoring:

```dhall
, tools = [ "gnucash_get_accounts", "gnucash_audit_log" ]  -- Agent can audit itself
```

### 6. Testing Before Production

**Test agent on sample book first**:
```bash
# Create test book
cp ~/finance/real-book.gnucash ~/test/test-book.gnucash

# Run agent on test book
just mcp-agent my-agent ~/test/test-book.gnucash

# Verify audit trail
sqlite3 ~/test/test-book.gnucash.audit.db "SELECT * FROM audit_log"

# If satisfied, use on real book
just mcp-agent my-agent ~/finance/real-book.gnucash
```

---

## Troubleshooting

### Agent Config Fails Type-Check

**Error**: `dhall: Invalid Dhall input`

**Solution**: Check Dhall syntax
```bash
dhall format --inplace dhall/agents/my-agent.dhall
dhall type --file dhall/agents/my-agent.dhall
```

### Tool Name Mismatch

**Error**: `tools/call: unknown method: list_accounts`

**Solution**: Verify tool names exactly match MCP registry
```bash
# List all available tools
echo '{"jsonrpc":"2.0","method":"tools/list","id":1}' | \
  ./gnucash-bridge | jq -r '.result.tools[].name'
```

### Agent Sees Wrong Tools

**Error**: Agent has tools not in config

**Solution**: Check agent loading
```bash
# Verify --agent flag is passed
./gnucash-bridge --agent dhall/agents/my-agent.dhall

# Check stderr for agent config loading messages
./gnucash-bridge --agent dhall/agents/my-agent.dhall 2>&1 | grep -i agent
```

### Vendor Patterns Not Matching

**Error**: Transactions not being categorized

**Solution**: Test patterns manually
```bash
# Export agent config to JSON
dhall-to-json --file dhall/agents/my-agent.dhall > /tmp/agent.json

# Check patterns
jq '.vendor_patterns' /tmp/agent.json

# Compare with actual transaction descriptions
echo '{"method":"get_transactions","id":1}' | ./gnucash-bridge | \
  jq -r '.result[] | .description'
```

---

## Future Work (Week 6+)

### Agent Runtime (Week 6)

- **Agent executor**: Run agents on schedule or on-demand
- **Job queue**: Manage agent execution
- **Result storage**: Store agent outputs (reports, drafts)
- **Human review workflow**: Queue low-confidence operations

### Tailscale Aperture Integration

- **Identity-aware audit trail**: Populate `user_email`, `node_name`
- **Policy enforcement**: Restrict tools based on Tailscale identity
- **Aperture JSONL export**: Stream audit logs to Aperture
- **Zero-trust architecture**: All access via Tailscale + Aperture

### Advanced Patterns

- **Multi-agent workflows**: Spend-monitor triggers transaction-categorizer
- **LLM integration**: Use Claude API for natural language categorization
- **Anomaly detection**: Statistical models for spending outliers
- **Reconciliation agents**: Auto-match bank statements to transactions

---

## References

- [MCP Specification](https://spec.modelcontextprotocol.io/)
- [Dhall Language](https://dhall-lang.org/)
- [Dhall Standard Library](https://prelude.dhall-lang.org/)
- [GnuCash Documentation](https://www.gnucash.org/docs.phtml)
- [Tailscale Aperture](https://tailscale.com/kb/1288/aperture/)
- [gnucashr MCP Architecture](./MCP_ARCHITECTURE.md)
- [gnucashr Epic Plan](./epic/README.md)

---

**Questions?** See docs/MCP_ARCHITECTURE.md for low-level protocol details.
