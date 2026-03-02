# Week 7: Security Sidecar + Identity

**Goal**: Enforce the authorization rules already defined in Dhall, add identity tracking, rate limiting, and an approval flow for write operations.

## Architecture Decision

**Sidecar as inline interceptor, not a separate process.**

The "sidecar" is a C++ module (`security.h/cpp`) that sits between `mcp_dispatch()` and the actual tool execution. It's not a separate daemon -- it's a function call inserted into the MCP request pipeline:

```
MCP request → mcp_dispatch() → security_check() → execute tool → audit log
                                    │
                                    ├── identity lookup
                                    ├── authorization check (tier + rules)
                                    ├── rate limit check
                                    └── approval requirement → queue or block
```

This keeps the single-binary design from Week 6 and avoids IPC complexity.

## Phase 7.1: Identity Provider (Day 1)

**Files**: `include/gnucash/identity.h`, `src/identity.cpp`

Identity resolved from (priority order):
1. `--identity <email>` CLI flag
2. `GNUCASH_USER` environment variable
3. Tailscale `whois` (if available)
4. System username fallback

```cpp
struct Identity {
    std::string user_id;      // email or username
    std::string display_name; // human-readable
    std::string node_name;    // hostname
    std::string source;       // "cli", "env", "tailscale", "system"
};

Result<Identity> resolve_identity(const std::optional<std::string>& cli_identity);
```

Wire into audit records: `audit_record.user_email`, `audit_record.node_name`.

## Phase 7.2: Authorization Enforcement (Day 2-3)

**Files**: `include/gnucash/security.h`, `src/security.cpp`

### SecurityPolicy

Loaded from Dhall rules at startup (already parsed by dhall_config):

```cpp
struct AuthorizationRule {
    std::string operation;
    AuthorizationLevel level;  // AUTO, REVIEW, APPROVE
    int max_per_hour;
    std::optional<int64_t> max_amount_cents;
};

struct SecurityPolicy {
    std::vector<AuthorizationRule> rules;
    Identity identity;
    bool enforcement_enabled;  // --enforce flag, default false for backward compat
};
```

### security_check()

Called from `mcp_dispatch()` before tool execution:

```cpp
enum class SecurityDecision { ALLOW, QUEUE_REVIEW, REQUIRE_APPROVAL, DENY };

struct SecurityResult {
    SecurityDecision decision;
    std::string reason;
    std::optional<std::string> approval_id;  // for QUEUE_REVIEW/REQUIRE_APPROVAL
};

SecurityResult security_check(const SecurityPolicy& policy,
                              const std::string& tool_name,
                              const json& arguments,
                              const std::string& agent_name);
```

Decision logic:
- **Read tools** → ALLOW (always)
- **Write tool, Auto tier agent** → check rate limit, ALLOW or DENY
- **Write tool, Review tier** → QUEUE_REVIEW (store in approval DB, return approval_id)
- **Write tool, Approve tier** → REQUIRE_APPROVAL (block until human approves)
- **Amount exceeds max_amount_cents** → REQUIRE_APPROVAL regardless of tier

### Integration point in mcp.cpp

Insert between tool name resolution and dispatch:

```cpp
// In mcp_dispatch(), after determining tool_name and arguments:
if (policy.enforcement_enabled) {
    auto sec = security_check(policy, tool_name, arguments, agent_name);
    if (sec.decision == SecurityDecision::DENY) {
        // return JSON-RPC error
    } else if (sec.decision == SecurityDecision::QUEUE_REVIEW) {
        // store in approval queue, return pending status
    } else if (sec.decision == SecurityDecision::REQUIRE_APPROVAL) {
        // block, return approval_id for human to approve
    }
    audit_record.authorization_level = sec.decision;
    audit_record.approval_guid = sec.approval_id;
}
```

## Phase 7.3: Rate Limiter (Day 3)

**In**: `security.cpp` (part of SecurityPolicy)

Simple sliding window per (agent, operation):

```cpp
struct RateLimiter {
    // key: "{agent_name}:{operation}" → deque of timestamps
    std::map<std::string, std::deque<std::chrono::steady_clock::time_point>> windows;

    bool check_and_record(const std::string& agent, const std::string& operation, int max_per_hour);
    int remaining(const std::string& agent, const std::string& operation, int max_per_hour);
};
```

Rate limits come from Dhall rules (`max_per_hour` field). Stored in-memory (resets on process restart -- acceptable for a single-binary MCP server).

## Phase 7.4: Approval Queue (Day 4-5)

**Files**: Extend `agent_state.h/cpp` with approval table, or new `approval.h/cpp`

Decision: Use a **shared approval SQLite DB** (not per-agent) at `{book}.approvals.db`.

```cpp
struct ApprovalRequest {
    std::string id;           // GUID
    std::string agent_name;
    std::string tool_name;
    json arguments;
    std::string requesting_user;
    std::string created_at;
    std::string status;       // "pending", "approved", "rejected", "expired"
    std::optional<std::string> approver;
    std::optional<std::string> resolved_at;
};

class ApprovalDB {
public:
    static Result<ApprovalDB> open(const std::string& book_path);

    Result<std::string> create_request(const std::string& agent, const std::string& tool,
                                       const json& args, const std::string& user);
    Result<std::vector<ApprovalRequest>> pending_requests(int limit = 50);
    Result<void> approve(const std::string& id, const std::string& approver);
    Result<void> reject(const std::string& id, const std::string& approver, const std::string& reason);
    Result<std::optional<ApprovalRequest>> get_request(const std::string& id);
};
```

### MCP tools for approval flow

Add 3 new MCP tools:
- `gnucash_list_approvals` - List pending approval requests (read)
- `gnucash_approve_request` - Approve a pending request (write, Approve tier)
- `gnucash_reject_request` - Reject a pending request (write, Approve tier)

This enables Claude Code (or a human via the CLI) to review and approve queued operations.

## Phase 7.5: Anomaly Detection (Day 5)

**In**: `security.cpp`

Simple rule-based detection (not ML):

```cpp
struct AnomalyCheck {
    bool is_anomalous;
    std::string reason;
    double severity;  // 0.0-1.0
};

AnomalyCheck check_transaction_anomaly(const json& arguments,
                                        const AgentStateDB& state);
```

Checks:
1. **Amount threshold** - Transaction > $5,000 (configurable)
2. **Frequency spike** - >3x normal rate for this agent in last hour
3. **New vendor** - Description doesn't match any known vendor pattern
4. **Off-hours** - Transaction posted outside business hours (configurable)

Anomalies are logged to audit trail with `classification = ANOMALY` and optionally escalate to REQUIRE_APPROVAL.

## Phase 7.6: CLI Integration + Tests (Day 6-7)

### New CLI flags (main.cpp)

```
--identity <email>     Set user identity
--enforce              Enable authorization enforcement (default: off)
--approval-db <path>   Override approval DB path
--dry-run              Show security decisions without executing
```

### Catch2 Tests

Target: 20+ new tests covering:
- Identity resolution (CLI, env, system fallback)
- Authorization decisions (allow, queue, deny per tier)
- Rate limiting (window, reset, overflow)
- Approval queue (create, approve, reject, expire)
- Anomaly detection (amount, frequency, new vendor)
- Integration: security_check blocks write in enforce mode

### Justfile recipes

```
security-test          # Run security Catch2 tests
approval-list book     # Show pending approvals
approval-approve id    # Approve a request
```

## Deferred to Week 8

- **Tailscale whois integration** - requires Tailscale SDK, defer to when infra is ready
- **Webhook notifications** - approval requests trigger webhook (Slack, email)
- **Expiration sweep** - cron to expire stale approval requests
- **Cryptographic signing** - sign audit records with agent identity key

## Files Created/Modified

| File | Action |
|------|--------|
| `include/gnucash/identity.h` | NEW |
| `src/identity.cpp` | NEW |
| `include/gnucash/security.h` | NEW |
| `src/security.cpp` | NEW |
| `include/gnucash/approval.h` | NEW |
| `src/approval.cpp` | NEW |
| `test/test_security.cpp` | NEW |
| `src/mcp.cpp` | MODIFY - insert security_check |
| `src/main.cpp` | MODIFY - add CLI flags |
| `CMakeLists.txt` | MODIFY - add sources |
| `justfile` | MODIFY - add recipes |
| `docs/epic/PROGRESS.md` | MODIFY |

## Success Criteria (Gate G4 partial)

- [ ] `--enforce` flag blocks unauthorized writes
- [ ] Rate limiter denies after threshold
- [ ] Approval queue stores and resolves requests
- [ ] Anomaly detection flags large transactions
- [ ] Identity flows through to audit records
- [ ] All existing 149 tests still pass
- [ ] 20+ new security tests pass
- [ ] Backward compatible: without `--enforce`, behavior unchanged
