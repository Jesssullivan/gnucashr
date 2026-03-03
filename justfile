# gnucashr monorepo - central command runner
# Replaces Makefile as the primary developer interface
#
# Usage: just --list

set dotenv-load
set positional-arguments

# ============================================================
# R Package (packages/gnucashr)
# ============================================================

# Run R CMD check on the package
check:
    cd packages/gnucashr && Rscript -e "devtools::check()"

# Run R package tests
test:
    cd packages/gnucashr && Rscript -e "devtools::test()"

# Generate roxygen2 documentation
document:
    cd packages/gnucashr && Rscript -e "devtools::document()"

# Build R source tarball
build:
    cd packages/gnucashr && R CMD build .

# Run R CMD check --as-cran
cran-check: document
    cd packages/gnucashr && R CMD build . && R CMD check --as-cran gnucashr_*.tar.gz

# Install package locally
install:
    cd packages/gnucashr && R CMD INSTALL .

# Run code coverage
coverage:
    cd packages/gnucashr && Rscript -e 'covr::package_coverage(type = "tests")'

# Lint R code
lint:
    cd packages/gnucashr && Rscript -e "lintr::lint_package()"

# ============================================================
# Nix
# ============================================================

# Enter Nix development shell
shell:
    nix develop

# Build R package tarball via Nix
nix-build:
    nix build .#tarball

# Run R CMD check via Nix
nix-check:
    nix build .#checks.x86_64-linux.r-cmd-check

# Build all Nix derivations
nix-all:
    nix build .#rDeps .#cppBuild .#tarball --max-jobs 4

# ============================================================
# C++ Library (lib/gnucash-core)
# ============================================================

# Configure C++ library build
cpp-configure:
    cd lib/gnucash-core && mkdir -p build && cd build && cmake .. -DCMAKE_BUILD_TYPE=Debug

# Build C++ library
cpp-build: cpp-configure
    cd lib/gnucash-core/build && cmake --build . -j$(nproc)

# Run C++ library tests
cpp-test: cpp-build
    cd lib/gnucash-core/build && ctest --output-on-failure

# Clean C++ build artifacts
cpp-clean:
    rm -rf lib/gnucash-core/build

# Build C++ library via Nix (hermetic)
cpp-nix-build:
    nix build .#gnucashCore

# Run C++ tests via Nix (hermetic)
cpp-nix-test:
    nix build .#gnucashCoreTests

# Build JSON bridge via Nix (hermetic)
cpp-nix-bridge:
    nix build .#gnucashBridge

# Run JSON bridge interactively (stdin/stdout)
bridge book:
    cd lib/gnucash-core/build && ./gnucash-bridge <<< '{"method":"open","params":{"path":"{{book}}"},"id":1}'

# ============================================================
# Dhall
# ============================================================

# Type-check all Dhall configuration
dhall-check:
    dhall type --file dhall/package.dhall --quiet
    @echo "All Dhall types OK"

# Export authorization rules to JSON
dhall-export-rules:
    dhall-to-json --file dhall/rules/authorization.dhall

# Export all templates to JSON
dhall-export-templates:
    @echo "=== SaaS Spend ===" && dhall-to-json --file dhall/templates/saas-spend.dhall
    @echo "=== Tax Categories ===" && dhall-to-json --file dhall/templates/tax-categories.dhall
    @echo "=== Income Categories ===" && dhall-to-json --file dhall/templates/income-categories.dhall

# Lint all Dhall files
dhall-lint:
    find dhall -name "*.dhall" -exec dhall lint --inplace {} \;

# ============================================================
# MCP Server (Week 5)
# ============================================================

# Build MCP server
mcp-build:
    cd lib/gnucash-core && mkdir -p build && cd build && cmake .. -DCMAKE_BUILD_TYPE=Release && cmake --build . -j$(nproc)

# Run MCP server (stdin/stdout)
mcp-run book *args='':
    cd lib/gnucash-core/build && echo '{"jsonrpc":"2.0","method":"initialize","params":{"protocolVersion":"2024-11-05","capabilities":{},"clientInfo":{"name":"test","version":"1.0"}},"id":1}' | ./gnucash-bridge {{args}}

# Run MCP server with agent config
mcp-agent agent book:
    cd lib/gnucash-core/build && ./gnucash-bridge --agent ../../dhall/agents/{{agent}}.dhall

# Test MCP protocol
mcp-test:
    cd lib/gnucash-core/build && bash ../test/test-mcp-session.sh

# Test audit trail
mcp-test-audit:
    cd lib/gnucash-core/build && bash ../test/test-audit-trail.sh

# Test agent filtering
mcp-test-agent:
    cd lib/gnucash-core/build && bash ../test/test-agent-filtering.sh

# Run all MCP tests
mcp-test-all: mcp-test mcp-test-audit mcp-test-agent
    @echo "All MCP tests passed ✓"

# Query audit log for a book
mcp-audit book *filters='':
    @echo '{"jsonrpc":"2.0","method":"tools/call","params":{"name":"gnucash_audit_log","arguments":{}},"id":1}' | sqlite3 {{book}}.audit.db "SELECT * FROM audit_log ORDER BY timestamp DESC LIMIT 10"

# Type-check agent configs
mcp-check-agents:
    @for agent in spend-monitor report-generator transaction-categorizer invoice-generator tax-estimator subscription-manager bill-pay bank-feed-importer reconciler; do \
        echo "Checking dhall/agents/$$agent.dhall..."; \
        dhall type --file dhall/agents/$$agent.dhall > /dev/null && echo "  ✓ Valid" || echo "  ✗ Error"; \
    done

# Build MCP server via Nix (hermetic)
mcp-nix-build:
    nix build .#gnucashMcp

# ============================================================
# Agents (Week 6)
# ============================================================

# Run an agent against a GnuCash book (CLI one-shot)
agent-run agent book:
    cd lib/gnucash-core/build && ./gnucash-bridge --agent ../../dhall/agents/{{agent}}.dhall --run {{book}}

# Run agent tests (Catch2)
agent-test: cpp-build
    cd lib/gnucash-core/build && ctest --output-on-failure -R "agent|vendor|state|spend|report|categorizer|dispatch"

# Show agent state for a book
agent-status agent book:
    @echo "Agent state DB: {{book}}.agent.{{agent}}.db"
    @sqlite3 "{{book}}.agent.{{agent}}.db" "SELECT key, value, updated_at FROM agent_state ORDER BY updated_at DESC LIMIT 10" 2>/dev/null || echo "  (no state yet)"

# Show review queue for a book
agent-review book agent="transaction-categorizer":
    @echo "Review queue: {{book}}.agent.{{agent}}.db"
    @sqlite3 "{{book}}.agent.{{agent}}.db" "SELECT id, transaction_guid, suggested_category, confidence, status FROM review_queue WHERE status='pending' LIMIT 20" 2>/dev/null || echo "  (no pending reviews)"

# ============================================================
# Daemon / Scheduler (Week 15)
# ============================================================

# Start daemon with a config file
daemon-start config:
    cd lib/gnucash-core/build && ./gnucash-bridge --daemon {{config}}

# Validate a daemon config file (parse only)
daemon-config-validate config:
    @cd lib/gnucash-core/build && echo '{}' | timeout 1 ./gnucash-bridge --daemon {{config}} 2>&1; true

# Run scheduler tests (Catch2)
scheduler-test: cpp-build
    cd lib/gnucash-core/build && ctest --output-on-failure -R "scheduler"

# ============================================================
# Security (Week 7)
# ============================================================

# Run security tests (Catch2)
security-test: cpp-build
    cd lib/gnucash-core/build && ctest --output-on-failure -R "security|identity|classify|rate|approval|anomaly|amount"

# Run MCP server with security enforcement
security-run book *args='':
    cd lib/gnucash-core/build && ./gnucash-bridge --enforce {{args}}

# Show pending approval requests for a book
security-approvals book:
    @echo "Approval DB: {{book}}.approvals.db"
    @sqlite3 "{{book}}.approvals.db" "SELECT id, agent_name, tool_name, status, created_at FROM approval_requests ORDER BY created_at DESC LIMIT 20" 2>/dev/null || echo "  (no approval DB)"

# Approve a pending request
security-approve book id approver:
    @sqlite3 "{{book}}.approvals.db" "UPDATE approval_requests SET status='approved', approver='{{approver}}', resolved_at=datetime('now') WHERE id='{{id}}' AND status='pending'"

# ============================================================
# Bank Feed Import (Phase 5)
# ============================================================

# Import OFX bank statement into a GnuCash book
import-ofx book ofx_file account_path imbalance="Imbalance-USD":
    cd lib/gnucash-core/build && echo '{"jsonrpc":"2.0","method":"tools/call","params":{"name":"gnucash_import_ofx","arguments":{"content":"'"$(cat {{ofx_file}})"'","account_path":"{{account_path}}","imbalance_account":"{{imbalance}}"}},"id":1}' | ./gnucash-bridge --book {{book}}

# Import CSV bank statement into a GnuCash book
import-csv book csv_file format account_path imbalance="Imbalance-USD":
    cd lib/gnucash-core/build && echo '{"jsonrpc":"2.0","method":"tools/call","params":{"name":"gnucash_import_csv","arguments":{"content":"'"$(cat {{csv_file}})"'","format":"{{format}}","account_path":"{{account_path}}","imbalance_account":"{{imbalance}}"}},"id":1}' | ./gnucash-bridge --book {{book}}

# Check bank feed import status for an account
bank-feed-status book account_path="":
    cd lib/gnucash-core/build && echo '{"jsonrpc":"2.0","method":"tools/call","params":{"name":"gnucash_bank_feed_status","arguments":{"account_path":"{{account_path}}"}},"id":1}' | ./gnucash-bridge --book {{book}}

# ============================================================
# Development Utilities
# ============================================================

# Show project status
status:
    @echo "=== gnucashr monorepo ==="
    @echo ""
    @echo "R Package:"
    @grep "^Version:" packages/gnucashr/DESCRIPTION
    @echo ""
    @echo "Git:"
    @git log --oneline -5
    @echo ""
    @echo "Nix:"
    @ls -la result-* 2>/dev/null || echo "  (no result symlinks)"

# Clean build artifacts
clean:
    rm -f packages/gnucashr/gnucashr_*.tar.gz
    rm -rf packages/gnucashr/gnucashr.Rcheck
    rm -rf packages/gnucashr/src/*.o packages/gnucashr/src/*.so packages/gnucashr/src/*.dll
    rm -rf lib/gnucash-core/build
    rm -f result result-*
