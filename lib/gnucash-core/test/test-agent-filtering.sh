#!/usr/bin/env bash
# Test agent configuration and tool filtering

set -e

BRIDGE="./gnucash-bridge"
AGENT_CONFIG="/home/jsullivan2/git/gnucashr/dhall/agents/spend-monitor.dhall"

echo "=== Test 1: No agent config (all 20 tools available) ==="
echo '{"jsonrpc":"2.0","method":"tools/list","id":1}' | $BRIDGE | jq -c '{tool_count: (.result.tools | length), sample_tools: [.result.tools[0].name, .result.tools[1].name, .result.tools[2].name]}'
echo ""

echo "=== Test 2: spend-monitor agent (6 tools only) ==="
echo '{"jsonrpc":"2.0","method":"tools/list","id":1}' | $BRIDGE --agent "$AGENT_CONFIG" 2>/dev/null | jq -c '{tool_count: (.result.tools | length), tools: [.result.tools[].name]}'
echo ""

echo "=== Test 3: Help message ==="
$BRIDGE --help 2>&1 | head -5
echo ""

echo "=== Test 4: Invalid agent config ==="
echo '{"jsonrpc":"2.0","method":"tools/list","id":1}' | $BRIDGE --agent "/nonexistent.dhall" 2>&1 | head -3
echo ""

echo "=== All tests complete ==="
