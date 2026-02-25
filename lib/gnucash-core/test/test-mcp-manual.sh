#!/usr/bin/env bash
# Manual MCP protocol test
# Tests the gnucash-bridge MCP implementation

set -e

BRIDGE="./gnucash-bridge"
FIXTURE="../test/fixtures/with-accounts.gnucash"

echo "=== Testing MCP Protocol ==="
echo ""

# Test 1: Initialize
echo "Test 1: Initialize handshake"
echo '{"jsonrpc":"2.0","method":"initialize","params":{"protocolVersion":"2024-11-05","capabilities":{},"clientInfo":{"name":"test-client","version":"1.0"}},"id":1}' | $BRIDGE
echo ""

# Test 2: Initialized notification
echo "Test 2: Initialized notification (no response expected)"
echo '{"jsonrpc":"2.0","method":"initialized"}' | $BRIDGE
echo ""

# Test 3: Tools list
echo "Test 3: List available tools"
echo '{"jsonrpc":"2.0","method":"tools/list","id":2}' | $BRIDGE
echo ""

# Test 4: Ping
echo "Test 4: Ping"
echo '{"jsonrpc":"2.0","method":"ping","id":3}' | $BRIDGE
echo ""

# Test 5: Open book
echo "Test 5: Open GnuCash book"
echo "{\"jsonrpc\":\"2.0\",\"method\":\"tools/call\",\"params\":{\"name\":\"gnucash_open\",\"arguments\":{\"path\":\"$FIXTURE\",\"read_only\":true}},\"id\":4}" | $BRIDGE
echo ""

# Test 6: Get accounts
echo "Test 6: List accounts"
echo '{"jsonrpc":"2.0","method":"tools/call","params":{"name":"gnucash_get_accounts","arguments":{}},"id":5}' | $BRIDGE
echo ""

# Test 7: Unknown method
echo "Test 7: Unknown method (should error)"
echo '{"jsonrpc":"2.0","method":"unknown_method","id":6}' | $BRIDGE
echo ""

# Test 8: Legacy protocol fallback
echo "Test 8: Legacy protocol (no jsonrpc field)"
echo '{"method":"get_accounts","id":7}' | $BRIDGE
echo ""

echo "=== All tests complete ==="
