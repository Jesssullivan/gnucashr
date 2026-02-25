#!/usr/bin/env bash
# MCP protocol session test - all commands in one stdin stream

set -e

BRIDGE="./gnucash-bridge"
FIXTURE="../test/fixtures/with-accounts.gnucash"

echo "=== Testing MCP Protocol (Single Session) ==="
echo ""

# Send all commands in one stdin stream
{
    echo '{"jsonrpc":"2.0","method":"initialize","params":{"protocolVersion":"2024-11-05","capabilities":{},"clientInfo":{"name":"test-client","version":"1.0"}},"id":1}'
    echo '{"jsonrpc":"2.0","method":"initialized"}'
    echo '{"jsonrpc":"2.0","method":"ping","id":2}'
    echo "{\"jsonrpc\":\"2.0\",\"method\":\"tools/call\",\"params\":{\"name\":\"gnucash_open\",\"arguments\":{\"path\":\"$FIXTURE\",\"read_only\":true}},\"id\":3}"
    echo '{"jsonrpc":"2.0","method":"tools/call","params":{"name":"gnucash_get_accounts","arguments":{}},"id":4}'
    echo '{"jsonrpc":"2.0","method":"tools/call","params":{"name":"gnucash_account_tree","arguments":{}},"id":5}'
    echo '{"jsonrpc":"2.0","method":"tools/call","params":{"name":"gnucash_info","arguments":{}},"id":6}'
    echo '{"jsonrpc":"2.0","method":"tools/call","params":{"name":"gnucash_close","arguments":{}},"id":7}'
} | $BRIDGE | while IFS= read -r line; do
    echo "$line" | jq -c '
        if .error then
            {id, error: .error.message}
        elif .result.content then
            {id, result: "content"}
        elif .result.tools then
            {id, result: "tools_list", count: (.result.tools | length)}
        elif .result then
            {id, result}
        else
            .
        end
    '
done

echo ""
echo "=== Session test complete ==="
