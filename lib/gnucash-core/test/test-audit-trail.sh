#!/usr/bin/env bash
# Test audit trail system

set -e

BRIDGE="./gnucash-bridge"
FIXTURE="../test/fixtures/with-accounts.gnucash"
TEST_BOOK="/tmp/test-audit-$$.gnucash"

echo "=== Testing Audit Trail System ==="
echo ""

# Copy fixture to temp location (so we can write)
cp "$FIXTURE" "$TEST_BOOK"

echo "Test book: $TEST_BOOK"
echo "Audit DB will be: $TEST_BOOK.audit.db"
echo ""

# Send commands that should be audited
{
    echo '{"jsonrpc":"2.0","method":"initialize","params":{"protocolVersion":"2024-11-05","capabilities":{},"clientInfo":{"name":"test-client","version":"1.0"}},"id":1}'
    echo "{\"jsonrpc\":\"2.0\",\"method\":\"tools/call\",\"params\":{\"name\":\"gnucash_open\",\"arguments\":{\"path\":\"$TEST_BOOK\",\"read_only\":true}},\"id\":2}"
    echo '{"jsonrpc":"2.0","method":"tools/call","params":{"name":"gnucash_get_accounts","arguments":{}},"id":3}'
    echo '{"jsonrpc":"2.0","method":"tools/call","params":{"name":"gnucash_info","arguments":{}},"id":4}'
    echo '{"jsonrpc":"2.0","method":"tools/call","params":{"name":"gnucash_audit_log","arguments":{"limit":10}},"id":5}'
    echo '{"jsonrpc":"2.0","method":"tools/call","params":{"name":"gnucash_close","arguments":{}},"id":6}'
} | $BRIDGE | while IFS= read -r line; do
    # Show request id and result type
    echo "$line" | jq -c '
        if .id == 5 and .result.content then
            # Print audit log content
            {id, audit_log: (.result.content[0].text | split("\n") | .[0:3])}
        elif .error then
            {id, error: .error.message}
        elif .result.content then
            {id, result: "content"}
        elif .result then
            {id, result}
        else
            .
        end
    '
done

echo ""
echo "=== Checking audit database directly ==="
if [ -f "$TEST_BOOK.audit.db" ]; then
    echo "Audit database created: $TEST_BOOK.audit.db"
    echo ""
    echo "Audit log contents:"
    sqlite3 "$TEST_BOOK.audit.db" "SELECT timestamp, tool_name, classification, result_status, duration_ms FROM audit_log ORDER BY id" | head -10
    echo ""
    echo "Total audit records: $(sqlite3 "$TEST_BOOK.audit.db" "SELECT COUNT(*) FROM audit_log")"
else
    echo "ERROR: Audit database not created!"
fi

echo ""
echo "=== Cleanup ==="
rm -f "$TEST_BOOK" "$TEST_BOOK.audit.db"
echo "Test complete"
