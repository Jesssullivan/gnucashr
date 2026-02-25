#include "gnucash/mcp.h"

int main() {
    // Use MCP protocol (with legacy fallback)
    gnucash::mcp::run_mcp_loop();
    return 0;
}
