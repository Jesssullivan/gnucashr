# Reference Implementations

This directory contains reference implementations for design patterns and protocols used in gnucashr.

## gnucash-mcp/

**Source**: https://github.com/Jesssullivan/gnucash-mcp
**Language**: Python
**Status**: Reference only - not compiled or run

Python-based MCP server using `piecash` (Python GnuCash library) and FastMCP.

### What We're Reusing from This Implementation

1. **Audit Logging Pattern** (`logging_config.py`)
   - Before/after state capture decorator
   - Dual format (text + JSON)
   - Classification system (read vs write)

2. **Tool Module Architecture** (`server.py`)
   - Modular tool grouping for context efficiency
   - 52 tools split into 7 categories

3. **Compact Output Format**
   - Tab-separated one-line strings reduce token usage
   - Pattern applicable to C++ MCP responses

4. **GUID Resolution**
   - Partial GUID prefix matching (8+ chars)
   - Can be ported to C++ library

5. **Multi-Currency Support Spec** (`docs/MULTI_CURRENCY_SPEC.md`)
   - Well-documented approach to cross-currency transactions
   - Directly applicable to gnucashr

### What We're Building Instead

**gnucash-mcp-cpp** (lib/gnucash-core/src/mcp_*)
- C++ implementation on gnucash-core library (not piecash)
- JSON-RPC 2.0 protocol (not FastMCP)
- 19 tools (focused on core operations, not comprehensive catalog)
- Aperture-compatible audit trail
- Dhall-based agent configuration

### How to Use This Reference

When implementing MCP features, consult the Python code for:
- Protocol patterns (tool registration, error handling)
- Audit log design decisions
- Output formatting strategies
- Multi-currency edge cases

**Do not**: Copy Python code directly - the C++ implementation is intentionally different.
