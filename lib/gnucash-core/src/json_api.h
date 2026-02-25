#pragma once
// JSON API for gnucash-core
// Provides stdin/stdout JSON-lines bridge for agent communication
//
// Protocol: one JSON object per line (JSON-lines / NDJSON)
// Request:  {"method": "...", "params": {...}, "id": 1}
// Response: {"result": {...}, "id": 1}
//           {"error": {"message": "..."}, "id": 1}

#include <nlohmann/json.hpp>
#include <string>

namespace gnucash {

using json = nlohmann::json;

// Process a single JSON request and return the response
json dispatch(const json& request);

// Run the stdin/stdout event loop (blocking)
// Reads JSON lines from stdin, dispatches, writes responses to stdout
void run_json_loop();

} // namespace gnucash
