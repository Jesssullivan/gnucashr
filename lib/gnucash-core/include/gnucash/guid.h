#pragma once
// GnuCash GUID generation and validation
// 32 lowercase hex chars, no dashes
// Extracted from packages/gnucashr/src/guid.cpp

#include <string>
#include <vector>

namespace gnucash {

std::string generate_guid();
std::vector<std::string> generate_guids(int n);
bool validate_guid(const std::string& guid);

} // namespace gnucash
