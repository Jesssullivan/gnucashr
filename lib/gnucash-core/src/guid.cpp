// GnuCash GUID generation and validation
// Extracted from packages/gnucashr/src/guid.cpp (Rcpp removed)

#include "gnucash/guid.h"
#include <random>
#include <sstream>
#include <iomanip>
#include <cctype>

namespace gnucash {

namespace {
    std::mt19937& get_random_engine() {
        thread_local std::mt19937 gen(std::random_device{}());
        return gen;
    }
}

std::string generate_guid() {
    auto& gen = get_random_engine();
    std::uniform_int_distribution<int> dist(0, 15);

    std::ostringstream ss;
    ss << std::hex;
    for (int i = 0; i < 32; i++) {
        ss << dist(gen);
    }
    return ss.str();
}

std::vector<std::string> generate_guids(int n) {
    std::vector<std::string> guids;
    guids.reserve(n);
    for (int i = 0; i < n; i++) {
        guids.push_back(generate_guid());
    }
    return guids;
}

bool validate_guid(const std::string& guid) {
    if (guid.length() != 32) return false;
    for (char c : guid) {
        if (!std::isxdigit(static_cast<unsigned char>(c))) return false;
    }
    return true;
}

} // namespace gnucash
