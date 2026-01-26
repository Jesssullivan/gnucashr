// guid.cpp - UUID/GUID generation for GnuCash
//
// GnuCash uses 32-character hex GUIDs (lowercase, no dashes).
// This module generates and validates GUIDs in the GnuCash format.

#include <Rcpp.h>
#include <random>
#include <iomanip>
#include <sstream>
using namespace Rcpp;

// Thread-local random engine for thread safety
std::mt19937& get_random_engine() {
    static thread_local std::mt19937 engine(std::random_device{}());
    return engine;
}

//' Generate GnuCash-Format GUID
//'
//' Generate a random 32-character hex GUID in GnuCash format.
//' GnuCash uses lowercase hex without dashes.
//'
//' @return Character string with 32-character hex GUID
//' @export
// [[Rcpp::export]]
String generate_guid() {
    std::mt19937& engine = get_random_engine();
    std::uniform_int_distribution<int> dist(0, 15);

    std::stringstream ss;
    ss << std::hex << std::setfill('0');

    for (int i = 0; i < 32; i++) {
        ss << dist(engine);
    }

    return ss.str();
}

//' Validate GnuCash GUID Format
//'
//' Check if a string is a valid 32-character hex GUID.
//'
//' @param guid Character string to validate
//' @return Logical indicating validity
//' @export
// [[Rcpp::export]]
bool validate_guid(String guid) {
    std::string s = guid.get_cstring();

    // Must be exactly 32 characters
    if (s.length() != 32) {
        return false;
    }

    // All characters must be hex digits
    for (char c : s) {
        if (!std::isxdigit(static_cast<unsigned char>(c))) {
            return false;
        }
    }

    return true;
}

//' Generate Multiple GUIDs
//'
//' Vectorized GUID generation for batch operations.
//'
//' @param n Number of GUIDs to generate
//' @return CharacterVector of GUIDs
//' @export
// [[Rcpp::export]]
CharacterVector generate_guids(int n) {
    CharacterVector result(n);
    std::mt19937& engine = get_random_engine();
    std::uniform_int_distribution<int> dist(0, 15);

    for (int i = 0; i < n; i++) {
        std::stringstream ss;
        ss << std::hex << std::setfill('0');
        for (int j = 0; j < 32; j++) {
            ss << dist(engine);
        }
        result[i] = ss.str();
    }

    return result;
}
