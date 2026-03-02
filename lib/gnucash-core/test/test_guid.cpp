#include <catch2/catch_test_macros.hpp>
#include "gnucash/guid.h"
#include <set>
#include <cctype>

using namespace gnucash;

TEST_CASE("generate_guid length", "[guid]") {
    auto guid = generate_guid();
    REQUIRE(guid.length() == 32);
}

TEST_CASE("generate_guid hex characters only", "[guid]") {
    auto guid = generate_guid();
    for (char c : guid) {
        REQUIRE(std::isxdigit(static_cast<unsigned char>(c)));
    }
}

TEST_CASE("generate_guid uniqueness", "[guid]") {
    std::set<std::string> guids;
    for (int i = 0; i < 100; i++) {
        guids.insert(generate_guid());
    }
    REQUIRE(guids.size() == 100);
}

TEST_CASE("generate_guids", "[guid]") {
    auto guids = generate_guids(10);
    REQUIRE(guids.size() == 10);
    for (const auto& g : guids) {
        REQUIRE(g.length() == 32);
        REQUIRE(validate_guid(g));
    }
}

TEST_CASE("validate_guid valid", "[guid]") {
    REQUIRE(validate_guid("0123456789abcdef0123456789abcdef"));
    REQUIRE(validate_guid("AABBCCDD11223344AABBCCDD11223344"));
    REQUIRE(validate_guid("f0000000000000000000000000000001"));
}

TEST_CASE("validate_guid invalid", "[guid]") {
    // Too short
    REQUIRE_FALSE(validate_guid("0123456789abcdef"));
    // Too long
    REQUIRE_FALSE(validate_guid("0123456789abcdef0123456789abcdef0"));
    // Non-hex characters
    REQUIRE_FALSE(validate_guid("0123456789abcdef0123456789abcdeg"));
    // Empty
    REQUIRE_FALSE(validate_guid(""));
    // With dashes (UUID format, not GnuCash format)
    REQUIRE_FALSE(validate_guid("01234567-89ab-cdef-0123-456789abcdef"));
}
