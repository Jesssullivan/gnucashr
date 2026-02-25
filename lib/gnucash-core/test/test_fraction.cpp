#include <catch2/catch_test_macros.hpp>
#include <catch2/matchers/catch_matchers_floating_point.hpp>
#include "gnucash/fraction.h"

using namespace gnucash;
using Catch::Matchers::WithinAbs;

TEST_CASE("gcd", "[fraction]") {
    REQUIRE(gcd(12, 8) == 4);
    REQUIRE(gcd(100, 75) == 25);
    REQUIRE(gcd(17, 13) == 1);
    REQUIRE(gcd(0, 5) == 5);
    REQUIRE(gcd(5, 0) == 5);
    REQUIRE(gcd(-12, 8) == 4);
}

TEST_CASE("lcm", "[fraction]") {
    REQUIRE(lcm(4, 6) == 12);
    REQUIRE(lcm(3, 5) == 15);
    REQUIRE(lcm(0, 5) == 0);
    REQUIRE(lcm(100, 100) == 100);
}

TEST_CASE("Fraction construction", "[fraction]") {
    Fraction f(500, 100);
    REQUIRE(f.num == 500);
    REQUIRE(f.denom == 100);
}

TEST_CASE("Fraction default construction", "[fraction]") {
    Fraction f;
    REQUIRE(f.num == 0);
    REQUIRE(f.denom == 1);
}

TEST_CASE("Fraction to_double", "[fraction]") {
    REQUIRE_THAT(Fraction(500, 100).to_double(), WithinAbs(5.0, 1e-9));
    REQUIRE_THAT(Fraction(1, 3).to_double(), WithinAbs(0.333333, 1e-4));
    REQUIRE_THAT(Fraction(0, 100).to_double(), WithinAbs(0.0, 1e-9));
    // Zero denominator returns 0
    REQUIRE_THAT(Fraction(5, 0).to_double(), WithinAbs(0.0, 1e-9));
}

TEST_CASE("Fraction from_double", "[fraction]") {
    auto f = Fraction::from_double(5.0);
    REQUIRE(f.num == 500);
    REQUIRE(f.denom == 100);

    auto f2 = Fraction::from_double(3.14159, 100000);
    REQUIRE(f2.num == 314159);
    REQUIRE(f2.denom == 100000);
}

TEST_CASE("Fraction is_zero", "[fraction]") {
    REQUIRE(Fraction(0, 100).is_zero());
    REQUIRE_FALSE(Fraction(1, 100).is_zero());
}

TEST_CASE("Fraction add", "[fraction]") {
    // 1/4 + 1/4 = 1/2
    auto r = add(Fraction(1, 4), Fraction(1, 4));
    REQUIRE_THAT(r.to_double(), WithinAbs(0.5, 1e-9));

    // 1/3 + 1/6 = 1/2
    auto r2 = add(Fraction(1, 3), Fraction(1, 6));
    REQUIRE_THAT(r2.to_double(), WithinAbs(0.5, 1e-9));

    // 500/100 + 300/100 = 800/100 = 8
    auto r3 = add(Fraction(500, 100), Fraction(300, 100));
    REQUIRE_THAT(r3.to_double(), WithinAbs(8.0, 1e-9));
}

TEST_CASE("Fraction negate", "[fraction]") {
    auto f = negate(Fraction(500, 100));
    REQUIRE(f.num == -500);
    REQUIRE(f.denom == 100);
}

TEST_CASE("splits_balance balanced", "[fraction]") {
    // Simple two-split: +100 and -100
    std::vector<Fraction> splits = {{10000, 100}, {-10000, 100}};
    REQUIRE(splits_balance(splits));
}

TEST_CASE("splits_balance unbalanced", "[fraction]") {
    std::vector<Fraction> splits = {{10000, 100}, {-5000, 100}};
    REQUIRE_FALSE(splits_balance(splits));
}

TEST_CASE("splits_balance empty", "[fraction]") {
    std::vector<Fraction> splits;
    REQUIRE(splits_balance(splits));
}

TEST_CASE("splits_balance multi-split", "[fraction]") {
    // Salary $3000 -> Checking $2000, Savings $1000
    std::vector<Fraction> splits = {
        {-300000, 100},  // Salary (income, negative)
        {200000, 100},   // Checking
        {100000, 100},   // Savings
    };
    REQUIRE(splits_balance(splits));
}

TEST_CASE("splits_balance mixed denominators", "[fraction]") {
    // 1/3 + 2/3 - 1/1 = 0
    std::vector<Fraction> splits = {{1, 3}, {2, 3}, {-1, 1}};
    REQUIRE(splits_balance(splits));
}

TEST_CASE("fractions_to_doubles", "[fraction]") {
    std::vector<int64_t> nums = {500, 1000, -200};
    std::vector<int64_t> denoms = {100, 100, 100};
    auto result = fractions_to_doubles(nums, denoms);
    REQUIRE(result.size() == 3);
    REQUIRE_THAT(result[0], WithinAbs(5.0, 1e-9));
    REQUIRE_THAT(result[1], WithinAbs(10.0, 1e-9));
    REQUIRE_THAT(result[2], WithinAbs(-2.0, 1e-9));
}

TEST_CASE("fractions_to_doubles zero denom", "[fraction]") {
    std::vector<int64_t> nums = {500};
    std::vector<int64_t> denoms = {0};
    auto result = fractions_to_doubles(nums, denoms);
    REQUIRE_THAT(result[0], WithinAbs(0.0, 1e-9));
}

TEST_CASE("running_balance", "[fraction]") {
    std::vector<int64_t> nums = {1000, 500, -200};
    std::vector<int64_t> denoms = {100, 100, 100};
    auto result = running_balance(nums, denoms, 0.0);
    REQUIRE(result.size() == 3);
    REQUIRE_THAT(result[0], WithinAbs(10.0, 1e-9));
    REQUIRE_THAT(result[1], WithinAbs(15.0, 1e-9));
    REQUIRE_THAT(result[2], WithinAbs(13.0, 1e-9));
}

TEST_CASE("running_balance with opening", "[fraction]") {
    std::vector<int64_t> nums = {500};
    std::vector<int64_t> denoms = {100};
    auto result = running_balance(nums, denoms, 100.0);
    REQUIRE_THAT(result[0], WithinAbs(105.0, 1e-9));
}
