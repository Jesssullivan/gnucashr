#pragma once
// Fraction arithmetic for GnuCash value storage
// Extracted from packages/gnucashr/src/fraction.cpp

#include <cstdint>
#include <cmath>
#include <string>
#include <vector>

namespace gnucash {

struct Fraction {
    int64_t num;
    int64_t denom;

    Fraction() : num(0), denom(1) {}
    Fraction(int64_t n, int64_t d) : num(n), denom(d) {}

    double to_double() const {
        if (denom == 0) return 0.0;
        return static_cast<double>(num) / static_cast<double>(denom);
    }

    static Fraction from_double(double val, int64_t denom = 100) {
        return {static_cast<int64_t>(std::round(val * denom)), denom};
    }

    // Parse amount string to Fraction, avoiding double precision loss
    // Handles: "45.23", "-45.23", "$45.23", "$-45.23", "-$45.23", "1,234.56"
    // Returns Fraction with denom = 10^(decimal places), e.g. -4523/100
    static Fraction from_string(const std::string& s);

    bool is_zero() const { return num == 0; }

    bool operator==(const Fraction& o) const {
        return num == o.num && denom == o.denom;
    }
    bool operator!=(const Fraction& o) const { return !(*this == o); }
};

// Arithmetic
int64_t gcd(int64_t a, int64_t b);
int64_t lcm(int64_t a, int64_t b);
Fraction add(const Fraction& a, const Fraction& b);
Fraction negate(const Fraction& f);

// Validation
bool splits_balance(const std::vector<Fraction>& splits);

// Batch operations
std::vector<double> fractions_to_doubles(
    const std::vector<int64_t>& nums,
    const std::vector<int64_t>& denoms);

std::vector<double> running_balance(
    const std::vector<int64_t>& nums,
    const std::vector<int64_t>& denoms,
    double opening = 0.0);

} // namespace gnucash
