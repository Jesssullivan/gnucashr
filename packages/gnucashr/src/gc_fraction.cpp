// Fraction arithmetic for GnuCash value storage
// Extracted from packages/gnucashr/src/fraction.cpp (Rcpp removed)

#include "gnucash/fraction.h"
#include <cstdlib>
#include <algorithm>
#include <cctype>

namespace gnucash {

Fraction Fraction::from_string(const std::string& s) {
    if (s.empty()) return {0, 100};

    // Strip whitespace, currency symbols, commas
    std::string clean;
    clean.reserve(s.size());
    bool negative = false;
    for (char c : s) {
        if (c == '-') {
            negative = !negative;
        } else if (c == '$' || c == ',' || c == ' ' || c == '\t') {
            // skip
        } else if (c == '(' || c == ')') {
            // Accounting format: (45.23) means negative
            if (c == '(') negative = !negative;
        } else {
            clean.push_back(c);
        }
    }

    if (clean.empty()) return {0, 100};

    // Find decimal point
    auto dot = clean.find('.');
    int64_t integer_part = 0;
    int64_t decimal_part = 0;
    int64_t denom = 1;

    if (dot == std::string::npos) {
        // No decimal: e.g. "45" -> 4500/100
        integer_part = std::strtoll(clean.c_str(), nullptr, 10);
        denom = 100;
    } else {
        // Has decimal: count decimal places
        std::string int_str = clean.substr(0, dot);
        std::string dec_str = clean.substr(dot + 1);

        if (!int_str.empty())
            integer_part = std::strtoll(int_str.c_str(), nullptr, 10);

        // Calculate denom based on number of decimal places
        denom = 1;
        for (size_t i = 0; i < dec_str.size(); i++) denom *= 10;

        if (!dec_str.empty())
            decimal_part = std::strtoll(dec_str.c_str(), nullptr, 10);
    }

    int64_t num = integer_part * denom + decimal_part;
    if (negative) num = -num;

    return {num, denom};
}

int64_t gcd(int64_t a, int64_t b) {
    a = std::abs(a);
    b = std::abs(b);
    while (b != 0) {
        int64_t t = b;
        b = a % b;
        a = t;
    }
    return a;
}

int64_t lcm(int64_t a, int64_t b) {
    if (a == 0 || b == 0) return 0;
    return std::abs(a / gcd(a, b) * b);
}

Fraction add(const Fraction& a, const Fraction& b) {
    int64_t common = lcm(a.denom, b.denom);
    int64_t num = a.num * (common / a.denom) + b.num * (common / b.denom);
    int64_t g = gcd(std::abs(num), common);
    return {num / g, common / g};
}

Fraction negate(const Fraction& f) {
    return {-f.num, f.denom};
}

bool splits_balance(const std::vector<Fraction>& splits) {
    if (splits.empty()) return true;

    // Find LCM of all denominators
    int64_t common_denom = splits[0].denom;
    for (size_t i = 1; i < splits.size(); i++) {
        common_denom = lcm(common_denom, splits[i].denom);
        if (common_denom == 0) return false;
    }

    // Sum numerators scaled to common denominator
    int64_t total = 0;
    for (const auto& s : splits) {
        total += s.num * (common_denom / s.denom);
    }

    return total == 0;
}

std::vector<double> fractions_to_doubles(
    const std::vector<int64_t>& nums,
    const std::vector<int64_t>& denoms) {
    std::vector<double> result(nums.size());
    for (size_t i = 0; i < nums.size(); i++) {
        if (denoms[i] == 0) {
            result[i] = 0.0;
        } else {
            result[i] = static_cast<double>(nums[i]) / static_cast<double>(denoms[i]);
        }
    }
    return result;
}

std::vector<double> running_balance(
    const std::vector<int64_t>& nums,
    const std::vector<int64_t>& denoms,
    double opening) {
    std::vector<double> result(nums.size());
    double balance = opening;
    for (size_t i = 0; i < nums.size(); i++) {
        double val = (denoms[i] != 0)
            ? static_cast<double>(nums[i]) / static_cast<double>(denoms[i])
            : 0.0;
        balance += val;
        result[i] = balance;
    }
    return result;
}

} // namespace gnucash
