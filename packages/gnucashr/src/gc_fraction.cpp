// Fraction arithmetic for GnuCash value storage
// Extracted from packages/gnucashr/src/fraction.cpp (Rcpp removed)

#include "gnucash/fraction.h"
#include <cstdlib>

namespace gnucash {

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
