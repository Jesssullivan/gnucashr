// fraction.cpp - High-performance fraction arithmetic for GnuCash
//
// GnuCash stores monetary values as fractions (value_num/value_denom) to avoid
// floating-point errors. This Rcpp module provides fast vectorized conversion.

#include <Rcpp.h>
using namespace Rcpp;

// Greatest common divisor using Euclidean algorithm
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

// Least common multiple
int64_t lcm(int64_t a, int64_t b) {
    if (a == 0 || b == 0) return 0;
    return std::abs(a / gcd(a, b) * b);
}

//' Convert Fractions to Double
//'
//' Vectorized conversion of GnuCash fraction representation to double.
//' This is a hot path in balance calculations, hence Rcpp implementation.
//'
//' @param numerator Integer vector of numerators (value_num)
//' @param denominator Integer vector of denominators (value_denom)
//' @return NumericVector of double values
//' @export
// [[Rcpp::export]]
NumericVector fraction_to_double(IntegerVector numerator, IntegerVector denominator) {
    int n = numerator.size();
    if (n != denominator.size()) {
        stop("numerator and denominator must have same length");
    }

    NumericVector result(n);

    for (int i = 0; i < n; i++) {
        if (IntegerVector::is_na(numerator[i]) || IntegerVector::is_na(denominator[i])) {
            result[i] = NA_REAL;
        } else if (denominator[i] == 0) {
            result[i] = NA_REAL;
        } else {
            result[i] = static_cast<double>(numerator[i]) / static_cast<double>(denominator[i]);
        }
    }

    return result;
}

//' Convert Double to Fraction
//'
//' Convert decimal values to fractions with specified precision.
//' Default denominator of 100 for currency (cents precision).
//'
//' @param value NumericVector of decimal values
//' @param denom Target denominator (default 100 for cents)
//' @return IntegerMatrix with columns: numerator, denominator
//' @export
// [[Rcpp::export]]
IntegerMatrix double_to_fraction(NumericVector value, int denom = 100) {
    int n = value.size();
    IntegerMatrix result(n, 2);
    colnames(result) = CharacterVector::create("numerator", "denominator");

    for (int i = 0; i < n; i++) {
        if (NumericVector::is_na(value[i])) {
            result(i, 0) = NA_INTEGER;
            result(i, 1) = NA_INTEGER;
        } else {
            // Round to nearest integer after scaling
            result(i, 0) = static_cast<int>(std::round(value[i] * denom));
            result(i, 1) = denom;
        }
    }

    return result;
}

//' Add Two Fractions
//'
//' Vectorized fraction addition maintaining exact representation.
//' Used for accumulating balances without floating-point drift.
//'
//' @param num1 First numerator vector
//' @param denom1 First denominator vector
//' @param num2 Second numerator vector
//' @param denom2 Second denominator vector
//' @return IntegerMatrix with columns: numerator, denominator
//' @export
// [[Rcpp::export]]
IntegerMatrix add_fractions(IntegerVector num1, IntegerVector denom1,
                            IntegerVector num2, IntegerVector denom2) {
    int n = num1.size();
    if (n != denom1.size() || n != num2.size() || n != denom2.size()) {
        stop("All vectors must have same length");
    }

    IntegerMatrix result(n, 2);
    colnames(result) = CharacterVector::create("numerator", "denominator");

    for (int i = 0; i < n; i++) {
        if (IntegerVector::is_na(num1[i]) || IntegerVector::is_na(denom1[i]) ||
            IntegerVector::is_na(num2[i]) || IntegerVector::is_na(denom2[i])) {
            result(i, 0) = NA_INTEGER;
            result(i, 1) = NA_INTEGER;
        } else {
            // Find common denominator
            int64_t d1 = denom1[i];
            int64_t d2 = denom2[i];
            int64_t common = lcm(d1, d2);

            // Scale numerators
            int64_t n1 = static_cast<int64_t>(num1[i]) * (common / d1);
            int64_t n2 = static_cast<int64_t>(num2[i]) * (common / d2);

            // Add and reduce
            int64_t sum = n1 + n2;
            int64_t g = gcd(sum, common);

            result(i, 0) = static_cast<int>(sum / g);
            result(i, 1) = static_cast<int>(common / g);
        }
    }

    return result;
}

//' Validate Splits Balance
//'
//' Verify that splits in a transaction sum to zero (double-entry principle).
//' Returns TRUE if balanced, FALSE otherwise.
//'
//' @param numerators Integer vector of split value_num
//' @param denominators Integer vector of split value_denom
//' @return Logical indicating if splits balance to zero
//' @export
// [[Rcpp::export]]
bool validate_splits_balance(IntegerVector numerators, IntegerVector denominators) {
    int n = numerators.size();
    if (n != denominators.size()) {
        stop("numerators and denominators must have same length");
    }

    if (n == 0) return true;

    // Find LCM of all denominators
    int64_t common = denominators[0];
    for (int i = 1; i < n; i++) {
        if (!IntegerVector::is_na(denominators[i])) {
            common = lcm(common, static_cast<int64_t>(denominators[i]));
        }
    }

    // Sum all numerators scaled to common denominator
    int64_t sum = 0;
    for (int i = 0; i < n; i++) {
        if (!IntegerVector::is_na(numerators[i]) && !IntegerVector::is_na(denominators[i])) {
            sum += static_cast<int64_t>(numerators[i]) * (common / denominators[i]);
        }
    }

    return sum == 0;
}
