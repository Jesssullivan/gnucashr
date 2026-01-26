// validation.cpp - Transaction validation for GnuCash
//
// High-performance validation of double-entry accounting constraints.
// These functions are called frequently during transaction posting.

#include <Rcpp.h>
using namespace Rcpp;

//' Validate Transaction Balance
//'
//' Check that a set of split values sum to zero (double-entry principle).
//' Uses exact integer arithmetic to avoid floating-point errors.
//'
//' @param value_nums Integer vector of split value numerators
//' @param value_denoms Integer vector of split value denominators
//' @param tolerance Maximum allowed imbalance (default 0, strict)
//' @return List with: balanced (bool), total (double), message (string)
//' @export
// [[Rcpp::export]]
List validate_transaction_balance(IntegerVector value_nums,
                                   IntegerVector value_denoms,
                                   double tolerance = 0.0) {
    int n = value_nums.size();
    if (n != value_denoms.size()) {
        return List::create(
            Named("balanced") = false,
            Named("total") = NA_REAL,
            Named("message") = "Numerator and denominator vectors have different lengths"
        );
    }

    if (n < 2) {
        return List::create(
            Named("balanced") = false,
            Named("total") = NA_REAL,
            Named("message") = "Transaction must have at least 2 splits"
        );
    }

    // Find LCM of all denominators for exact arithmetic
    int64_t lcm = 1;
    for (int i = 0; i < n; i++) {
        if (IntegerVector::is_na(value_denoms[i]) || value_denoms[i] == 0) {
            return List::create(
                Named("balanced") = false,
                Named("total") = NA_REAL,
                Named("message") = "Invalid denominator (NA or zero)"
            );
        }
        int64_t d = value_denoms[i];
        int64_t g = lcm;
        while (d != 0) {
            int64_t t = d;
            d = g % d;
            g = t;
        }
        lcm = (lcm / g) * value_denoms[i];
    }

    // Sum all values scaled to common denominator
    int64_t total = 0;
    for (int i = 0; i < n; i++) {
        if (IntegerVector::is_na(value_nums[i])) {
            return List::create(
                Named("balanced") = false,
                Named("total") = NA_REAL,
                Named("message") = "Invalid numerator (NA)"
            );
        }
        total += static_cast<int64_t>(value_nums[i]) * (lcm / value_denoms[i]);
    }

    double total_decimal = static_cast<double>(total) / static_cast<double>(lcm);
    bool balanced = (total == 0) || (std::abs(total_decimal) <= tolerance);

    std::string message;
    if (balanced) {
        message = "Transaction is balanced";
    } else {
        char buf[100];
        snprintf(buf, sizeof(buf), "Transaction is out of balance by %.4f", total_decimal);
        message = buf;
    }

    return List::create(
        Named("balanced") = balanced,
        Named("total") = total_decimal,
        Named("message") = message
    );
}

//' Validate Account GUID Format
//'
//' Check if a string is a valid GnuCash GUID (32 hex characters).
//'
//' @param guids Character vector of GUIDs to validate
//' @return Logical vector indicating validity
//' @export
// [[Rcpp::export]]
LogicalVector validate_guids(CharacterVector guids) {
    int n = guids.size();
    LogicalVector result(n);

    for (int i = 0; i < n; i++) {
        if (CharacterVector::is_na(guids[i])) {
            result[i] = false;
            continue;
        }

        std::string s = Rcpp::as<std::string>(guids[i]);

        // Must be exactly 32 characters
        if (s.length() != 32) {
            result[i] = false;
            continue;
        }

        // All characters must be hex digits
        bool valid = true;
        for (char c : s) {
            if (!std::isxdigit(static_cast<unsigned char>(c))) {
                valid = false;
                break;
            }
        }
        result[i] = valid;
    }

    return result;
}

//' Check for Duplicate GUIDs
//'
//' Verify that all GUIDs in a vector are unique.
//'
//' @param guids Character vector of GUIDs
//' @return List with: unique (bool), duplicates (character vector)
//' @export
// [[Rcpp::export]]
List check_guid_uniqueness(CharacterVector guids) {
    int n = guids.size();
    std::set<std::string> seen;
    std::vector<std::string> duplicates;

    for (int i = 0; i < n; i++) {
        if (!CharacterVector::is_na(guids[i])) {
            std::string s = Rcpp::as<std::string>(guids[i]);
            if (seen.find(s) != seen.end()) {
                duplicates.push_back(s);
            } else {
                seen.insert(s);
            }
        }
    }

    return List::create(
        Named("unique") = duplicates.empty(),
        Named("n_duplicates") = duplicates.size(),
        Named("duplicates") = wrap(duplicates)
    );
}

//' Validate Split Values
//'
//' Check that split values are valid (non-zero, proper fractions).
//'
//' @param value_nums Integer vector of numerators
//' @param value_denoms Integer vector of denominators
//' @return List with validation results
//' @export
// [[Rcpp::export]]
List validate_split_values(IntegerVector value_nums, IntegerVector value_denoms) {
    int n = value_nums.size();
    if (n != value_denoms.size()) {
        return List::create(
            Named("valid") = false,
            Named("message") = "Vectors have different lengths"
        );
    }

    std::vector<int> zero_indices;
    std::vector<int> invalid_denom_indices;
    std::vector<int> na_indices;

    for (int i = 0; i < n; i++) {
        if (IntegerVector::is_na(value_nums[i]) || IntegerVector::is_na(value_denoms[i])) {
            na_indices.push_back(i + 1);  // 1-indexed for R
            continue;
        }

        if (value_denoms[i] <= 0) {
            invalid_denom_indices.push_back(i + 1);
            continue;
        }

        if (value_nums[i] == 0) {
            zero_indices.push_back(i + 1);
        }
    }

    bool valid = na_indices.empty() && invalid_denom_indices.empty();

    std::string message = "Valid";
    if (!na_indices.empty()) {
        message = "Contains NA values";
    } else if (!invalid_denom_indices.empty()) {
        message = "Contains invalid denominators";
    } else if (!zero_indices.empty()) {
        message = "Contains zero values (warning)";
    }

    return List::create(
        Named("valid") = valid,
        Named("message") = message,
        Named("zero_value_indices") = wrap(zero_indices),
        Named("invalid_denom_indices") = wrap(invalid_denom_indices),
        Named("na_indices") = wrap(na_indices)
    );
}

//' Calculate Running Balance
//'
//' Calculate running balance for a series of transactions.
//' Optimized for large transaction histories.
//'
//' @param value_nums Integer vector of value numerators
//' @param value_denoms Integer vector of value denominators
//' @param opening_balance Starting balance (default 0)
//' @return NumericVector of running balances
//' @export
// [[Rcpp::export]]
NumericVector calculate_running_balance(IntegerVector value_nums,
                                         IntegerVector value_denoms,
                                         double opening_balance = 0.0) {
    int n = value_nums.size();
    if (n != value_denoms.size()) {
        stop("Numerator and denominator vectors must have same length");
    }

    NumericVector balances(n);
    double running = opening_balance;

    for (int i = 0; i < n; i++) {
        if (IntegerVector::is_na(value_nums[i]) || IntegerVector::is_na(value_denoms[i])) {
            balances[i] = running;  // Keep previous balance on NA
        } else if (value_denoms[i] == 0) {
            balances[i] = running;  // Keep previous on invalid denom
        } else {
            double value = static_cast<double>(value_nums[i]) /
                           static_cast<double>(value_denoms[i]);
            running += value;
            balances[i] = running;
        }
    }

    return balances;
}
