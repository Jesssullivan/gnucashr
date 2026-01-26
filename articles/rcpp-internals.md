# Rcpp Internals

This article documents the Rcpp implementation details in gnucashr. It
is intended for contributors who want to understand, modify, or extend
the C++ code.

## Overview

gnucashr uses Rcpp and RcppParallel for performance-critical operations:

| File                         | Purpose                    | Threading        |
|------------------------------|----------------------------|------------------|
| `src/fraction.cpp`           | Fraction arithmetic        | Single-threaded  |
| `src/guid.cpp`               | GUID generation/validation | Thread-local RNG |
| `src/monte-carlo.cpp`        | Monte Carlo simulation     | RcppParallel     |
| `src/parallel-scenarios.cpp` | Scenario projection        | RcppParallel     |
| `src/validation.cpp`         | Transaction validation     | Single-threaded  |

## Build Dependencies

The package requires:

    LinkingTo:
        Rcpp,
        RcppParallel,
        RcppArmadillo
    SystemRequirements: GNU make, C++17

## Fraction Arithmetic (fraction.cpp)

GnuCash stores monetary values as fractions to avoid floating-point
errors. These functions provide vectorized conversion with exact
arithmetic.

### Core Functions

``` cpp
// Greatest common divisor (Euclidean algorithm)
int64_t gcd(int64_t a, int64_t b);

// Least common multiple
int64_t lcm(int64_t a, int64_t b);
```

### Exported Functions

**`fraction_to_double(numerator, denominator)`**

Converts GnuCash fraction representation to double. This is a hot path
in balance calculations.

``` cpp
// [[Rcpp::export]]
NumericVector fraction_to_double(IntegerVector numerator,
                                  IntegerVector denominator) {
    int n = numerator.size();
    NumericVector result(n);

    for (int i = 0; i < n; i++) {
        if (IntegerVector::is_na(numerator[i]) ||
            IntegerVector::is_na(denominator[i]) ||
            denominator[i] == 0) {
            result[i] = NA_REAL;
        } else {
            result[i] = static_cast<double>(numerator[i]) /
                        static_cast<double>(denominator[i]);
        }
    }
    return result;
}
```

Key implementation details:

- NA propagation for missing values
- Zero denominator returns NA (not Inf)
- Static cast to double before division to avoid integer overflow

**`add_fractions(num1, denom1, num2, denom2)`**

Adds fractions using exact integer arithmetic with LCM-based common
denominator. Returns reduced fractions.

``` cpp
// Find common denominator
int64_t common = lcm(d1, d2);

// Scale numerators
int64_t n1 = num1[i] * (common / d1);
int64_t n2 = num2[i] * (common / d2);

// Add and reduce
int64_t sum = n1 + n2;
int64_t g = gcd(sum, common);
result_num = sum / g;
result_denom = common / g;
```

## GUID Generation (guid.cpp)

GnuCash uses 32-character lowercase hex GUIDs without dashes.

### Thread Safety

Uses thread-local random engine for safe concurrent generation:

``` cpp
std::mt19937& get_random_engine() {
    static thread_local std::mt19937 engine(std::random_device{}());
    return engine;
}
```

The `thread_local` keyword ensures each thread gets its own RNG
instance, preventing data races in parallel code.

### GUID Format

``` cpp
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
```

### Batch Generation

`generate_guids(n)` generates multiple GUIDs efficiently by reusing the
same RNG instance.

## Monte Carlo Simulation (monte-carlo.cpp)

The Monte Carlo module uses RcppParallel for embarrassingly parallel
simulation.

### RcppParallel Worker Pattern

``` cpp
// [[Rcpp::depends(RcppParallel)]]
#include <RcppParallel.h>

struct MonteCarloWorker : public Worker {
    // Input parameters (const references)
    const double base_revenue;
    // ... other parameters

    // Output matrices (RMatrix/RVector for thread-safe access)
    RMatrix<double> results;
    RVector<double> final_cash;

    // Constructor
    MonteCarloWorker(...) : ... {}

    // Worker function called for each grain
    void operator()(std::size_t begin, std::size_t end) {
        // Thread-local RNG with deterministic seed
        std::mt19937_64 rng(master_seed + begin);

        for (std::size_t sim = begin; sim < end; ++sim) {
            // Perform simulation
        }
    }
};
```

### Deterministic Seeding

For reproducibility, each thread’s RNG is seeded deterministically:

``` cpp
std::mt19937_64 rng(master_seed + begin);
```

This means:

- Same `seed` parameter produces identical results
- Results are reproducible across runs
- Different grain boundaries produce different results (expected with
  parallelism)

### Parallel Execution

``` cpp
// Create worker
MonteCarloWorker worker(..., results, final_cash);

// Execute in parallel
parallelFor(0, n_sims, worker);
```

RcppParallel automatically:

- Divides work into grains
- Schedules grains across available cores
- Joins results when complete

### Output Matrices

Use `RMatrix<double>` and `RVector<double>` for thread-safe concurrent
writes:

``` cpp
// Safe: RMatrix handles concurrent access
results(sim, period) = cumulative_cash;

// UNSAFE: Don't use raw Rcpp types in parallel code
// NumericMatrix results;  // NOT thread-safe
```

## Parallel Scenarios (parallel-scenarios.cpp)

### Scenario Projection Worker

Projects multiple growth scenarios across entities:

``` cpp
struct ScenarioProjectionWorker : public Worker {
    const RVector<double> base_values;
    const RMatrix<double> growth_matrix;  // scenarios x entities
    const int n_periods;

    RMatrix<double> results;  // scenarios x (entities * periods)
    RVector<double> final_totals;

    void operator()(std::size_t begin, std::size_t end) {
        for (std::size_t scenario = begin; scenario < end; ++scenario) {
            for (int entity = 0; entity < n_entities; ++entity) {
                // Project growth for this scenario/entity combination
            }
        }
    }
};
```

### Sensitivity Grid Worker

Computes outcomes across a 2D parameter grid:

``` cpp
struct SensitivityGridWorker : public Worker {
    const RVector<double> growth_range;
    const RVector<double> expense_range;

    RMatrix<double> outcomes;  // growth x expense

    void operator()(std::size_t begin, std::size_t end) {
        for (std::size_t idx = begin; idx < end; ++idx) {
            // Convert linear index to 2D grid coordinates
            int g_idx = idx / n_expense;
            int e_idx = idx % n_expense;

            // Compute outcome for this parameter combination
        }
    }
};
```

The linear-to-2D index conversion allows parallel iteration over the
grid.

## Validation (validation.cpp)

Transaction validation uses exact integer arithmetic to verify
double-entry balance.

### Balance Validation Algorithm

``` cpp
// [[Rcpp::export]]
List validate_transaction_balance(IntegerVector value_nums,
                                   IntegerVector value_denoms,
                                   double tolerance = 0.0) {
    // 1. Find LCM of all denominators
    int64_t lcm = 1;
    for (int i = 0; i < n; i++) {
        // Compute running LCM
    }

    // 2. Sum all values scaled to common denominator
    int64_t total = 0;
    for (int i = 0; i < n; i++) {
        total += value_nums[i] * (lcm / value_denoms[i]);
    }

    // 3. Check balance (exact or within tolerance)
    bool balanced = (total == 0) ||
                    (std::abs(total_decimal) <= tolerance);

    return List::create(...);
}
```

Key points:

- Uses `int64_t` to avoid overflow with large denominators
- LCM-based scaling provides exact arithmetic
- Optional tolerance for floating-point imports

## Adding New Rcpp Exports

To add a new Rcpp function:

### 1. Create or Edit Source File

``` cpp
// src/my-function.cpp
#include <Rcpp.h>
using namespace Rcpp;

//' My New Function
//'
//' Description of what it does.
//'
//' @param x Input parameter
//' @return Return value description
//' @export
// [[Rcpp::export]]
NumericVector my_function(NumericVector x) {
    // Implementation
}
```

### 2. Document with Roxygen

The `//'` comments become R documentation. Include:

- `@param` for each parameter
- `@return` describing the return value
- `@export` to make it available to users

### 3. Regenerate Exports

``` r
# In R, from package root
Rcpp::compileAttributes()
devtools::document()
```

This generates:

- `src/RcppExports.cpp` - C++ registration code
- `R/RcppExports.R` - R wrapper functions
- `man/my_function.Rd` - Documentation

### 4. For Parallel Functions

Include RcppParallel dependency:

``` cpp
// [[Rcpp::depends(RcppParallel)]]
#include <RcppParallel.h>
```

Create a Worker struct and use `parallelFor()`.

## Thread Safety Checklist

When writing parallel Rcpp code:

1.  **Use RMatrix/RVector** for output, not raw Rcpp types
2.  **Avoid R API calls** inside worker `operator()` - no `Rf_*`, no
    `Rcpp::*`
3.  **Use thread-local RNG** for random number generation
4.  **Deterministic seeding** based on grain begin for reproducibility
5.  **No shared mutable state** between threads
6.  **Const inputs** - mark input parameters as `const`

### Safe Patterns

``` cpp
// GOOD: Thread-local RNG
static thread_local std::mt19937 rng(seed);

// GOOD: RMatrix for parallel writes
RMatrix<double> results;
results(i, j) = value;

// GOOD: Local variables
double local_sum = 0;
```

### Unsafe Patterns

``` cpp
// BAD: Shared mutable state
static double shared_counter;  // Race condition!

// BAD: R API in parallel code
Rcpp::Rcout << "message";  // Not thread-safe

// BAD: Raw Rcpp types in parallel
NumericMatrix results;  // Use RMatrix instead
```

## Testing Rcpp Code

### Unit Tests

``` r
test_that("fraction_to_double handles edge cases", {
  # NA propagation
  expect_equal(fraction_to_double(NA_integer_, 100L), NA_real_)

  # Zero denominator
  expect_equal(fraction_to_double(100L, 0L), NA_real_)

  # Normal case
  expect_equal(fraction_to_double(150L, 100L), 1.5)
})
```

### Property-Based Tests

gnucashr uses hedgehog for property-based testing of Rcpp functions:

``` r
test_that("add_fractions is commutative", {
  forall(gen_fraction(), gen_fraction(), function(f1, f2) {
    result1 <- add_fractions(f1$num, f1$denom, f2$num, f2$denom)
    result2 <- add_fractions(f2$num, f2$denom, f1$num, f1$denom)
    expect_equal(result1, result2)
  })
})
```

### Benchmark Tests

Use bench package to verify performance characteristics:

``` r
test_that("parallel Monte Carlo is faster than sequential", {
  skip_on_cran()

  parallel_time <- bench::mark(
    monte_carlo_parallel(100000, 0.7, n_sims = 10000),
    iterations = 3
  )$median

  expect_lt(as.numeric(parallel_time), 0.2)  # Under 200ms
})
```

## Debugging Tips

### Compile with Debug Symbols

``` r
# In ~/.R/Makevars
CXXFLAGS = -g -O0 -Wall -Wextra
```

### Print Debugging

For single-threaded code only:

``` cpp
Rcpp::Rcout << "Debug: value = " << value << std::endl;
```

For parallel code, write to a thread-local buffer and collect after:

``` cpp
// Not recommended - use proper logging or unit tests instead
```

### Memory Debugging

Use AddressSanitizer for memory issues:

``` r
# In ~/.R/Makevars
CXXFLAGS = -fsanitize=address -fno-omit-frame-pointer
LDFLAGS = -fsanitize=address
```

## Resources

- [Rcpp
  Attributes](https://cran.r-project.org/web/packages/Rcpp/vignettes/Rcpp-attributes.pdf)
- [RcppParallel Guide](https://rcppcore.github.io/RcppParallel/)
- [Writing R
  Extensions](https://cran.r-project.org/doc/manuals/r-release/R-exts.html)
- [Rcpp Gallery](https://gallery.rcpp.org/)
