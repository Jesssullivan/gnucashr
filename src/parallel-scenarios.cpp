// parallel-scenarios.cpp - Parallel scenario projection and sensitivity analysis
//
// Uses RcppParallel for batch scenario computation.
// Optimized for sensitivity grids and what-if analysis.

// [[Rcpp::depends(RcppParallel)]]
#include <Rcpp.h>
#include <RcppParallel.h>
#include <cmath>
#include <algorithm>

using namespace Rcpp;
using namespace RcppParallel;

// Worker for projecting multiple growth scenarios in parallel
struct ScenarioProjectionWorker : public Worker {
    // Input
    const RVector<double> base_values;     // Starting values per entity
    const RMatrix<double> growth_matrix;   // scenarios x entities
    const int n_periods;
    const int n_entities;

    // Output: 3D array flattened [n_scenarios x (n_periods * n_entities)]
    RMatrix<double> results;
    RVector<double> final_totals;

    ScenarioProjectionWorker(NumericVector base_values_, NumericMatrix growth_matrix_,
                              int n_periods_,
                              NumericMatrix results_, NumericVector final_totals_)
        : base_values(base_values_), growth_matrix(growth_matrix_),
          n_periods(n_periods_), n_entities(base_values_.size()),
          results(results_), final_totals(final_totals_) {}

    void operator()(std::size_t begin, std::size_t end) {
        for (std::size_t scenario = begin; scenario < end; ++scenario) {
            double scenario_total = 0.0;

            for (int entity = 0; entity < n_entities; ++entity) {
                double value = base_values[entity];
                double growth = growth_matrix(scenario, entity);
                double cumulative = 0.0;

                for (int period = 0; period < n_periods; ++period) {
                    value *= (1.0 + growth);
                    cumulative += value;

                    // Store in flattened format
                    int col = entity * n_periods + period;
                    results(scenario, col) = cumulative;
                }

                scenario_total += cumulative;
            }

            final_totals[scenario] = scenario_total;
        }
    }
};

//' Parallel Scenario Projection
//'
//' Project multiple growth scenarios in parallel for each entity.
//'
//' @param base_values Starting values per entity
//' @param growth_matrix Matrix of growth rates (scenarios x entities)
//' @param n_periods Number of periods to project
//' @return List with results matrix, final totals, and metadata
//' @export
// [[Rcpp::export]]
List parallel_project_scenarios(
    NumericVector base_values,
    NumericMatrix growth_matrix,
    int n_periods = 12
) {
    int n_scenarios = growth_matrix.nrow();
    int n_entities = base_values.size();

    if (growth_matrix.ncol() != n_entities) {
        stop("growth_matrix must have same number of columns as base_values length");
    }

    // Allocate output
    NumericMatrix results(n_scenarios, n_entities * n_periods);
    NumericVector final_totals(n_scenarios);

    ScenarioProjectionWorker worker(
        base_values, growth_matrix, n_periods,
        results, final_totals
    );

    parallelFor(0, n_scenarios, worker);

    return List::create(
        Named("results") = results,
        Named("final_totals") = final_totals,
        Named("n_scenarios") = n_scenarios,
        Named("n_entities") = n_entities,
        Named("n_periods") = n_periods
    );
}

// Worker for sensitivity grid analysis
struct SensitivityGridWorker : public Worker {
    // Input
    const double base_revenue;
    const double base_expense_rate;
    const RVector<double> growth_range;
    const RVector<double> expense_range;
    const int n_growth;
    const int n_expense;
    const int n_periods;

    // Output: matrix [n_growth x n_expense]
    RMatrix<double> outcomes;

    SensitivityGridWorker(double base_revenue_, double base_expense_rate_,
                          NumericVector growth_range_, NumericVector expense_range_,
                          int n_periods_, NumericMatrix outcomes_)
        : base_revenue(base_revenue_), base_expense_rate(base_expense_rate_),
          growth_range(growth_range_), expense_range(expense_range_),
          n_growth(growth_range_.size()), n_expense(expense_range_.size()),
          n_periods(n_periods_), outcomes(outcomes_) {}

    void operator()(std::size_t begin, std::size_t end) {
        for (std::size_t idx = begin; idx < end; ++idx) {
            // Convert linear index to 2D grid coordinates
            int g_idx = idx / n_expense;
            int e_idx = idx % n_expense;

            double growth = growth_range[g_idx];
            double expense_rate = expense_range[e_idx];

            // Project forward
            double revenue = base_revenue;
            double cumulative_cash = 0.0;

            for (int period = 0; period < n_periods; ++period) {
                revenue *= (1.0 + growth);
                double net = revenue * (1.0 - expense_rate);
                cumulative_cash += net;
            }

            outcomes(g_idx, e_idx) = cumulative_cash;
        }
    }
};

//' Parallel Sensitivity Grid
//'
//' Compute sensitivity grid for growth rate and expense ratio combinations.
//' Efficient for large parameter sweeps.
//'
//' @param base_revenue Starting revenue
//' @param base_expense_rate Base expense ratio
//' @param growth_range Vector of growth rates to test
//' @param expense_range Vector of expense ratios to test
//' @param n_periods Number of periods to project
//' @return List with outcomes matrix and ranges
//' @export
// [[Rcpp::export]]
List parallel_sensitivity_grid(
    double base_revenue,
    double base_expense_rate,
    NumericVector growth_range,
    NumericVector expense_range,
    int n_periods = 12
) {
    int n_growth = growth_range.size();
    int n_expense = expense_range.size();
    int total_combos = n_growth * n_expense;

    // Allocate output matrix
    NumericMatrix outcomes(n_growth, n_expense);

    SensitivityGridWorker worker(
        base_revenue, base_expense_rate,
        growth_range, expense_range,
        n_periods, outcomes
    );

    parallelFor(0, total_combos, worker);

    // Set dimension names
    CharacterVector growth_names(n_growth);
    for (int i = 0; i < n_growth; ++i) {
        growth_names[i] = std::to_string(growth_range[i]);
    }
    CharacterVector expense_names(n_expense);
    for (int i = 0; i < n_expense; ++i) {
        expense_names[i] = std::to_string(expense_range[i]);
    }
    rownames(outcomes) = growth_names;
    colnames(outcomes) = expense_names;

    return List::create(
        Named("outcomes") = outcomes,
        Named("growth_range") = growth_range,
        Named("expense_range") = expense_range,
        Named("base_revenue") = base_revenue,
        Named("base_expense_rate") = base_expense_rate,
        Named("n_periods") = n_periods
    );
}

// Worker for batch growth projections with varying initial values
struct BatchProjectionWorker : public Worker {
    // Input
    const RVector<double> initial_values;
    const double growth_rate;
    const double expense_rate;
    const int n_periods;

    // Output
    RMatrix<double> projections;  // [n_items x n_periods]
    RVector<double> final_values;

    BatchProjectionWorker(NumericVector initial_values_, double growth_rate_,
                          double expense_rate_, int n_periods_,
                          NumericMatrix projections_, NumericVector final_values_)
        : initial_values(initial_values_), growth_rate(growth_rate_),
          expense_rate(expense_rate_), n_periods(n_periods_),
          projections(projections_), final_values(final_values_) {}

    void operator()(std::size_t begin, std::size_t end) {
        for (std::size_t item = begin; item < end; ++item) {
            double value = initial_values[item];
            double cumulative = 0.0;

            for (int period = 0; period < n_periods; ++period) {
                value *= (1.0 + growth_rate);
                double net = value * (1.0 - expense_rate);
                cumulative += net;
                projections(item, period) = cumulative;
            }

            final_values[item] = cumulative;
        }
    }
};

//' Batch Growth Projection
//'
//' Project growth for many initial values with same parameters.
//' Useful for account-level projections.
//'
//' @param initial_values Vector of starting values
//' @param growth_rate Monthly growth rate
//' @param expense_rate Expense ratio (0-1)
//' @param n_periods Number of periods
//' @return List with projections matrix and final values
//' @export
// [[Rcpp::export]]
List batch_project_growth(
    NumericVector initial_values,
    double growth_rate,
    double expense_rate,
    int n_periods = 12
) {
    int n_items = initial_values.size();

    NumericMatrix projections(n_items, n_periods);
    NumericVector final_values(n_items);

    BatchProjectionWorker worker(
        initial_values, growth_rate, expense_rate, n_periods,
        projections, final_values
    );

    parallelFor(0, n_items, worker);

    return List::create(
        Named("projections") = projections,
        Named("final_values") = final_values,
        Named("n_items") = n_items,
        Named("n_periods") = n_periods,
        Named("growth_rate") = growth_rate,
        Named("expense_rate") = expense_rate
    );
}

//' Sequential Projection (Non-Parallel Fallback)
//'
//' Single-threaded projection for testing and comparison.
//'
//' @param base_revenue Starting revenue
//' @param growth_rate Monthly growth rate
//' @param expense_rate Expense ratio
//' @param n_periods Number of periods
//' @return NumericVector of cumulative cash by period
//' @export
// [[Rcpp::export]]
NumericVector sequential_projection(
    double base_revenue,
    double growth_rate,
    double expense_rate,
    int n_periods = 12
) {
    NumericVector result(n_periods);
    double revenue = base_revenue;
    double cumulative = 0.0;

    for (int period = 0; period < n_periods; ++period) {
        revenue *= (1.0 + growth_rate);
        double net = revenue * (1.0 - expense_rate);
        cumulative += net;
        result[period] = cumulative;
    }

    return result;
}
