// monte-carlo.cpp - Parallel Monte Carlo simulation for financial forecasting
//
// Uses RcppParallel for thread-safe embarrassingly parallel simulation.
// Each thread gets a deterministic seed: master_seed + thread_id for reproducibility.

// [[Rcpp::depends(RcppParallel)]]
#include <Rcpp.h>
#include <RcppParallel.h>
#include <random>
#include <cmath>
#include <algorithm>
#include <numeric>

using namespace Rcpp;
using namespace RcppParallel;

// Thread-safe random number generator per worker
// Uses deterministic seeding: master_seed + grain_begin for reproducibility
struct MonteCarloWorker : public Worker {
    // Input parameters
    const double base_revenue;
    const double base_expense_rate;
    const int n_periods;
    const double growth_mean;
    const double growth_sd;
    const double expense_mean;
    const double expense_sd;
    const uint64_t master_seed;

    // Output: matrix of [n_sims x n_periods] cumulative cash values
    RMatrix<double> results;
    RVector<double> final_cash;

    MonteCarloWorker(double base_revenue_, double base_expense_rate_,
                     int n_periods_, double growth_mean_, double growth_sd_,
                     double expense_mean_, double expense_sd_,
                     uint64_t master_seed_,
                     NumericMatrix results_, NumericVector final_cash_)
        : base_revenue(base_revenue_), base_expense_rate(base_expense_rate_),
          n_periods(n_periods_), growth_mean(growth_mean_), growth_sd(growth_sd_),
          expense_mean(expense_mean_), expense_sd(expense_sd_),
          master_seed(master_seed_),
          results(results_), final_cash(final_cash_) {}

    void operator()(std::size_t begin, std::size_t end) {
        // Thread-local RNG with deterministic seed
        std::mt19937_64 rng(master_seed + begin);
        std::normal_distribution<double> growth_dist(growth_mean, growth_sd);
        std::normal_distribution<double> expense_dist(expense_mean, expense_sd);

        for (std::size_t sim = begin; sim < end; ++sim) {
            double revenue = base_revenue;
            double cumulative_cash = 0.0;

            for (int period = 0; period < n_periods; ++period) {
                // Sample growth rate and expense rate
                double growth_rate = growth_dist(rng);
                double expense_rate = std::max(0.0, std::min(1.0, expense_dist(rng)));

                // Apply growth
                revenue *= (1.0 + growth_rate);

                // Calculate net cash for period
                double expenses = revenue * expense_rate;
                double net_cash = revenue - expenses;
                cumulative_cash += net_cash;

                // Store period result
                results(sim, period) = cumulative_cash;
            }

            // Store final cumulative cash
            final_cash[sim] = cumulative_cash;
        }
    }
};

//' Parallel Monte Carlo Simulation
//'
//' Run Monte Carlo simulation for financial projections using parallel execution.
//' Uses RcppParallel with thread-local RNG for reproducible, deterministic results.
//'
//' @param base_revenue Starting monthly revenue
//' @param base_expense_rate Base expense ratio (0-1)
//' @param n_sims Number of simulations to run (default 10000)
//' @param n_periods Number of periods to project (default 12)
//' @param growth_mean Mean monthly growth rate (default 0.05 = 5%)
//' @param growth_sd Standard deviation of growth (default 0.03)
//' @param expense_mean Mean expense ratio (default matches base_expense_rate)
//' @param expense_sd Standard deviation of expense ratio (default 0.05)
//' @param seed Master seed for reproducibility (default 42)
//' @return List with: results (matrix), final_cash (vector), summary (quantiles)
//' @export
// [[Rcpp::export]]
List monte_carlo_parallel(
    double base_revenue,
    double base_expense_rate,
    int n_sims = 10000,
    int n_periods = 12,
    double growth_mean = 0.05,
    double growth_sd = 0.03,
    double expense_mean = -1.0,
    double expense_sd = 0.05,
    int seed = 42
) {
    // Default expense_mean to base_expense_rate if not specified
    if (expense_mean < 0) {
        expense_mean = base_expense_rate;
    }

    // Allocate output matrices
    NumericMatrix results(n_sims, n_periods);
    NumericVector final_cash(n_sims);

    // Create worker and run in parallel
    MonteCarloWorker worker(
        base_revenue, base_expense_rate, n_periods,
        growth_mean, growth_sd, expense_mean, expense_sd,
        static_cast<uint64_t>(seed),
        results, final_cash
    );

    parallelFor(0, n_sims, worker);

    // Calculate summary statistics for final cash
    std::vector<double> sorted_cash(final_cash.begin(), final_cash.end());
    std::sort(sorted_cash.begin(), sorted_cash.end());

    int n = sorted_cash.size();
    double p5 = sorted_cash[static_cast<int>(n * 0.05)];
    double p10 = sorted_cash[static_cast<int>(n * 0.10)];
    double p25 = sorted_cash[static_cast<int>(n * 0.25)];
    double p50 = sorted_cash[static_cast<int>(n * 0.50)];
    double p75 = sorted_cash[static_cast<int>(n * 0.75)];
    double p90 = sorted_cash[static_cast<int>(n * 0.90)];
    double p95 = sorted_cash[static_cast<int>(n * 0.95)];

    double mean = std::accumulate(sorted_cash.begin(), sorted_cash.end(), 0.0) / n;
    double sq_sum = 0.0;
    for (double v : sorted_cash) {
        sq_sum += (v - mean) * (v - mean);
    }
    double sd = std::sqrt(sq_sum / n);

    List summary = List::create(
        Named("mean") = mean,
        Named("sd") = sd,
        Named("p5") = p5,
        Named("p10") = p10,
        Named("p25") = p25,
        Named("p50") = p50,
        Named("p75") = p75,
        Named("p90") = p90,
        Named("p95") = p95,
        Named("min") = sorted_cash.front(),
        Named("max") = sorted_cash.back()
    );

    return List::create(
        Named("results") = results,
        Named("final_cash") = final_cash,
        Named("summary") = summary,
        Named("params") = List::create(
            Named("base_revenue") = base_revenue,
            Named("base_expense_rate") = base_expense_rate,
            Named("n_sims") = n_sims,
            Named("n_periods") = n_periods,
            Named("growth_mean") = growth_mean,
            Named("growth_sd") = growth_sd,
            Named("expense_mean") = expense_mean,
            Named("expense_sd") = expense_sd,
            Named("seed") = seed
        )
    );
}

// Worker for multi-entity Monte Carlo simulation
struct MultiEntityWorker : public Worker {
    // Input: base values per entity
    const RVector<double> base_revenues;
    const RVector<double> growth_means;
    const RVector<double> growth_sds;
    const RVector<double> expense_rates;
    const int n_entities;
    const int n_periods;
    const uint64_t master_seed;

    // Output: 3D array flattened to 2D [n_sims x (n_entities * n_periods)]
    RMatrix<double> entity_results;
    RVector<double> total_cash;

    MultiEntityWorker(NumericVector base_revenues_, NumericVector growth_means_,
                      NumericVector growth_sds_, NumericVector expense_rates_,
                      int n_periods_, uint64_t master_seed_,
                      NumericMatrix entity_results_, NumericVector total_cash_)
        : base_revenues(base_revenues_), growth_means(growth_means_),
          growth_sds(growth_sds_), expense_rates(expense_rates_),
          n_entities(base_revenues_.size()), n_periods(n_periods_),
          master_seed(master_seed_),
          entity_results(entity_results_), total_cash(total_cash_) {}

    void operator()(std::size_t begin, std::size_t end) {
        std::mt19937_64 rng(master_seed + begin);

        for (std::size_t sim = begin; sim < end; ++sim) {
            double sim_total_cash = 0.0;

            for (int entity = 0; entity < n_entities; ++entity) {
                std::normal_distribution<double> growth_dist(
                    growth_means[entity], growth_sds[entity]
                );

                double revenue = base_revenues[entity];
                double cumulative = 0.0;

                for (int period = 0; period < n_periods; ++period) {
                    double growth = growth_dist(rng);
                    revenue *= (1.0 + growth);
                    double net = revenue * (1.0 - expense_rates[entity]);
                    cumulative += net;

                    // Store in flattened matrix: sim row, entity*n_periods + period col
                    int col = entity * n_periods + period;
                    entity_results(sim, col) = cumulative;
                }

                sim_total_cash += cumulative;
            }

            total_cash[sim] = sim_total_cash;
        }
    }
};

//' Multi-Entity Parallel Monte Carlo
//'
//' Run Monte Carlo simulation for multiple entities with correlated growth.
//'
//' @param base_revenues NumericVector of starting revenues per entity
//' @param growth_means NumericVector of mean growth rates per entity
//' @param growth_sds NumericVector of growth standard deviations per entity
//' @param expense_rates NumericVector of expense ratios per entity
//' @param n_sims Number of simulations (default 10000)
//' @param n_periods Number of periods (default 12)
//' @param seed Master seed for reproducibility
//' @return List with entity_results, total_cash, and summary
//' @export
// [[Rcpp::export]]
List monte_carlo_multi_entity(
    NumericVector base_revenues,
    NumericVector growth_means,
    NumericVector growth_sds,
    NumericVector expense_rates,
    int n_sims = 10000,
    int n_periods = 12,
    int seed = 42
) {
    int n_entities = base_revenues.size();

    // Validate inputs
    if (growth_means.size() != n_entities ||
        growth_sds.size() != n_entities ||
        expense_rates.size() != n_entities) {
        stop("All input vectors must have the same length");
    }

    // Allocate outputs
    NumericMatrix entity_results(n_sims, n_entities * n_periods);
    NumericVector total_cash(n_sims);

    MultiEntityWorker worker(
        base_revenues, growth_means, growth_sds, expense_rates,
        n_periods, static_cast<uint64_t>(seed),
        entity_results, total_cash
    );

    parallelFor(0, n_sims, worker);

    // Summary statistics
    std::vector<double> sorted(total_cash.begin(), total_cash.end());
    std::sort(sorted.begin(), sorted.end());
    int n = sorted.size();

    double mean = std::accumulate(sorted.begin(), sorted.end(), 0.0) / n;

    return List::create(
        Named("entity_results") = entity_results,
        Named("total_cash") = total_cash,
        Named("summary") = List::create(
            Named("mean") = mean,
            Named("p5") = sorted[static_cast<int>(n * 0.05)],
            Named("p25") = sorted[static_cast<int>(n * 0.25)],
            Named("p50") = sorted[static_cast<int>(n * 0.50)],
            Named("p75") = sorted[static_cast<int>(n * 0.75)],
            Named("p95") = sorted[static_cast<int>(n * 0.95)]
        ),
        Named("n_entities") = n_entities,
        Named("n_periods") = n_periods,
        Named("n_sims") = n_sims
    );
}
