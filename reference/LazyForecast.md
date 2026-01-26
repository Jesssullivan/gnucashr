# LazyForecast R6 Class

LazyForecast R6 Class

LazyForecast R6 Class

## Details

Monadic lazy evaluation for financial forecasting. Builds an expression
tree (AST) without executing until collect() is called. Integrates with
Result and Logger monads for safe, auditable computation.

## Methods

### Public methods

- [`LazyForecast$new()`](#method-LazyForecast-new)

- [`LazyForecast$bind()`](#method-LazyForecast-bind)

- [`LazyForecast$map()`](#method-LazyForecast-map)

- [`LazyForecast$grow()`](#method-LazyForecast-grow)

- [`LazyForecast$filter()`](#method-LazyForecast-filter)

- [`LazyForecast$aggregate()`](#method-LazyForecast-aggregate)

- [`LazyForecast$scenario()`](#method-LazyForecast-scenario)

- [`LazyForecast$monte_carlo()`](#method-LazyForecast-monte_carlo)

- [`LazyForecast$sensitivity()`](#method-LazyForecast-sensitivity)

- [`LazyForecast$collect()`](#method-LazyForecast-collect)

- [`LazyForecast$force()`](#method-LazyForecast-force)

- [`LazyForecast$show_plan()`](#method-LazyForecast-show_plan)

- [`LazyForecast$metadata()`](#method-LazyForecast-metadata)

- [`LazyForecast$get_ast()`](#method-LazyForecast-get_ast)

- [`LazyForecast$get_source()`](#method-LazyForecast-get_source)

- [`LazyForecast$print()`](#method-LazyForecast-print)

- [`LazyForecast$clone()`](#method-LazyForecast-clone)

------------------------------------------------------------------------

### Method `new()`

Create a new LazyForecast

#### Usage

    LazyForecast$new(source = NULL)

#### Arguments

- `source`:

  Data source (config, tibble, GnuCashDB, BookCollection, or NULL)

------------------------------------------------------------------------

### Method `bind()`

Monadic bind - chain computation

#### Usage

    LazyForecast$bind(fn)

#### Arguments

- `fn`:

  Function that takes a value and returns a LazyForecast

#### Returns

New LazyForecast with combined AST

------------------------------------------------------------------------

### Method `map()`

Map over value (functor)

#### Usage

    LazyForecast$map(fn)

#### Arguments

- `fn`:

  Function to apply to materialized value

#### Returns

New LazyForecast with transform operation added

------------------------------------------------------------------------

### Method `grow()`

Add growth projection to AST

#### Usage

    LazyForecast$grow(rate, months, compound = TRUE)

#### Arguments

- `rate`:

  Monthly growth rate

- `months`:

  Number of months to project

- `compound`:

  Whether to compound (default TRUE)

#### Returns

Self (for chaining)

------------------------------------------------------------------------

### Method [`filter()`](https://rdrr.io/r/stats/filter.html)

Add filter operation to AST

#### Usage

    LazyForecast$filter(predicate)

#### Arguments

- `predicate`:

  Filter predicate expression or function

#### Returns

Self (for chaining)

------------------------------------------------------------------------

### Method [`aggregate()`](https://rdrr.io/r/stats/aggregate.html)

Add aggregation operation to AST

#### Usage

    LazyForecast$aggregate(..., .fns = list(sum = sum, mean = mean))

#### Arguments

- `...`:

  Column specifications

- `.fns`:

  Aggregation functions

#### Returns

Self (for chaining)

------------------------------------------------------------------------

### Method `scenario()`

Add named scenario to AST

#### Usage

    LazyForecast$scenario(name, params)

#### Arguments

- `name`:

  Scenario name

- `params`:

  Scenario parameters

#### Returns

Self (for chaining)

------------------------------------------------------------------------

### Method `monte_carlo()`

Add Monte Carlo simulation to AST

#### Usage

    LazyForecast$monte_carlo(n = 10000, params = list(), seed = 42L)

#### Arguments

- `n`:

  Number of simulations

- `params`:

  List with growth, expense parameters

- `seed`:

  Random seed for reproducibility

#### Returns

Self (for chaining)

------------------------------------------------------------------------

### Method `sensitivity()`

Add sensitivity analysis to AST

#### Usage

    LazyForecast$sensitivity(param_ranges)

#### Arguments

- `param_ranges`:

  Named list of parameter ranges

#### Returns

Self (for chaining)

------------------------------------------------------------------------

### Method `collect()`

Execute the AST and return materialized result

#### Usage

    LazyForecast$collect(parallel = TRUE, n_threads = NULL)

#### Arguments

- `parallel`:

  Use parallel execution (default TRUE)

- `n_threads`:

  Number of threads (NULL for auto)

#### Returns

Materialized result

------------------------------------------------------------------------

### Method [`force()`](https://rdrr.io/r/base/force.html)

Alias for collect()

#### Usage

    LazyForecast$force()

------------------------------------------------------------------------

### Method `show_plan()`

Print the AST without executing

#### Usage

    LazyForecast$show_plan()

#### Returns

Self (invisibly)

------------------------------------------------------------------------

### Method `metadata()`

Get metadata about the forecast

#### Usage

    LazyForecast$metadata()

#### Returns

List of metadata

------------------------------------------------------------------------

### Method `get_ast()`

Get the AST

#### Usage

    LazyForecast$get_ast()

#### Returns

List of AST nodes

------------------------------------------------------------------------

### Method `get_source()`

Get the source

#### Usage

    LazyForecast$get_source()

#### Returns

Source data

------------------------------------------------------------------------

### Method [`print()`](https://rdrr.io/r/base/print.html)

Print method

#### Usage

    LazyForecast$print()

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    LazyForecast$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
