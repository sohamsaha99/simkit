
<!-- README.md is generated from README.Rmd. Please edit that file -->

# simkit

<!-- badges: start -->

[![R-CMD-check](https://github.com/sohamsaha99/simkit/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/sohamsaha99/simkit/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

simkit provides a tidy, pipeline-based framework for statistical
simulation studies. It handles parallelism, checkpointed storage, fault
tolerance, progress reporting, and performance evaluation so you can
focus on estimators and scenarios.

## Installation

You can install the development version of simkit from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("sohamsaha99/simkit")
```

## Example

A `SimDesign` is assembled by adding scenarios, methods, and
configuration together with `+`, then run and evaluated:

``` r
library(simkit)
library(future)
plan(sequential)

design <- sim_design(replicates = 20, seed = 1) +
  sim_scenario(
    name   = "n30",
    params = list(n = 30, mu = 0),
    dgp    = function(params) rnorm(params$n, params$mu),
    truth  = function(params) list(mean = params$mu)
  ) +
  sim_method(name = "sample_mean", fn = function(data, params) list(est = mean(data))) +
  sim_storage(path = tempfile("simkit_readme_"))

design |>
  sim_run(progress = FALSE) |>
  sim_collect() |>
  sim_evaluate(bias = function(sims, truth) mean(sims$est) - truth$mean)
#> # A tibble: 1 × 4
#>   scenario method      metric  value
#>   <chr>    <chr>       <chr>   <dbl>
#> 1 n30      sample_mean bias   0.0585
```

See `vignette("getting-started")` for the full pipeline, including
scenario grids, bootstrap variance estimation, and plotting.
