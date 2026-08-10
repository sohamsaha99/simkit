# Shared test fixtures, auto-loaded by testthat.

simple_dgp <- function(params) rnorm(params$n, mean = params$mu, sd = 1)
simple_truth <- function(params) list(mean = params$mu)
simple_method <- function(data, params) list(est = mean(data))

base_design <- function(replicates = 50, seed = 1) {
  sim_design(replicates = replicates, seed = seed) +
    sim_scenario(
      name = "s1", params = list(n = 30, mu = 0),
      dgp = simple_dgp, truth = simple_truth
    ) +
    sim_method(name = "m1", fn = simple_method)
}
