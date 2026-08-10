# tests/testthat/test-classes.R

# ---- SimScenario ------------------------------------------------------

test_that("sim_scenario constructs a valid SimScenario", {
  s <- sim_scenario(
    name = "s1", params = list(n = 30, mu = 0),
    dgp = simple_dgp, truth = simple_truth
  )
  expect_s3_class(s, "SimScenario")
  expect_equal(s$name, "s1")
  expect_equal(s$params, list(n = 30, mu = 0))
})

test_that("sim_scenario validates its arguments", {
  expect_error(sim_scenario(1, list(), simple_dgp, simple_truth), class = "rlang_error")
  expect_error(sim_scenario("s1", list(n = 1), "not a function", simple_truth), class = "rlang_error")
  expect_error(sim_scenario("s1", list(n = 1), simple_dgp, "not a function"), class = "rlang_error")
})

# ---- sim_scenario_grid --------------------------------------------------

test_that("sim_scenario_grid produces one scenario per combination", {
  grid <- sim_scenario_grid(
    params  = list(n = c(100, 500), dist = c("normal", "t3")),
    name_fn = function(p) paste0(p$dist, "_n", p$n),
    dgp     = function(p) if (p$dist == "normal") rnorm(p$n) else rt(p$n, df = 3),
    truth   = function(p) list(mean = 0)
  )

  expect_s3_class(grid, "SimScenarioGrid")
  expect_length(grid, 4)
  expect_true(all(vapply(grid, inherits, logical(1), "SimScenario")))
  expect_setequal(
    vapply(grid, `[[`, character(1), "name"),
    c("normal_n100", "normal_n500", "t3_n100", "t3_n500")
  )
})

test_that("sim_scenario_grid holds fixed params constant across the grid", {
  grid <- sim_scenario_grid(
    params  = list(n = c(100, 500), mu = 2, sigma = c(0.5, 1.0)),
    name_fn = function(p) paste0("n", p$n, "_s", p$sigma),
    dgp     = function(p) rnorm(p$n, p$mu, p$sigma),
    truth   = function(p) list(mean = p$mu)
  )

  expect_length(grid, 4)
  scenario <- grid[[which(vapply(grid, `[[`, character(1), "name") == "n100_s0.5")]]
  expect_equal(scenario$params, list(n = 100, mu = 2, sigma = 0.5))
})

test_that("sim_scenario_grid applies name_fn and detects duplicate names", {
  expect_error(
    sim_scenario_grid(
      params  = list(n = c(100, 500)),
      name_fn = function(p) "always_the_same",
      dgp     = simple_dgp,
      truth   = simple_truth
    ),
    class = "rlang_error"
  )
})

test_that("sim_scenario_grid validates its arguments", {
  expect_error(
    sim_scenario_grid(params = list(), name_fn = identity, dgp = simple_dgp, truth = simple_truth),
    class = "rlang_error"
  )
  expect_error(
    sim_scenario_grid(params = list(n = 1), name_fn = "not a function", dgp = simple_dgp, truth = simple_truth),
    class = "rlang_error"
  )
  expect_error(
    sim_scenario_grid(params = list(n = 1), name_fn = identity, dgp = "not a function", truth = simple_truth),
    class = "rlang_error"
  )
  expect_error(
    sim_scenario_grid(params = list(n = 1), name_fn = identity, dgp = simple_dgp, truth = "not a function"),
    class = "rlang_error"
  )
})

test_that("a SimScenarioGrid can be added directly to a SimDesign with +", {
  d <- sim_design(replicates = 20, seed = 1) +
    sim_scenario_grid(
      params  = list(n = c(100, 500), sigma = c(0.5, 1.0)),
      name_fn = function(p) paste0("n", p$n, "_s", p$sigma),
      dgp     = function(p) rnorm(p$n, sd = p$sigma),
      truth   = function(p) list(mean = 0)
    ) +
    sim_method("m1", simple_method)

  expect_length(d$scenarios, 4)
  expect_true(all(vapply(d$scenarios, inherits, logical(1), "SimScenario")))
})

test_that("adding a SimScenarioGrid with a colliding name is a hard error", {
  expect_error(
    sim_design(replicates = 20, seed = 1) +
      sim_scenario("s1", list(n = 30, mu = 0), simple_dgp, simple_truth) +
      sim_scenario_grid(
        params  = list(n = c(30, 60)),
        name_fn = function(p) "s1",
        dgp     = simple_dgp,
        truth   = simple_truth
      ),
    class = "rlang_error"
  )
})

# ---- SimMethod ---------------------------------------------------------

test_that("sim_method constructs a valid SimMethod", {
  m <- sim_method(name = "m1", fn = simple_method)
  expect_s3_class(m, "SimMethod")
  expect_equal(m$name, "m1")
  expect_equal(m$store, "summary")
})

test_that("sim_method validates its arguments", {
  expect_error(sim_method("m1", "not a function"), class = "rlang_error")
  expect_error(sim_method("m1", simple_method, store = "bogus"), class = "rlang_error")
})

# ---- SimParallel --------------------------------------------------------

test_that("sim_parallel constructs a valid SimParallel with defaults", {
  p <- sim_parallel()
  expect_s3_class(p, "SimParallel")
  expect_equal(p$workers, 1)
  expect_equal(p$backend, "future")
  expect_equal(p$chunk_size, 1)
})

test_that("sim_parallel validates its arguments", {
  expect_error(sim_parallel(workers = 0), class = "rlang_error")
  expect_error(sim_parallel(backend = "bogus"), class = "rlang_error")
  expect_error(sim_parallel(chunk_size = 0), class = "rlang_error")
})

# ---- SimStorage ---------------------------------------------------------

test_that("sim_storage constructs a valid SimStorage with defaults", {
  st <- sim_storage(path = "./sim_results")
  expect_s3_class(st, "SimStorage")
  expect_equal(st$format, "rds")
  expect_true(st$resume)
})

test_that("sim_storage validates its arguments", {
  expect_error(sim_storage(path = 1), class = "rlang_error")
  expect_error(sim_storage(path = "x", format = "bogus"), class = "rlang_error")
  expect_error(sim_storage(path = "x", resume = "yes"), class = "rlang_error")
})

# ---- SimDesign ------------------------------------------------------------

test_that("sim_design constructs an empty SimDesign", {
  d <- sim_design(replicates = 100, seed = 1)
  expect_s3_class(d, "SimDesign")
  expect_equal(d$replicates, 100)
  expect_equal(d$seed, 1)
  expect_length(d$scenarios, 0)
  expect_length(d$methods, 0)
})

test_that("sim_design validates its arguments", {
  expect_error(sim_design(replicates = 0, seed = 1), class = "rlang_error")
  expect_error(sim_design(replicates = 100, seed = "x"), class = "rlang_error")
})

# ---- `+.SimDesign` --------------------------------------------------------

test_that("+ assembles scenarios, methods, parallel, and storage", {
  d <- sim_design(replicates = 100, seed = 1) +
    sim_scenario("s1", list(n = 30, mu = 0), simple_dgp, simple_truth) +
    sim_method("m1", simple_method) +
    sim_parallel(workers = 2) +
    sim_storage(path = "./results")

  expect_length(d$scenarios, 1)
  expect_length(d$methods, 1)
  expect_s3_class(d$parallel, "SimParallel")
  expect_s3_class(d$storage, "SimStorage")
})

test_that("+ never mutates the original design", {
  base <- sim_design(replicates = 100, seed = 1) +
    sim_scenario("s1", list(n = 30, mu = 0), simple_dgp, simple_truth)

  design_a <- base + sim_parallel(workers = 4)
  design_b <- base + sim_parallel(workers = 8)

  expect_null(base$parallel)
  expect_equal(design_a$parallel$workers, 4)
  expect_equal(design_b$parallel$workers, 8)
})

test_that("+ rejects unsupported classes", {
  d <- sim_design(replicates = 100, seed = 1)
  expect_error(d + "not a component", class = "rlang_error")
})

test_that("+ raises a hard error on duplicate scenario names", {
  expect_error(
    sim_design(replicates = 100, seed = 1) +
      sim_scenario("s1", list(n = 30, mu = 0), simple_dgp, simple_truth) +
      sim_scenario("s1", list(n = 60, mu = 1), simple_dgp, simple_truth),
    class = "rlang_error"
  )
})

test_that("+ raises a hard error on duplicate method names", {
  expect_error(
    sim_design(replicates = 100, seed = 1) +
      sim_method("m1", simple_method) +
      sim_method("m1", simple_method),
    class = "rlang_error"
  )
})

test_that("base_design() builds cleanly and matches the design.md example shape", {
  d <- base_design(replicates = 20)
  expect_s3_class(d, "SimDesign")
  expect_length(d$scenarios, 1)
  expect_length(d$methods, 1)
})

# ---- print methods ---------------------------------------------------------

test_that("print methods render without error", {
  expect_message(print(base_design()), "SimDesign")
  expect_message(print(sim_scenario("s1", list(n = 30, mu = 0), simple_dgp, simple_truth)), "SimScenario")
  expect_message(print(sim_method("m1", simple_method)), "SimMethod")
  expect_message(print(sim_parallel()), "SimParallel")
  expect_message(print(sim_storage(path = "./x")), "SimStorage")
})

test_that("print.SimDesign renders correctly", {
  expect_snapshot(print(base_design()))
})
