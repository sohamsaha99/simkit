# tests/testthat/test-bootstrap.R

# ---- output shape ------------------------------------------------------------

test_that("output = 'se' adds only $se, not $ci_lo/$ci_hi", {
  set.seed(1)
  boot_fn <- sim_bootstrap(
    estimator = function(data, params) list(est = mean(data)),
    B = 200, statistic = "est", output = "se"
  )
  out <- boot_fn(rnorm(100), list())

  expect_true(all(c("est", "se") %in% names(out)))
  expect_false(any(c("ci_lo", "ci_hi", "ci") %in% names(out)))
})

test_that("output = 'ci' adds $ci_lo and $ci_hi, not a combined $ci field", {
  set.seed(2)
  boot_fn <- sim_bootstrap(
    estimator = function(data, params) list(est = mean(data)),
    B = 200, statistic = "est", output = "ci"
  )
  out <- boot_fn(rnorm(100), list())

  expect_true(all(c("ci_lo", "ci_hi") %in% names(out)))
  expect_false("ci" %in% names(out))
  expect_false("se" %in% names(out))
  expect_lt(out$ci_lo, out$est)
  expect_gt(out$ci_hi, out$est)
})

test_that("output = c('se', 'ci') adds all four fields", {
  boot_fn <- sim_bootstrap(
    estimator = function(data, params) list(est = mean(data)),
    B = 100, statistic = "est", output = c("se", "ci")
  )
  out <- boot_fn(rnorm(50), list())
  expect_true(all(c("est", "se", "ci_lo", "ci_hi") %in% names(out)))
})

# ---- statistical sanity -------------------------------------------------------

test_that("bootstrap SE for the mean of N(0,1), n=100 is in the right ballpark", {
  set.seed(42)
  boot_fn <- sim_bootstrap(
    estimator = function(data, params) list(est = mean(data)),
    B = 1000, statistic = "est", output = "se"
  )
  out <- boot_fn(rnorm(100), list())

  # True SE of the sample mean is 1/sqrt(100) = 0.1
  expect_true(out$se > 0.05 && out$se < 0.2)
})

test_that("parallel = TRUE and parallel = FALSE give consistent bootstrap SE", {
  old_plan <- future::plan()
  withr::defer(future::plan(old_plan))
  future::plan(future::multisession, workers = 2)

  set.seed(7)
  data <- rnorm(300)

  seq_fn <- sim_bootstrap(
    estimator = function(data, params) list(est = mean(data)),
    B = 500, statistic = "est", output = "se", parallel = FALSE
  )
  par_fn <- sim_bootstrap(
    estimator = function(data, params) list(est = mean(data)),
    B = 500, statistic = "est", output = "se", parallel = TRUE
  )

  se_seq <- seq_fn(data, list())$se
  se_par <- par_fn(data, list())$se

  expect_equal(se_seq, se_par, tolerance = 0.03)
})

# ---- drop-in usage with sim_method / sim_run ----------------------------------

test_that("sim_bootstrap works as a drop-in sim_method fn", {
  design <- sim_design(replicates = 5, seed = 1) +
    sim_scenario("s1", list(n = 50, mu = 0), simple_dgp, simple_truth) +
    sim_method(
      name = "boot_mean",
      fn = sim_bootstrap(
        estimator = function(data, params) list(est = mean(data)),
        B = 50, statistic = "est", output = c("se", "ci")
      )
    ) +
    sim_storage(path = withr::local_tempdir(), resume = FALSE)

  res <- sim_run(design, progress = FALSE)
  df <- sim_collect(res)

  expect_equal(nrow(df), 5)
  expect_true(all(c("est", "se", "ci_lo", "ci_hi") %in% names(df)))
  expect_true(all(!is.na(df$se)))
})

# ---- oversubscription warning --------------------------------------------------

test_that("nested parallelism warns when outer x inner exceeds detectCores()", {
  withr::local_options(simkit.outer_workers = 10000)
  old_plan <- future::plan()
  withr::defer(future::plan(old_plan))
  future::plan(future::multisession, workers = 2)

  boot_fn <- sim_bootstrap(
    estimator = function(data, params) list(est = mean(data)),
    B = 20, statistic = "est", output = "se", parallel = TRUE
  )
  expect_warning(boot_fn(rnorm(50), list()), "worker processes")
})

test_that("no warning when outer x inner stays within detectCores()", {
  withr::local_options(simkit.outer_workers = 1)
  old_plan <- future::plan()
  withr::defer(future::plan(old_plan))
  future::plan(future::sequential)

  boot_fn <- sim_bootstrap(
    estimator = function(data, params) list(est = mean(data)),
    B = 20, statistic = "est", output = "se", parallel = TRUE
  )
  expect_no_warning(boot_fn(rnorm(50), list()))
})

# ---- argument validation -------------------------------------------------------

test_that("sim_bootstrap validates its arguments", {
  expect_error(sim_bootstrap(estimator = "not a function"), "function")
  expect_error(
    sim_bootstrap(estimator = function(data, params) list(est = 1), B = 1),
    "at least 2"
  )
  expect_error(
    sim_bootstrap(estimator = function(data, params) list(est = 1), output = "combined"),
    "subset"
  )
})

test_that("sim_bootstrap aborts if estimator's output is missing statistic", {
  boot_fn <- sim_bootstrap(
    estimator = function(data, params) list(wrong_name = mean(data)),
    B = 10, statistic = "est"
  )
  expect_error(boot_fn(rnorm(20), list()), "est")
})
