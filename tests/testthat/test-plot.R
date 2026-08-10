# tests/testthat/test-plot.R

test_that("sim_plot returns a ggplot object, coloring by an existing column", {
  skip_if_not_installed("ggplot2")

  design <- sim_design(replicates = 20, seed = 1) +
    sim_scenario_grid(
      params  = list(n = c(30, 100)),
      name_fn = function(p) paste0("n", p$n),
      dgp     = function(p) rnorm(p$n),
      truth   = function(p) list(mean = 0)
    ) +
    sim_method("m1", simple_method) +
    sim_storage(path = withr::local_tempdir())
  res <- sim_run(design, progress = FALSE)
  evaluation <- sim_evaluate(res, bias = function(sims, truth) mean(sims$est) - truth$mean)

  p <- sim_plot(evaluation, metric = "bias", x = "n", color = "method")

  expect_s3_class(p, "ggplot")
  expect_equal(nrow(p$data), 2)
  expect_equal(sort(p$data$x), c(30, 100))
})

test_that("sim_plot extracts params from manifest.json when path is supplied", {
  skip_if_not_installed("ggplot2")

  root <- withr::local_tempdir()
  design <- sim_design(replicates = 10, seed = 1) +
    sim_scenario_grid(
      params  = list(n = c(30, 60), dist = c("normal", "t3")),
      name_fn = function(p) paste0(p$dist, "_n", p$n),
      dgp     = function(p) if (p$dist == "normal") rnorm(p$n) else rt(p$n, df = 3),
      truth   = function(p) list(mean = 0)
    ) +
    sim_method("m1", simple_method) +
    sim_storage(path = root)
  res <- sim_run(design, progress = FALSE)
  evaluation <- sim_evaluate(res, bias = function(sims, truth) mean(sims$est) - truth$mean)

  p <- sim_plot(evaluation, metric = "bias", x = "n", color = "method", facet = "dist", path = root)

  expect_s3_class(p, "ggplot")
  expect_equal(nrow(p$data), 4)
  expect_setequal(p$data$facet, c("normal", "t3"))
})

test_that("sim_plot fails informatively when metric is not in the evaluation", {
  skip_if_not_installed("ggplot2")

  design <- base_design(replicates = 10) + sim_storage(path = withr::local_tempdir())
  res <- sim_run(design, progress = FALSE)
  evaluation <- sim_evaluate(res, bias = function(sims, truth) mean(sims$est) - truth$mean)

  expect_error(
    sim_plot(evaluation, metric = "mse", x = "n"),
    class = "rlang_error"
  )
})

test_that("sim_plot fails informatively when a scenario parameter cannot be extracted", {
  skip_if_not_installed("ggplot2")

  design <- base_design(replicates = 10) + sim_storage(path = withr::local_tempdir())
  res <- sim_run(design, progress = FALSE)
  evaluation <- sim_evaluate(res, bias = function(sims, truth) mean(sims$est) - truth$mean)

  # base_design()'s scenario is named "s1" -- it does not encode `n`, and no
  # `path` is supplied to fall back to the manifest.
  expect_error(
    sim_plot(evaluation, metric = "bias", x = "n"),
    class = "rlang_error"
  )
})

test_that("sim_plot validates its evaluation argument", {
  skip_if_not_installed("ggplot2")

  expect_error(
    sim_plot(data.frame(x = 1), metric = "bias", x = "n"),
    class = "rlang_error"
  )
})

test_that("sim_plot uses scenario/method columns directly without extraction", {
  skip_if_not_installed("ggplot2")

  design <- sim_design(replicates = 10, seed = 1) +
    sim_scenario("s1", list(n = 10, mu = 0), simple_dgp, simple_truth) +
    sim_scenario("s2", list(n = 20, mu = 0), simple_dgp, simple_truth) +
    sim_method("m1", simple_method) +
    sim_storage(path = withr::local_tempdir())
  res <- sim_run(design, progress = FALSE)
  evaluation <- sim_evaluate(res, bias = function(sims, truth) mean(sims$est) - truth$mean)

  p <- sim_plot(evaluation, metric = "bias", x = "scenario", color = "method")

  expect_s3_class(p, "ggplot")
  expect_setequal(p$data$x, c("s1", "s2"))
})
