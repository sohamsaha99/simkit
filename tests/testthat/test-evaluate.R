# tests/testthat/test-evaluate.R

test_that("sim_evaluate maps metrics over every (scenario, method) pair", {
  design <- sim_design(replicates = 200, seed = 1) +
    sim_scenario("s1", list(n = 30, mu = 0), simple_dgp, simple_truth) +
    sim_scenario("s2", list(n = 30, mu = 5), simple_dgp, simple_truth) +
    sim_method("m1", simple_method) +
    sim_storage(path = withr::local_tempdir())
  res <- sim_run(design, progress = FALSE)

  eval <- sim_evaluate(res, bias = function(sims, truth) mean(sims$est) - truth$mean)

  expect_setequal(eval$scenario, c("s1", "s2"))
  expect_equal(unique(eval$metric), "bias")
  expect_lt(abs(eval$value[eval$scenario == "s1"]), 0.5)
  expect_lt(abs(eval$value[eval$scenario == "s2"]), 0.5)
})

test_that("sim_evaluate excludes failed replicates from metric input", {
  design <- sim_design(replicates = 5, seed = 1) +
    sim_scenario("s1", list(n = 10, mu = 0), simple_dgp, simple_truth) +
    sim_method("bad", function(data, params) {
      if (params$.rep_index == 3) stop("deliberate failure")
      list(est = mean(data))
    }) +
    sim_storage(path = withr::local_tempdir())
  expect_warning(res <- sim_run(design, progress = FALSE), "failed")

  eval <- sim_evaluate(res, count = function(sims, truth) nrow(sims))

  expect_equal(eval$value, 4)
})

test_that("a metric referencing a column the method never returns degrades to NA silently", {
  design <- base_design(replicates = 5) + sim_storage(path = withr::local_tempdir())
  res <- sim_run(design, progress = FALSE)

  expect_no_warning(
    eval <- sim_evaluate(res, coverage = function(sims, truth) {
      mean(sims$ci_lo <= truth$mean & truth$mean <= sims$ci_hi)
    })
  )
  expect_true(is.na(eval$value))
})

test_that("a metric function that errors produces NA and a single warning, without aborting the rest", {
  design <- base_design(replicates = 5) + sim_storage(path = withr::local_tempdir())
  res <- sim_run(design, progress = FALSE)

  expect_warning(
    eval <- sim_evaluate(
      res,
      bad  = function(sims, truth) stop("deliberate metric failure"),
      good = function(sims, truth) mean(sims$est)
    ),
    "bad"
  )

  expect_true(is.na(eval$value[eval$metric == "bad"]))
  expect_false(is.na(eval$value[eval$metric == "good"]))
})

test_that("sim_evaluate requires named metric functions", {
  design <- base_design(replicates = 3) + sim_storage(path = withr::local_tempdir())
  res <- sim_run(design, progress = FALSE)

  expect_error(
    sim_evaluate(res, function(sims, truth) mean(sims$est)),
    class = "rlang_error"
  )
})

test_that("sim_evaluate aborts when truth cannot be derived from a bare path", {
  dir <- withr::local_tempdir()
  design <- base_design(replicates = 3) + sim_storage(path = dir)
  sim_run(design, progress = FALSE)

  df <- sim_collect(dir)
  expect_error(
    sim_evaluate(df, bias = function(sims, truth) mean(sims$est)),
    class = "rlang_error"
  )
})

test_that("sim_evaluate works directly on sim_collect(<SimResults>) output", {
  design <- base_design(replicates = 20) + sim_storage(path = withr::local_tempdir())
  res <- sim_run(design, progress = FALSE)

  eval <- sim_collect(res) |>
    sim_evaluate(bias = function(sims, truth) mean(sims$est) - truth$mean)

  expect_equal(nrow(eval), 1)
  expect_false(is.na(eval$value))
})

test_that("sim_summarize pivots a long evaluation to wide format", {
  design <- base_design(replicates = 20) + sim_storage(path = withr::local_tempdir())
  res <- sim_run(design, progress = FALSE)

  eval <- sim_evaluate(
    res,
    bias = function(sims, truth) mean(sims$est) - truth$mean,
    mse  = function(sims, truth) mean((sims$est - truth$mean)^2)
  )
  summ <- sim_summarize(eval)

  expect_setequal(c("scenario", "method", "bias", "mse"), names(summ))
  expect_equal(nrow(summ), 1)
})
