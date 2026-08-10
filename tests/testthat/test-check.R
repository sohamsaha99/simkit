# tests/testthat/test-check.R

# ---- passing check -----------------------------------------------------------

test_that("sim_check reports a passing design", {
  design <- base_design(replicates = 1000)
  check <- suppressMessages(sim_check(design, replicates = 2))

  expect_s3_class(check, "SimCheck")
  expect_true(simkit_check_passed(check))
  expect_length(check$results, 1)
  expect_equal(check$results[[1]]$ok, 2)
  expect_equal(check$results[[1]]$replicates, 2)
  expect_true(check$results[[1]]$ms_per_rep >= 0)
  expect_true(is.na(check$results[[1]]$error_message))
})

test_that("sim_check checks every (scenario, method) pair", {
  design <- sim_design(replicates = 10, seed = 1) +
    sim_scenario("s1", list(n = 5, mu = 0), simple_dgp, simple_truth) +
    sim_scenario("s2", list(n = 5, mu = 1), simple_dgp, simple_truth) +
    sim_method("m1", simple_method) +
    sim_method("m2", simple_method)

  check <- suppressMessages(sim_check(design, replicates = 2))
  expect_length(check$results, 4)
  expect_true(simkit_check_passed(check))
})

test_that("print.SimCheck reports the pre-flight check header", {
  design <- base_design(replicates = 1000)
  expect_message(check <- sim_check(design, replicates = 2), "simkit pre-flight check")
})

# ---- failing check ------------------------------------------------------------

test_that("sim_check reports a failing pair with error message and traceback", {
  design <- sim_design(replicates = 100, seed = 1) +
    sim_scenario("s1", list(n = 10, mu = 0), simple_dgp, simple_truth) +
    sim_method("bad", function(data, params) mean(data, trim = params$trim))

  check <- suppressMessages(sim_check(design, replicates = 2))

  expect_false(simkit_check_passed(check))
  pair <- check$results[[1]]
  expect_equal(pair$ok, 0)
  expect_match(pair$error_message, "trim")
  expect_true(is.character(pair$traceback))
  expect_false(is.na(pair$traceback))
})

test_that("sim_check counts partial successes when failures are intermittent", {
  design <- sim_design(replicates = 100, seed = 1) +
    sim_scenario("s1", list(n = 10, mu = 0), simple_dgp, simple_truth) +
    sim_method("flaky", function(data, params) {
      if (params$.rep_index == 2) stop("deliberate failure")
      list(est = mean(data))
    })

  check <- suppressMessages(sim_check(design, replicates = 3))
  pair <- check$results[[1]]
  expect_equal(pair$ok, 2)
  expect_match(pair$error_message, "deliberate failure")
  expect_false(simkit_check_passed(check))
})

# ---- scratch directory isolation ---------------------------------------------

test_that("sim_check never writes to the design's configured storage path", {
  dir <- withr::local_tempdir()
  design <- base_design(replicates = 100) + sim_storage(path = dir)

  suppressMessages(sim_check(design, replicates = 2))

  expect_false(fs::dir_exists(fs::path(dir, "s1")))
  expect_false(fs::file_exists(fs::path(dir, "manifest.json")))
})

# ---- ETA -----------------------------------------------------------------------

test_that("simkit_format_eta divides the sequential estimate by worker count", {
  design_seq <- sim_design(replicates = 720000, seed = 1)
  design_par <- design_seq + sim_parallel(workers = 4)

  eta_seq <- simkit_format_eta(1, design_seq)
  eta_par <- simkit_format_eta(1, design_par)

  expect_match(eta_seq, "min")
  expect_no_match(eta_seq, "parallel")
  expect_match(eta_par, "parallel, 4 workers")
})

# ---- integration with sim_run(check = TRUE) -----------------------------------

test_that("sim_run(check = TRUE) aborts the full run when a pair fails", {
  design <- sim_design(replicates = 5, seed = 1) +
    sim_scenario("s1", list(n = 10, mu = 0), simple_dgp, simple_truth) +
    sim_method("bad", function(data, params) stop("nope")) +
    sim_storage(path = withr::local_tempdir(), resume = FALSE)

  suppressMessages(
    expect_error(sim_run(design, progress = FALSE, check = TRUE), class = "rlang_error")
  )
})

test_that("sim_run(check = TRUE) proceeds to the full run when the check passes", {
  dir <- withr::local_tempdir()
  design <- base_design(replicates = 5) + sim_storage(path = dir, resume = FALSE)

  res <- suppressMessages(sim_run(design, progress = FALSE, check = TRUE))
  df <- sim_collect(res)

  expect_equal(nrow(df), 5)
})

test_that("sim_run(check = TRUE) failure leaves the configured storage path untouched", {
  dir <- withr::local_tempdir()
  design <- sim_design(replicates = 5, seed = 1) +
    sim_scenario("s1", list(n = 10, mu = 0), simple_dgp, simple_truth) +
    sim_method("bad", function(data, params) stop("nope")) +
    sim_storage(path = dir, resume = FALSE)

  suppressMessages(expect_error(sim_run(design, progress = FALSE, check = TRUE)))
  expect_false(fs::file_exists(fs::path(dir, "manifest.json")))
})
