# tests/testthat/test-run-sequential.R

# ---- known-answer test -----------------------------------------------------

test_that("sequential sim_run + sim_collect recovers correct bias and mse", {
  design <- base_design(replicates = 2000) +
    sim_storage(path = withr::local_tempdir(), resume = FALSE)
  res <- sim_run(design, progress = FALSE)
  df <- sim_collect(res)

  expect_equal(nrow(df), 2000)

  bias <- mean(df$est) - 0
  mse <- mean((df$est - 0)^2)

  expect_lt(abs(bias), 0.05)
  expect_lt(abs(mse - 1 / 30), 0.01)

  summ <- sim_evaluate(
    res,
    bias = function(sims, truth) mean(sims$est) - truth$mean,
    mse  = function(sims, truth) mean((sims$est - truth$mean)^2)
  ) |>
    sim_summarize()

  expect_lt(abs(summ$bias[summ$method == "m1"]), 0.05)
  expect_lt(abs(summ$mse[summ$method == "m1"] - 1 / 30), 0.01)
})

test_that("progress = TRUE does not error", {
  design <- base_design(replicates = 3) + sim_storage(path = withr::local_tempdir())
  expect_no_error(sim_run(design, progress = TRUE))
})

# ---- fault tolerance --------------------------------------------------------

test_that("on_error = 'warn' stores a sentinel, warns, and continues", {
  design <- sim_design(replicates = 5, seed = 1) +
    sim_scenario("s1", list(n = 10, mu = 0), simple_dgp, simple_truth) +
    sim_method("bad", function(data, params) {
      if (params$.rep_index == 3) stop("deliberate failure")
      list(est = mean(data))
    }) +
    sim_storage(path = withr::local_tempdir(), resume = FALSE)

  expect_warning(res <- sim_run(design, progress = FALSE, on_error = "warn"), "failed")
  df <- sim_collect(res)

  expect_equal(nrow(df), 5)
  expect_equal(sum(!is.na(df$error)), 1)
  expect_true(is.na(df$est[!is.na(df$error)]))
  expect_true(all(!is.na(df$est[is.na(df$error)])))
})

test_that("on_error = 'skip' stores a sentinel silently", {
  design <- sim_design(replicates = 3, seed = 1) +
    sim_scenario("s1", list(n = 10, mu = 0), simple_dgp, simple_truth) +
    sim_method("bad", function(data, params) {
      if (params$.rep_index == 2) stop("deliberate failure")
      list(est = mean(data))
    }) +
    sim_storage(path = withr::local_tempdir(), resume = FALSE)

  expect_no_warning(res <- sim_run(design, progress = FALSE, on_error = "skip"))
  df <- sim_collect(res)

  expect_equal(sum(!is.na(df$error)), 1)
})

test_that("on_error = 'stop' halts the run and re-throws the error", {
  design <- sim_design(replicates = 3, seed = 1) +
    sim_scenario("s1", list(n = 10, mu = 0), simple_dgp, simple_truth) +
    sim_method("bad", function(data, params) {
      if (params$.rep_index == 1) stop("deliberate failure")
      list(est = mean(data))
    }) +
    sim_storage(path = withr::local_tempdir(), resume = FALSE)

  expect_error(sim_run(design, progress = FALSE, on_error = "stop"), "deliberate failure")
})

# ---- resume ------------------------------------------------------------------

test_that("resume = TRUE does not rewrite completed replicates", {
  dir <- withr::local_tempdir()
  design <- base_design(replicates = 5) + sim_storage(path = dir, resume = TRUE)
  sim_run(design, progress = FALSE)

  rep_file <- fs::path(dir, "s1", "m1", "rep_0001.rds")
  saveRDS(list(est = 999999), rep_file)

  sim_run(design, progress = FALSE)
  df <- sim_collect(dir)

  expect_equal(df$est[df$rep == 1], 999999)
})

test_that("resume = TRUE fills in replicates missing from a partial run", {
  dir <- withr::local_tempdir()
  design <- base_design(replicates = 10) + sim_storage(path = dir, resume = TRUE)
  sim_run(design, progress = FALSE)

  rep_file <- fs::dir_ls(fs::path(dir, "s1", "m1"), regexp = "rep_0005\\.rds$")
  fs::file_delete(rep_file)

  sim_run(design, progress = FALSE)
  df <- sim_collect(dir)

  expect_equal(nrow(df), 10)
  expect_setequal(df$rep, 1:10)
})

test_that("resume = FALSE overwrites without checking for a design conflict", {
  dir <- withr::local_tempdir()
  design1 <- base_design(replicates = 5) + sim_storage(path = dir, resume = TRUE)
  sim_run(design1, progress = FALSE)

  design2 <- sim_design(replicates = 5, seed = 1) +
    sim_scenario("s1", list(n = 999, mu = 0), simple_dgp, simple_truth) +
    sim_method("m1", simple_method) +
    sim_storage(path = dir, resume = FALSE)

  expect_no_error(sim_run(design2, progress = FALSE))
})

test_that("resume = TRUE aborts when the storage path holds a conflicting design", {
  dir <- withr::local_tempdir()
  design1 <- base_design(replicates = 5) + sim_storage(path = dir, resume = TRUE)
  sim_run(design1, progress = FALSE)

  design2 <- sim_design(replicates = 5, seed = 1) +
    sim_scenario("s1", list(n = 999, mu = 0), simple_dgp, simple_truth) +
    sim_method("m1", simple_method) +
    sim_storage(path = dir, resume = TRUE)

  expect_error(sim_run(design2, progress = FALSE), class = "rlang_error")
})

# ---- manifest ----------------------------------------------------------------

test_that("sim_run writes a manifest.json with the expected schema", {
  dir <- withr::local_tempdir()
  design <- base_design(replicates = 5, seed = 7) + sim_storage(path = dir)
  sim_run(design, progress = FALSE)

  manifest <- jsonlite::read_json(fs::path(dir, "manifest.json"))

  expect_equal(manifest$seed, 7)
  expect_equal(manifest$replicates, 5)
  expect_length(manifest$scenarios, 1)
  expect_length(manifest$methods, 1)
  expect_equal(manifest$scenarios[[1]]$name, "s1")
  expect_equal(manifest$methods[[1]]$name, "m1")
  expect_false(fs::file_exists(fs::path(dir, "in_progress.json")))
})

# ---- params injection --------------------------------------------------------

test_that("sim_run injects .rep_index and .rng_seed into params", {
  design <- sim_design(replicates = 3, seed = 42) +
    sim_scenario("s1", list(n = 5, mu = 0), simple_dgp, simple_truth) +
    sim_method("m1", function(data, params) {
      list(est = mean(data), rep_seen = params$.rep_index, seed_seen = params$.rng_seed)
    }) +
    sim_storage(path = withr::local_tempdir())

  res <- sim_run(design, progress = FALSE)
  df <- sim_collect(res)

  expect_setequal(df$rep_seen, 1:3)
  expect_true(all(!is.na(df$seed_seen)))
})

# ---- sim_collect --------------------------------------------------------------

test_that("sim_collect accepts either a SimResults handle or a path", {
  dir <- withr::local_tempdir()
  design <- base_design(replicates = 5) + sim_storage(path = dir)
  res <- sim_run(design, progress = FALSE)

  df_from_handle <- sim_collect(res)
  df_from_path <- sim_collect(dir)

  expect_equal(nrow(df_from_handle), 5)
  # Collecting from a SimResults handle attaches scenario truth values (used
  # by sim_evaluate()) as an attribute; a bare path can't recover the
  # design's truth functions, so that attribute legitimately differs even
  # though the underlying data is identical.
  expect_equal(df_from_handle, df_from_path, ignore_attr = TRUE)
})

test_that("sim_collect subsets by scenario and method", {
  dir <- withr::local_tempdir()
  design <- sim_design(replicates = 3, seed = 1) +
    sim_scenario("s1", list(n = 5, mu = 0), simple_dgp, simple_truth) +
    sim_scenario("s2", list(n = 5, mu = 1), simple_dgp, simple_truth) +
    sim_method("m1", simple_method) +
    sim_storage(path = dir)
  sim_run(design, progress = FALSE)

  df <- sim_collect(dir, scenarios = "s1")
  expect_equal(unique(df$scenario), "s1")
})
