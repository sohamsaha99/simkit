# tests/testthat/test-run-parallel.R

# ---- parallel matches sequential ---------------------------------------------

test_that("parallel results match sequential given the same design seed", {
  dir <- withr::local_tempdir()

  design_seq <- base_design(replicates = 20) +
    sim_storage(path = fs::path(dir, "seq"), resume = FALSE)
  res_seq <- sim_run(design_seq, progress = FALSE)

  design_par <- base_design(replicates = 20) +
    sim_parallel(workers = 2) +
    sim_storage(path = fs::path(dir, "par"), resume = FALSE)
  res_par <- sim_run(design_par, progress = FALSE)

  seq_vals <- sim_collect(res_seq) |> dplyr::arrange(rep) |> dplyr::pull(est)
  par_vals <- sim_collect(res_par) |> dplyr::arrange(rep) |> dplyr::pull(est)

  expect_equal(seq_vals, par_vals)
})

test_that("parallel results match sequential across multiple scenarios and methods", {
  dir <- withr::local_tempdir()

  build <- function() {
    sim_design(replicates = 15, seed = 7) +
      sim_scenario("s1", list(n = 8, mu = 0), simple_dgp, simple_truth) +
      sim_scenario("s2", list(n = 8, mu = 1), simple_dgp, simple_truth) +
      sim_method("m1", simple_method) +
      sim_method("m2", function(data, params) list(est = median(data)))
  }

  res_seq <- sim_run(build() + sim_storage(path = fs::path(dir, "seq")), progress = FALSE)
  res_par <- sim_run(
    build() + sim_parallel(workers = 2) + sim_storage(path = fs::path(dir, "par")),
    progress = FALSE
  )

  df_seq <- sim_collect(res_seq) |> dplyr::arrange(scenario, method, rep)
  df_par <- sim_collect(res_par) |> dplyr::arrange(scenario, method, rep)

  expect_equal(df_seq$est, df_par$est)
})

test_that("sim_parallel(workers = 1) behaves like sequential execution", {
  dir <- withr::local_tempdir()

  res <- sim_run(
    base_design(replicates = 10) + sim_parallel(workers = 1) + sim_storage(path = dir),
    progress = FALSE
  )
  df <- sim_collect(res)

  expect_equal(nrow(df), 10)
  expect_true(all(!is.na(df$est)))
})

test_that("resume = TRUE under a parallel backend skips completed replicates", {
  dir <- withr::local_tempdir()
  design <- base_design(replicates = 10) +
    sim_parallel(workers = 2) +
    sim_storage(path = dir, resume = TRUE)

  sim_run(design, progress = FALSE)

  rep_file <- fs::path(dir, "s1", "m1", "rep_0005.rds")
  fs::file_delete(rep_file)

  sim_run(design, progress = FALSE)
  df <- sim_collect(dir)

  expect_equal(nrow(df), 10)
  expect_setequal(df$rep, 1:10)
})

test_that("on_error = 'warn' works the same under a parallel backend", {
  design <- sim_design(replicates = 8, seed = 3) +
    sim_scenario("s1", list(n = 10, mu = 0), simple_dgp, simple_truth) +
    sim_method("bad", function(data, params) {
      if (params$.rep_index == 4) stop("deliberate failure")
      list(est = mean(data))
    }) +
    sim_parallel(workers = 2) +
    sim_storage(path = withr::local_tempdir(), resume = FALSE)

  expect_warning(res <- sim_run(design, progress = FALSE, on_error = "warn"), "failed")
  df <- sim_collect(res)

  expect_equal(nrow(df), 8)
  expect_equal(sum(!is.na(df$error)), 1)
})

# ---- closure size warning -----------------------------------------------------

test_that("a method with a large captured environment triggers a warning", {
  big <- matrix(0, nrow = 3000, ncol = 3000) # ~72 MB
  big_method <- local({
    z <- big
    function(data, params) list(est = mean(data) + z[1, 1])
  })

  design <- base_design(replicates = 4) +
    sim_method("big", big_method) +
    sim_parallel(workers = 2) +
    sim_storage(path = withr::local_tempdir(), resume = FALSE)

  expect_warning(sim_run(design, progress = FALSE), "large environment")
})

test_that("a method with a small captured environment does not warn", {
  design <- base_design(replicates = 4) +
    sim_parallel(workers = 2) +
    sim_storage(path = withr::local_tempdir(), resume = FALSE)

  expect_no_warning(sim_run(design, progress = FALSE))
})

test_that("the closure warning does not fire for a purely sequential run", {
  big <- matrix(0, nrow = 3000, ncol = 3000)
  big_method <- local({
    z <- big
    function(data, params) list(est = mean(data) + z[1, 1])
  })

  design <- base_design(replicates = 3) +
    sim_method("big", big_method) +
    sim_storage(path = withr::local_tempdir(), resume = FALSE)

  expect_no_warning(sim_run(design, progress = FALSE))
})
