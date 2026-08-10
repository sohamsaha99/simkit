# tests/testthat/test-catalog.R

#' Manually construct a "partial" experiment directory: an in_progress.json
#' marker and a couple of successful replicates, but no manifest.json.
simkit_make_partial_experiment <- function(dir) {
  fs::dir_create(dir)
  simkit_write_in_progress(dir, "2025-01-15T14:32:00Z")
  pair_dir <- fs::path(dir, "s1", "m1")
  fs::dir_create(pair_dir)
  simkit_write_rep(list(est = 1, .rep_index = 1, .elapsed_ms = 1, .warnings = NA_character_),
                    fs::path(pair_dir, "rep_0001.rds"))
  simkit_write_rep(list(est = 2, .rep_index = 2, .elapsed_ms = 1, .warnings = NA_character_),
                    fs::path(pair_dir, "rep_0002.rds"))
}

#' Manually construct an "errored" experiment directory: replicate files on
#' disk (including an error sentinel) but neither an in_progress.json nor a
#' manifest.json -- as if the marker was removed once the process was
#' confirmed dead.
simkit_make_errored_experiment <- function(dir) {
  fs::dir_create(dir)
  pair_dir <- fs::path(dir, "s1", "m1")
  fs::dir_create(pair_dir)
  saveRDS(list(error = "deliberate failure", .rep_index = 1), fs::path(pair_dir, "rep_0001_ERROR.rds"))
}

# ---- finding experiments ----------------------------------------------------

test_that("sim_catalog finds every simkit result folder under a root", {
  root <- withr::local_tempdir()

  design <- base_design(replicates = 4) + sim_storage(path = fs::path(root, "complete_exp"), resume = FALSE)
  sim_run(design, progress = FALSE)

  simkit_make_partial_experiment(fs::path(root, "partial_exp"))
  simkit_make_errored_experiment(fs::path(root, "errored_exp"))
  fs::dir_create(fs::path(root, "not_a_simkit_dir"))

  cat <- sim_catalog(root)

  expect_s3_class(cat, "SimCatalog")
  expect_s3_class(cat, "tbl_df")
  expect_equal(nrow(cat), 3)
  expect_setequal(cat$label, c("complete_exp", "partial_exp", "errored_exp"))
})

test_that("sim_catalog errors on a nonexistent root", {
  expect_error(sim_catalog(tempfile("does_not_exist_")), class = "rlang_error")
})

# ---- status detection --------------------------------------------------------

test_that("a finished run is reported as complete, with full progress", {
  root <- withr::local_tempdir()
  design <- base_design(replicates = 4) + sim_storage(path = fs::path(root, "exp1"), resume = FALSE)
  sim_run(design, progress = FALSE)

  cat <- sim_catalog(root)
  expect_equal(cat$status, "complete")
  expect_equal(cat$completed, 4)
  expect_equal(cat$total, 4)
  expect_equal(cat$scenarios, 1)
  expect_equal(cat$methods, 1)
})

test_that("an in-flight/interrupted run with no manifest is reported as partial", {
  root <- withr::local_tempdir()
  simkit_make_partial_experiment(fs::path(root, "exp1"))

  cat <- sim_catalog(root)
  expect_equal(cat$status, "partial")
  expect_equal(cat$completed, 2)
  expect_true(is.na(cat$total))
})

test_that("a run with on-disk files but no in_progress or manifest marker is errored", {
  root <- withr::local_tempdir()
  simkit_make_errored_experiment(fs::path(root, "exp1"))

  cat <- sim_catalog(root)
  expect_equal(cat$status, "errored")
  expect_equal(cat$completed, 0)
  expect_true(is.na(cat$total))
})

test_that("dplyr::filter works directly on a SimCatalog and preserves its class", {
  root <- withr::local_tempdir()
  design <- base_design(replicates = 2) + sim_storage(path = fs::path(root, "exp1"), resume = FALSE)
  sim_run(design, progress = FALSE)
  simkit_make_partial_experiment(fs::path(root, "exp2"))

  cat <- sim_catalog(root)
  complete_only <- dplyr::filter(cat, status == "complete")

  expect_s3_class(complete_only, "SimCatalog")
  expect_equal(nrow(complete_only), 1)
  expect_equal(complete_only$label, "exp1")
})

# ---- manifest fields ---------------------------------------------------------

test_that("the manifest records label, description, seed, versions, and design_hash", {
  root <- withr::local_tempdir()
  design <- sim_design(replicates = 2, seed = 7, label = "my_label", description = "my description") +
    sim_scenario("s1", list(n = 5, mu = 0), simple_dgp, simple_truth) +
    sim_method("m1", simple_method) +
    sim_storage(path = fs::path(root, "exp1"), resume = FALSE)
  sim_run(design, progress = FALSE)

  manifest <- simkit_read_manifest(fs::path(root, "exp1"))
  expect_equal(manifest$label, "my_label")
  expect_equal(manifest$description, "my description")
  expect_equal(manifest$seed, 7)
  expect_false(is.null(manifest$r_version))
  expect_false(is.null(manifest$simkit_version))
  expect_false(is.null(manifest$design_hash))

  cat <- sim_catalog(root)
  expect_equal(cat$label, "my_label")
  expect_equal(cat$description, "my description")
  expect_equal(cat$seed, 7)
})

# ---- sim_load ------------------------------------------------------------------

test_that("sim_load() recovers a SimResults ready for sim_collect()", {
  root <- withr::local_tempdir()
  design <- base_design(replicates = 6) + sim_storage(path = fs::path(root, "exp1"), resume = FALSE)
  sim_run(design, progress = FALSE)

  loaded <- sim_catalog(root) |> sim_load()
  expect_s3_class(loaded, "SimResults")

  df <- sim_collect(loaded)
  expect_equal(nrow(df), 6)
})

test_that("sim_load() works after dplyr::filter narrows to one row", {
  root <- withr::local_tempdir()
  design <- base_design(replicates = 2) + sim_storage(path = fs::path(root, "exp1"), resume = FALSE)
  sim_run(design, progress = FALSE)
  simkit_make_partial_experiment(fs::path(root, "exp2"))

  loaded <- sim_catalog(root) |> dplyr::filter(status == "complete") |> sim_load()
  expect_equal(nrow(sim_collect(loaded)), 2)
})

test_that("sim_load() rejects catalogs with zero or multiple rows", {
  root <- withr::local_tempdir()
  design <- base_design(replicates = 2) + sim_storage(path = fs::path(root, "exp1"), resume = FALSE)
  sim_run(design, progress = FALSE)
  design2 <- base_design(replicates = 2) + sim_storage(path = fs::path(root, "exp2"), resume = FALSE)
  sim_run(design2, progress = FALSE)

  cat <- sim_catalog(root)
  expect_error(sim_load(cat), class = "rlang_error")
  expect_error(sim_load(dplyr::filter(cat, status == "never")), class = "rlang_error")
})

test_that("sim_load() aborts informatively when no manifest exists", {
  root <- withr::local_tempdir()
  simkit_make_partial_experiment(fs::path(root, "exp1"))

  cat <- sim_catalog(root)
  expect_error(sim_load(cat), class = "rlang_error")
})

test_that("sim_load()'s reconstructed design cannot re-run replicates", {
  root <- withr::local_tempdir()
  design <- base_design(replicates = 2) + sim_storage(path = fs::path(root, "exp1"), resume = FALSE)
  sim_run(design, progress = FALSE)

  loaded <- sim_catalog(root) |> sim_load()
  expect_error(loaded$design$scenarios[[1]]$dgp(loaded$design$scenarios[[1]]$params), class = "rlang_error")
  expect_error(loaded$design$methods[[1]]$fn(1, list()), class = "rlang_error")
  expect_equal(loaded$design$scenarios[[1]]$truth(loaded$design$scenarios[[1]]$params), list())
})

# ---- print methods -------------------------------------------------------------

test_that("print.SimCatalog renders a table without error", {
  root <- withr::local_tempdir()
  design <- base_design(replicates = 2) + sim_storage(path = fs::path(root, "exp1"), resume = FALSE)
  sim_run(design, progress = FALSE)
  simkit_make_partial_experiment(fs::path(root, "exp2"))
  simkit_make_errored_experiment(fs::path(root, "exp3"))

  cat <- sim_catalog(root)
  expect_message(print(cat), "simkit catalog")
  expect_message(print(cat), "3 experiments")
})

test_that("print.SimCatalog(full = TRUE) shows detailed provenance for one row", {
  root <- withr::local_tempdir()
  design <- sim_design(replicates = 2, seed = 3, label = "detail_label") +
    sim_scenario("s1", list(n = 5, mu = 0), simple_dgp, simple_truth) +
    sim_method("m1", simple_method) +
    sim_storage(path = fs::path(root, "exp1"), resume = FALSE)
  sim_run(design, progress = FALSE)

  cat <- sim_catalog(root)
  expect_message(print(cat, full = TRUE), "detail_label")
})

test_that("print.SimCatalog(full = TRUE) requires exactly one row", {
  root <- withr::local_tempdir()
  design <- base_design(replicates = 2) + sim_storage(path = fs::path(root, "exp1"), resume = FALSE)
  sim_run(design, progress = FALSE)
  design2 <- base_design(replicates = 2) + sim_storage(path = fs::path(root, "exp2"), resume = FALSE)
  sim_run(design2, progress = FALSE)

  cat <- sim_catalog(root)
  expect_error(print(cat, full = TRUE), class = "rlang_error")
})

test_that("an empty root prints a catalog with zero experiments", {
  root <- withr::local_tempdir()
  cat <- sim_catalog(root)
  expect_equal(nrow(cat), 0)
  expect_message(print(cat), "0 experiments")
})
