# tests/testthat/test-storage.R
#
# Alternative storage backends (qs, arrow) are deferred -- see
# dev/session-plan.md Session 10 and NEWS.md. `format` currently accepts
# only "rds"; these tests just lock that down.

test_that("sim_storage defaults to format = 'rds'", {
  storage <- sim_storage(path = tempfile())
  expect_equal(storage$format, "rds")
})

test_that("sim_storage rejects any format other than 'rds'", {
  expect_error(sim_storage(path = tempfile(), format = "qs"), "rds")
  expect_error(sim_storage(path = tempfile(), format = "arrow"), "rds")
})

test_that("format = 'rds' round-trips end-to-end", {
  dir <- withr::local_tempdir()
  design <- base_design(replicates = 10) + sim_storage(path = dir, format = "rds")
  res <- sim_run(design, progress = FALSE)
  df <- sim_collect(res)

  expect_equal(nrow(df), 10)
  expect_true(all(!is.na(df$est)))
  expect_equal(length(fs::dir_ls(fs::path(dir, "s1", "m1"), regexp = "\\.rds$")), 10)
})
