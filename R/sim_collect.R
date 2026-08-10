# R/sim_collect.R -- sim_collect(), sentinel file detection

#' Collect simulation results into a tidy data frame
#'
#' Reads on-disk replicate results written by [sim_run()] into memory as a
#' tidy tibble. Can be run in a fresh R session, independent of any
#' `SimDesign`/`SimResults` object, by passing a storage path directly.
#'
#' @param x Either a storage path (a single character string), or the
#'   `SimResults` object returned by [sim_run()].
#' @param scenarios Optional character vector of scenario names to include;
#'   defaults to all scenarios found on disk.
#' @param methods Optional character vector of method names to include;
#'   defaults to all methods found on disk.
#'
#' @return A tibble with one row per replicate: columns `scenario`,
#'   `method`, `rep`, one column per quantity returned by the method's
#'   `fn`, `elapsed_ms`, `warnings`, and `error` (`NA` unless the replicate
#'   failed, in which case the other quantity columns are `NA` for that
#'   row). When `x` is a `SimResults` object, the scenario truth values
#'   (computed from the design's `truth` functions) are attached as a
#'   `"simkit_truths"` attribute, for use by [sim_evaluate()] -- a bare
#'   storage path carries no such attribute, since truth functions cannot
#'   be recovered from disk.
#' @export
#'
#' @examples
#' design <- sim_design(replicates = 5, seed = 1) +
#'   sim_scenario(
#'     name = "s1", params = list(n = 10, mu = 0),
#'     dgp = function(params) rnorm(params$n, params$mu),
#'     truth = function(params) list(mean = params$mu)
#'   ) +
#'   sim_method(name = "m1", fn = function(data, params) list(est = mean(data))) +
#'   sim_storage(path = tempfile("simkit_"))
#' sim_run(design, progress = FALSE) |> sim_collect()
sim_collect <- function(x, scenarios = NULL, methods = NULL) {
  path <- if (inherits(x, "SimResults")) x$path else x
  if (!is.character(path) || length(path) != 1 || is.na(path)) {
    cli::cli_abort("{.arg x} must be a storage path or a SimResults object.")
  }
  if (!fs::dir_exists(path)) {
    cli::cli_abort("Storage path {.path {path}} does not exist.")
  }

  scenario_dirs <- fs::dir_ls(path, type = "directory")
  if (!is.null(scenarios)) {
    scenario_dirs <- scenario_dirs[fs::path_file(scenario_dirs) %in% scenarios]
  }

  rows <- list()
  for (sdir in scenario_dirs) {
    scenario_name <- fs::path_file(sdir)
    method_dirs <- fs::dir_ls(sdir, type = "directory")
    if (!is.null(methods)) {
      method_dirs <- method_dirs[fs::path_file(method_dirs) %in% methods]
    }
    for (mdir in method_dirs) {
      rows <- c(rows, simkit_collect_pair(scenario_name, fs::path_file(mdir), mdir))
    }
  }

  result <- if (length(rows) == 0) {
    dplyr::bind_rows(data.frame(
      scenario = character(), method = character(),
      rep = integer(), error = character(),
      stringsAsFactors = FALSE
    ))
  } else {
    dplyr::bind_rows(rows)
  }

  if (inherits(x, "SimResults")) {
    attr(result, "simkit_truths") <- simkit_scenario_truths(x$design)
  }

  result
}

#' Read every replicate result (and error sentinel) in one (scenario,
#' method) directory into a list of row-lists
#' @noRd
simkit_collect_pair <- function(scenario_name, method_name, mdir) {
  files <- fs::dir_ls(mdir, regexp = "rep_[0-9]+(_ERROR)?\\.rds$")
  if (length(files) == 0) {
    return(list())
  }

  lapply(files, function(f) {
    rec <- readRDS(f)
    file_name <- fs::path_file(f)
    rep_index <- as.integer(regmatches(file_name, regexpr("[0-9]+", file_name)))
    is_error <- grepl("_ERROR\\.rds$", file_name)

    if (is_error) {
      list(
        scenario = scenario_name, method = method_name, rep = rep_index,
        error = rec$error
      )
    } else {
      user_fields <- rec[!grepl("^\\.", names(rec))]
      c(
        list(scenario = scenario_name, method = method_name, rep = rep_index),
        user_fields,
        list(
          elapsed_ms = rec$.elapsed_ms,
          warnings   = rec$.warnings,
          error      = NA_character_
        )
      )
    }
  })
}
