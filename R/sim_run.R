# R/sim_run.R -- sim_run(), internal dispatch loop, on_error handling

new_SimResults <- function(path, design) {
  stopifnot(is.character(path), length(path) == 1)
  stopifnot(inherits(design, "SimDesign"))
  structure(list(path = path, design = design), class = "SimResults")
}

#' @export
print.SimResults <- function(x, ...) {
  cli::cli_h1("SimResults")
  cli::cli_bullets(c(
    "*" = "Path:  {x$path}",
    "*" = "Label: {x$design$label %||% '(unnamed)'}"
  ))
  invisible(x)
}

#' Run a simulation design sequentially
#'
#' Enumerates every (scenario, method, replicate) triple and executes it
#' one at a time, checkpointing each result to disk immediately upon
#' completion. If `resume = TRUE` on the design's [sim_storage()] (the
#' default), replicates already on disk are skipped.
#'
#' @param design A `SimDesign` with at least one scenario and one method.
#' @param progress If `TRUE` (default), report progress via
#'   [progressr::progressor()]. Silent unless the caller has registered
#'   `progressr` handlers.
#' @param on_error One of `"warn"` (default; store an error sentinel and
#'   emit [cli::cli_warn()]), `"skip"` (store an error sentinel silently),
#'   or `"stop"` (re-throw the error, halting the run).
#' @param check If `TRUE`, run [sim_check()] first (against its own scratch
#'   directory, never the design's configured storage) and abort the run if
#'   any (scenario, method) pair fails. Default `FALSE`. Check replicates
#'   never count toward `resume` progress; the full run's replicate
#'   numbering is unaffected.
#'
#' @return A `SimResults` object referencing the on-disk results. Pass it
#'   to [sim_collect()] to materialize a tidy tibble.
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
#' results <- sim_run(design, progress = FALSE)
#' sim_collect(results)
sim_run <- function(design, progress = TRUE, on_error = "warn", check = FALSE) {
  if (!inherits(design, "SimDesign")) {
    cli::cli_abort("{.arg design} must be a SimDesign.")
  }
  if (length(design$scenarios) == 0) {
    cli::cli_abort("Design has no scenarios. Add one with {.fn sim_scenario}.")
  }
  if (length(design$methods) == 0) {
    cli::cli_abort("Design has no methods. Add one with {.fn sim_method}.")
  }
  if (!is.character(on_error) || length(on_error) != 1 ||
      !on_error %in% c("warn", "skip", "stop")) {
    cli::cli_abort("{.arg on_error} must be one of {.val warn}, {.val skip}, or {.val stop}.")
  }
  if (!is.logical(check) || length(check) != 1 || is.na(check)) {
    cli::cli_abort("{.arg check} must be a single {.code TRUE}/{.code FALSE}.")
  }

  if (isTRUE(check)) {
    check_result <- sim_check(design)
    if (!simkit_check_passed(check_result)) {
      cli::cli_abort(c(
        "Pre-flight check failed -- aborting {.fn sim_run}.",
        "i" = "Run {.fn sim_check} directly to see full error details for each pair."
      ))
    }
  }

  storage <- design$storage %||% sim_storage(path = tempfile("simkit_"))
  design$storage <- storage
  fs::dir_create(storage$path)

  if (storage$resume) {
    simkit_check_storage_conflict(storage$path, design)
  }

  old_seed <- if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
    get(".Random.seed", envir = .GlobalEnv)
  } else {
    NULL
  }
  on.exit({
    if (!is.null(old_seed)) {
      assign(".Random.seed", old_seed, envir = .GlobalEnv)
    } else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
      rm(".Random.seed", envir = .GlobalEnv)
    }
  }, add = TRUE)

  use_parallel <- !is.null(design$parallel) &&
    !identical(design$parallel$backend, "sequential") &&
    design$parallel$workers > 1

  if (use_parallel) {
    for (method in design$methods) simkit_check_closure(method$fn, "fn")
    for (scenario in design$scenarios) simkit_check_closure(scenario$dgp, "dgp")

    old_plan <- future::plan()
    on.exit(future::plan(old_plan), add = TRUE)
    future::plan(future::multisession, workers = design$parallel$workers)
  }

  created_at <- format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  simkit_write_in_progress(storage$path, created_at)

  n_scenarios <- length(design$scenarios)
  n_methods <- length(design$methods)
  total_reps <- n_scenarios * n_methods * design$replicates
  progress_fn <- if (isTRUE(progress)) progressr::progressor(steps = total_reps) else NULL

  # Every (scenario, method, replicate) triple gets a seed from a single
  # L'Ecuyer-CMRG stream chain, keyed by its fixed position in this loop --
  # not by dispatch order -- so sequential and parallel runs of the same
  # design and seed produce byte-identical results.
  all_seeds <- simkit_make_seeds(design$seed, total_reps)

  for (scenario_idx in seq_len(n_scenarios)) {
    scenario <- design$scenarios[[scenario_idx]]
    for (method_idx in seq_len(n_methods)) {
      method <- design$methods[[method_idx]]
      offset <- ((scenario_idx - 1) * n_methods + (method_idx - 1)) * design$replicates
      pair_seeds <- all_seeds[(offset + 1):(offset + design$replicates)]
      simkit_run_pair(scenario, method, design, storage, on_error, pair_seeds, progress_fn, use_parallel)
    }
  }

  completed_at <- format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  simkit_write_manifest(storage$path, design, created_at, completed_at)
  simkit_remove_in_progress(storage$path)

  new_SimResults(path = storage$path, design = design)
}

#' Run every replicate of one (scenario, method) pair
#'
#' @param pair_seeds A list of length `design$replicates`, one
#'   `.Random.seed`-compatible seed per replicate index (see
#'   `simkit_make_seeds()`).
#' @param use_parallel If `TRUE`, dispatch pending replicates via
#'   [future.apply::future_lapply()] under whatever `future::plan()` the
#'   caller has already set; otherwise run them one at a time in-process.
#' @noRd
simkit_run_pair <- function(scenario, method, design, storage, on_error,
                             pair_seeds, progress_fn = NULL, use_parallel = FALSE) {
  pair_dir <- fs::path(storage$path, scenario$name, method$name)
  fs::dir_create(pair_dir)

  completed <- if (storage$resume) simkit_completed_reps(pair_dir) else integer(0)
  pending <- setdiff(seq_len(design$replicates), completed)

  if (!is.null(progress_fn)) {
    for (i in seq_along(completed)) progress_fn()
  }

  run_one <- simkit_replicate_runner(scenario, method, pair_dir, on_error)

  if (use_parallel && length(pending) > 0) {
    # `future.seed = NULL` disables future.apply's automatic-RNG-safety
    # check: replicates seed their own stream explicitly (see
    # simkit_replicate_runner()), so future's own seed management would
    # only get in the way. The "package:simkit may not be available"
    # warning is `future.apply` estimating payload size for a closure
    # whose lexical scope chains through the simkit namespace -- true of
    # any closure literal defined inside an internal package function --
    # and is benign: it reflects `devtools::load_all()`-based development,
    # not an installed package, and does not affect the result.
    withCallingHandlers(
      future.apply::future_lapply(
        pending,
        function(rep_index) run_one(rep_index, pair_seeds[[rep_index]]),
        future.seed = NULL
      ),
      warning = function(w) {
        if (grepl("may not be available when loading", conditionMessage(w), fixed = TRUE)) {
          invokeRestart("muffleWarning")
        }
      }
    )
  } else {
    for (rep_index in pending) run_one(rep_index, pair_seeds[[rep_index]])
  }

  if (!is.null(progress_fn)) {
    for (i in seq_along(pending)) progress_fn()
  }

  invisible(NULL)
}

#' Naive per-replicate seed derivation, used only by `sim_check()`
#'
#' `sim_check()` always runs strictly sequentially in a scratch directory, so
#' it has no need for the reproducible-across-backends guarantee
#' `simkit_make_seeds()` provides -- this simpler, order-independent-by-name
#' derivation is enough for its throwaway trial replicates.
#'
#' @noRd
simkit_naive_seed <- function(seed, scenario_name, method_name, rep_index) {
  key <- paste(seed, scenario_name, method_name, rep_index, sep = "::")
  digest::digest2int(key)
}

#' Build a closure that executes one replicate for a fixed (scenario, method)
#' pair, returning it for use by both the sequential and parallel dispatch
#' paths in `simkit_run_pair()`
#'
#' Deliberately calls no other `simkit_*` function in its body (atomic-write
#' and error-condition construction are inlined below rather than delegated
#' to `simkit_write_rep()` / a shared error constructor). The returned
#' closure is a plain value bound in this call's own frame, not a symbol in
#' the `simkit` namespace -- when shipped to a `future` multisession worker,
#' `future`'s globals-scanning can serialize it directly, rather than
#' needing the (possibly dev-loaded, not-yet-installed) `simkit` namespace to
#' be loadable in that separate worker process.
#'
#' @noRd
simkit_replicate_runner <- function(scenario, method, pair_dir, on_error) {
  function(rep_index, seed) {
    rep_path <- fs::path(pair_dir, sprintf("rep_%04d.rds", rep_index))
    params <- utils::modifyList(scenario$params, list(.rep_index = rep_index, .rng_seed = seed[2]))

    warn_env <- new.env(parent = emptyenv())
    warn_env$msgs <- character(0)

    outcome <- tryCatch(
      withCallingHandlers(
        {
          assign(".Random.seed", seed, envir = .GlobalEnv)
          start_time <- Sys.time()
          data <- scenario$dgp(params)
          out <- method$fn(data, params)
          elapsed_ms <- as.numeric(Sys.time() - start_time, units = "secs") * 1000

          if (!is.list(out)) {
            cli::cli_abort("Method {.val {method$name}} must return a list, not {.cls {class(out)}}.")
          }

          list(value = out, elapsed_ms = elapsed_ms)
        },
        warning = function(w) {
          warn_env$msgs <- c(warn_env$msgs, conditionMessage(w))
          invokeRestart("muffleWarning")
        }
      ),
      error = function(e) {
        if (identical(on_error, "stop")) {
          stop(e)
        }
        structure(
          list(
            message = conditionMessage(e), scenario = scenario$name, method = method$name,
            rep = rep_index, call = rlang::caller_env()
          ),
          class = c("simkit_rep_error", "error", "condition")
        )
      }
    )

    if (inherits(outcome, "simkit_rep_error")) {
      if (identical(on_error, "warn")) {
        cli::cli_warn(c(
          "!" = "Replicate {rep_index} of {.val {scenario$name}} x {.val {method$name}} failed.",
          "i" = outcome$message
        ))
      }
      err_path <- fs::path(pair_dir, sprintf("rep_%04d_ERROR.rds", rep_index))
      err_tmp <- paste0(err_path, ".tmp")
      saveRDS(list(error = outcome$message, .rep_index = rep_index), err_tmp)
      file.rename(err_tmp, err_path)
      return(invisible(NULL))
    }

    record <- outcome$value
    if (identical(method$store, "summary")) {
      record <- record[!grepl("^\\.", names(record))]
    }
    record$.rep_index <- rep_index
    record$.elapsed_ms <- outcome$elapsed_ms
    record$.warnings <- if (length(warn_env$msgs) == 0) NA_character_ else paste(warn_env$msgs, collapse = "; ")

    tmp <- paste0(rep_path, ".tmp")
    saveRDS(record, tmp)
    file.rename(tmp, rep_path)
    invisible(NULL)
  }
}
