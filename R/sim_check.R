# R/sim_check.R -- sim_check(), new_SimCheck(), print.SimCheck()

new_SimCheck <- function(results, replicates, design) {
  stopifnot(is.list(results))
  stopifnot(is.numeric(replicates), length(replicates) == 1)
  stopifnot(inherits(design, "SimDesign"))
  structure(
    list(results = results, replicates = replicates, design = design),
    class = "SimCheck"
  )
}

#' Pre-flight check a simulation design
#'
#' Runs a small number of trial replicates of every (scenario, method) pair,
#' strictly sequentially, before committing to a full [sim_run()]. This
#' surfaces bugs cheaply -- before any parallel infrastructure is spun up --
#' and reports a per-pair timing estimate for the full run.
#'
#' Trial replicates are written to an ephemeral scratch directory, never to
#' the design's configured [sim_storage()] path, and the scratch directory
#' is removed before `sim_check()` returns (whether the check passes or
#' fails).
#'
#' @param design A `SimDesign` with at least one scenario and one method.
#' @param replicates Number of trial replicates to run per (scenario,
#'   method) pair. Default `2`.
#'
#' @return A `SimCheck` object, invisibly. A formatted pass/fail report is
#'   printed as a side effect.
#' @export
#'
#' @examples
#' design <- sim_design(replicates = 100, seed = 1) +
#'   sim_scenario(
#'     name = "s1", params = list(n = 10, mu = 0),
#'     dgp = function(params) rnorm(params$n, params$mu),
#'     truth = function(params) list(mean = params$mu)
#'   ) +
#'   sim_method(name = "m1", fn = function(data, params) list(est = mean(data)))
#' sim_check(design, replicates = 2)
sim_check <- function(design, replicates = 2) {
  if (!inherits(design, "SimDesign")) {
    cli::cli_abort("{.arg design} must be a SimDesign.")
  }
  if (length(design$scenarios) == 0) {
    cli::cli_abort("Design has no scenarios. Add one with {.fn sim_scenario}.")
  }
  if (length(design$methods) == 0) {
    cli::cli_abort("Design has no methods. Add one with {.fn sim_method}.")
  }
  if (!is.numeric(replicates) || length(replicates) != 1 || replicates < 1) {
    cli::cli_abort("{.arg replicates} must be a single positive number.")
  }

  scratch_dir <- tempfile("simkit_check_")
  fs::dir_create(scratch_dir)
  on.exit(if (fs::dir_exists(scratch_dir)) fs::dir_delete(scratch_dir), add = TRUE)

  results <- list()
  for (scenario in design$scenarios) {
    for (method in design$methods) {
      results[[length(results) + 1]] <- simkit_check_pair(
        scenario, method, replicates, design, scratch_dir
      )
    }
  }

  check <- new_SimCheck(results, replicates, design)
  print(check)
  invisible(check)
}

#' Whether every pair in a SimCheck passed
#' @noRd
simkit_check_passed <- function(x) {
  all(vapply(x$results, function(pair) pair$ok == pair$replicates, logical(1)))
}

#' Run the trial replicates for one (scenario, method) pair
#' @noRd
simkit_check_pair <- function(scenario, method, replicates, design, scratch_dir) {
  pair_dir <- fs::path(scratch_dir, scenario$name, method$name)
  fs::dir_create(pair_dir)

  ok_count <- 0L
  elapsed_ms <- numeric(0)
  first_error <- NULL

  for (rep_index in seq_len(replicates)) {
    seed <- simkit_naive_seed(design$seed, scenario$name, method$name, rep_index)
    outcome <- simkit_check_replicate(scenario, method, rep_index, seed)

    if (isTRUE(outcome$ok)) {
      ok_count <- ok_count + 1L
      elapsed_ms <- c(elapsed_ms, outcome$elapsed_ms)
      rep_path <- fs::path(pair_dir, sprintf("rep_%04d.rds", rep_index))
      simkit_write_rep(outcome$value, rep_path)
    } else if (is.null(first_error)) {
      first_error <- outcome
    }
  }

  list(
    scenario      = scenario$name,
    method        = method$name,
    replicates    = replicates,
    ok            = ok_count,
    ms_per_rep    = if (length(elapsed_ms) > 0) mean(elapsed_ms) else NA_real_,
    error_message = if (is.null(first_error)) NA_character_ else first_error$message,
    traceback     = if (is.null(first_error)) NA_character_ else simkit_format_traceback(first_error$calls)
  )
}

#' Run a single trial replicate, capturing timing or the full error stack
#'
#' Unlike `simkit_run_replicate()` (used by the real run), this captures the
#' call stack at the moment of failure via a calling handler, so the printed
#' report can show a traceback rather than just a message.
#'
#' @noRd
simkit_check_replicate <- function(scenario, method, rep_index, seed) {
  captured_calls <- NULL
  params <- utils::modifyList(scenario$params, list(.rep_index = rep_index, .rng_seed = seed))

  tryCatch(
    withCallingHandlers(
      {
        set.seed(seed)
        start_time <- Sys.time()
        data <- scenario$dgp(params)
        out <- method$fn(data, params)
        elapsed_ms <- as.numeric(Sys.time() - start_time, units = "secs") * 1000

        if (!is.list(out)) {
          cli::cli_abort("Method {.val {method$name}} must return a list, not {.cls {class(out)}}.")
        }

        list(ok = TRUE, value = out, elapsed_ms = elapsed_ms)
      },
      error = function(e) {
        captured_calls <<- sys.calls()
      }
    ),
    error = function(e) {
      list(ok = FALSE, message = conditionMessage(e), calls = captured_calls)
    }
  )
}

#' Regex matching internal condition-dispatch frames to drop from a captured
#' call stack -- these are R's own error-handling machinery (invoked while
#' searching for and calling our calling handler), not part of the user's
#' code path, and would otherwise dominate the tail of `sys.calls()`.
#' @noRd
SIMKIT_TRACEBACK_NOISE <- paste0(
  "^(\\.handleSimpleError|doTryCatch|tryCatchOne|tryCatchList|tryCatch|",
  "withCallingHandlers|simpleError|simpleCondition|stop|h|simkit_check_replicate)\\("
)

#' Format a captured call stack as an arrow-joined traceback string
#' @noRd
simkit_format_traceback <- function(calls) {
  if (is.null(calls) || length(calls) == 0) {
    return(NA_character_)
  }
  deparsed <- vapply(calls, function(call) {
    paste(trimws(deparse(call)), collapse = " ")
  }, character(1))
  deparsed <- deparsed[!grepl(SIMKIT_TRACEBACK_NOISE, deparsed)]
  if (length(deparsed) == 0) {
    return(NA_character_)
  }
  n <- length(deparsed)
  keep <- deparsed[seq(max(1, n - 3), n)]
  paste(keep, collapse = " \u2192 ")
}

#' Format a millisecond duration as a human-readable string
#' @noRd
simkit_format_duration <- function(ms) {
  secs <- ms / 1000
  if (secs < 60) {
    return(sprintf("%.0f sec", secs))
  }
  mins <- secs / 60
  if (mins < 60) {
    return(sprintf("%.1f min", mins))
  }
  hrs <- mins / 60
  if (hrs < 24) {
    return(sprintf("%.1f hrs", hrs))
  }
  sprintf("%.1f days", hrs / 24)
}

#' Estimate the full run's duration from a per-pair sequential timing
#'
#' The naive sequential estimate (`ms_per_rep * design$replicates`) is
#' divided by the design's configured parallel worker count, if any, since
#' `sim_check()` always times replicates sequentially regardless of the
#' backend the real run will use.
#'
#' @noRd
simkit_format_eta <- function(ms_per_rep, design) {
  total_ms <- ms_per_rep * design$replicates
  if (!is.null(design$parallel)) {
    workers <- design$parallel$workers
    total_ms <- total_ms / workers
    suffix <- sprintf(" total (parallel, %d worker%s)", workers, if (workers == 1) "" else "s")
  } else {
    suffix <- " total"
  }
  paste0("~", simkit_format_duration(total_ms), suffix)
}

#' @export
print.SimCheck <- function(x, ...) {
  cli::cli_rule("simkit pre-flight check")

  dot <- "\u00b7"
  times <- "\u00d7"

  for (pair in x$results) {
    label <- sprintf("%s %s %s", pair$scenario, times, pair$method)
    if (pair$ok == pair$replicates) {
      eta <- simkit_format_eta(pair$ms_per_rep, x$design)
      cli::cli_text(
        "{cli::col_green(cli::symbol$tick)} {label} [{pair$ok}/{pair$replicates} ok {dot} {round(pair$ms_per_rep, 1)} ms/rep {dot} {eta}]"
      )
    } else {
      cli::cli_text("{cli::col_red(cli::symbol$cross)} {label} [{pair$ok}/{pair$replicates} ok]")
      cli::cli_bullets(c(" " = "Error: {pair$error_message}"))
      if (!is.na(pair$traceback)) {
        cli::cli_bullets(c(" " = "Traceback: {pair$traceback}"))
      }
    }
  }

  cli::cli_rule()

  n_pairs <- length(x$results)
  n_passed <- sum(vapply(x$results, function(pair) pair$ok == pair$replicates, logical(1)))
  n_failed <- n_pairs - n_passed
  fix_note <- if (n_failed > 0) paste0(" ", dot, " fix errors before running") else ""

  summary_bits <- c(
    "*" = "{n_pairs} pair{?s} checked {dot} {n_passed} passed {dot} {n_failed} failed{fix_note}"
  )
  cli::cli_bullets(summary_bits)

  invisible(x)
}
