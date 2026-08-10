# R/sim_evaluate.R -- sim_evaluate(), sim_summarize()

#' Compute each scenario's true estimand values
#'
#' Calls every scenario's `truth` function against its own `params`, once.
#' `truth` functions are deterministic given `params` (no dependence on
#' simulated data), so this can be done once per design rather than once
#' per replicate.
#'
#' @noRd
simkit_scenario_truths <- function(design) {
  truths <- lapply(design$scenarios, function(s) s$truth(s$params))
  names(truths) <- vapply(design$scenarios, `[[`, character(1), "name")
  truths
}

#' Evaluate one metric for one (scenario, method) pair
#'
#' Failures degrade to `NA` rather than aborting the whole evaluation: an
#' error thrown by `fn`, or a return value that isn't a single numeric, is
#' reported with one [cli::cli_warn()] and recorded as `NA`. A `NaN` result
#' (e.g. `mean()` of a metric that referenced a column the method never
#' returned, such as a missing confidence interval) is degraded to `NA`
#' silently -- that's an expected consequence of an optional column being
#' absent, not a bug in the metric. Ordinary R warnings raised while
#' evaluating `fn` (e.g. tibble's "unknown column" warning on that same
#' missing-CI-column access) are muffled for the same reason.
#'
#' @noRd
simkit_eval_metric <- function(fn, sims, truth, metric, scenario, method) {
  value <- tryCatch(
    withCallingHandlers(
      fn(sims, truth),
      warning = function(w) invokeRestart("muffleWarning")
    ),
    error = function(e) {
      cli::cli_warn(c(
        "!" = "Metric {.val {metric}} failed for {.val {scenario}} x {.val {method}}.",
        "i" = conditionMessage(e)
      ))
      NA_real_
    }
  )

  if (!is.numeric(value) || length(value) != 1) {
    cli::cli_warn(c(
      "!" = "Metric {.val {metric}} did not return a single numeric value for {.val {scenario}} x {.val {method}}.",
      "i" = "Got a value of class {.cls {class(value)}} and length {length(value)}."
    ))
    return(NA_real_)
  }

  if (is.nan(value)) NA_real_ else as.numeric(value)
}

#' Evaluate simulation results against ground truth
#'
#' Maps every user-supplied metric function over each (scenario, method)
#' pair's successful replicates, comparing against the scenario's true
#' estimand values, and returns a tidy long-format tibble.
#'
#' @param x Either the `SimResults` object returned by [sim_run()], or a
#'   data frame of already-collected results produced by
#'   `sim_collect(<SimResults>)`. Because scenario truth values are derived
#'   from the design's `truth` functions -- which cannot be recovered from a
#'   bare storage path -- `x` must trace back to a `SimResults` object; a
#'   data frame collected from a path string (rather than a `SimResults`
#'   handle) does not carry truth information and will cause
#'   `sim_evaluate()` to abort.
#' @param ... Named metric functions, each `function(sims, truth)`. `sims`
#'   is a data frame of one (scenario, method) pair's successful replicates
#'   (rows with a non-`NA` `error` are excluded); `truth` is the named list
#'   returned by that scenario's `truth` function. If a metric errors, or
#'   returns something other than a single numeric value, `sim_evaluate()`
#'   records `NA` for that (metric, scenario, method) cell and emits a
#'   single [cli::cli_warn()] rather than aborting the rest of the
#'   evaluation.
#'
#' @return A long-format tibble with columns `scenario`, `method`,
#'   `metric`, `value`.
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
#' evaluation <- sim_evaluate(
#'   results,
#'   bias = function(sims, truth) mean(sims$est) - truth$mean
#' )
#' evaluation
sim_evaluate <- function(x, ...) {
  metrics <- list(...)
  if (length(metrics) == 0) {
    cli::cli_abort(
      "Supply at least one named metric function, e.g. {.code sim_evaluate(x, bias = function(sims, truth) ...)}."
    )
  }
  metric_names <- names(metrics)
  if (is.null(metric_names) || any(metric_names == "")) {
    cli::cli_abort("Every metric passed via {.arg ...} must be named.")
  }
  is_fn <- vapply(metrics, is.function, logical(1))
  if (!all(is_fn)) {
    cli::cli_abort("Metric {.val {metric_names[!is_fn][1]}} must be a function.")
  }

  df <- if (inherits(x, "SimResults")) sim_collect(x) else x
  if (!is.data.frame(df)) {
    cli::cli_abort(
      "{.arg x} must be a SimResults object or a data frame of collected results (see {.fn sim_collect})."
    )
  }

  truths <- attr(df, "simkit_truths")
  if (is.null(truths)) {
    cli::cli_abort(c(
      "Cannot determine scenario truth values from {.arg x}.",
      "i" = "Pass the {.cls SimResults} object returned by {.fn sim_run}, or the data frame from {.code sim_collect(<SimResults>)}, so truth values can be derived from the design.",
      "i" = "A data frame collected from a bare storage path does not carry the scenario truth functions."
    ))
  }

  pairs <- unique(df[c("scenario", "method")])

  rows <- vector("list", nrow(pairs) * length(metrics))
  i <- 0
  for (p in seq_len(nrow(pairs))) {
    scen <- pairs$scenario[[p]]
    meth <- pairs$method[[p]]
    sims <- df[df$scenario == scen & df$method == meth & is.na(df$error), , drop = FALSE]
    truth <- truths[[scen]]

    for (nm in metric_names) {
      i <- i + 1
      rows[[i]] <- list(
        scenario = scen, method = meth, metric = nm,
        value = simkit_eval_metric(metrics[[nm]], sims, truth, nm, scen, meth)
      )
    }
  }

  dplyr::bind_rows(rows)
}

#' Summarize an evaluation in wide format
#'
#' Pivots the long-format tibble returned by [sim_evaluate()] to one row per
#' (scenario, method) pair and one column per metric.
#'
#' @param evaluation A long-format tibble as returned by [sim_evaluate()],
#'   with columns `scenario`, `method`, `metric`, `value`.
#'
#' @return A wide-format tibble with columns `scenario`, `method`, and one
#'   column per metric.
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
#' evaluation <- sim_evaluate(
#'   results,
#'   bias = function(sims, truth) mean(sims$est) - truth$mean
#' )
#' sim_summarize(evaluation)
sim_summarize <- function(evaluation) {
  required <- c("scenario", "method", "metric", "value")
  if (!is.data.frame(evaluation) || !all(required %in% names(evaluation))) {
    cli::cli_abort(
      "{.arg evaluation} must be the tibble returned by {.fn sim_evaluate}, with columns {.val scenario}, {.val method}, {.val metric}, and {.val value}."
    )
  }
  tidyr::pivot_wider(evaluation, names_from = "metric", values_from = "value")
}
