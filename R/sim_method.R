# R/sim_method.R — sim_method(), new_SimMethod(), print.SimMethod()

new_SimMethod <- function(name, fn, store) {
  stopifnot(is.character(name), length(name) == 1)
  stopifnot(is.function(fn))
  stopifnot(is.character(store), length(store) == 1)
  structure(
    list(name = name, fn = fn, store = store),
    class = "SimMethod"
  )
}

#' Define a simulation method
#'
#' A method is a named estimator. Its function receives the simulated
#' dataset and the scenario's `params`, and returns a named list of
#' estimates. Methods are added to a [sim_design()] with `+`.
#'
#' @param name A single character string, unique within a design.
#' @param fn A function `function(data, params)` that returns a named list
#'   of estimates.
#' @param store Either `"summary"` (default; drop `.`-prefixed fields from
#'   the main results) or `"full"` (store everything).
#'
#' @return A `SimMethod` object.
#' @export
#'
#' @examples
#' sim_method(
#'   name = "sample_mean",
#'   fn = function(data, params) list(est = mean(data), se = sd(data) / sqrt(length(data)))
#' )
sim_method <- function(name, fn, store = "summary") {
  if (!is.character(name) || length(name) != 1 || is.na(name)) {
    cli::cli_abort("{.arg name} must be a single character string.")
  }
  if (!is.function(fn)) {
    cli::cli_abort("{.arg fn} must be a function.")
  }
  if (!is.character(store) || length(store) != 1 || !store %in% c("summary", "full")) {
    cli::cli_abort("{.arg store} must be either {.val summary} or {.val full}.")
  }
  new_SimMethod(name, fn, store)
}

#' @export
print.SimMethod <- function(x, ...) {
  cli::cli_h1("SimMethod: {x$name}")
  cli::cli_bullets(c(
    "*" = "Store: {x$store}"
  ))
  invisible(x)
}

# ---- Bootstrap wrapper (Session 7) -----------------------------------------

#' Wrap an estimator with bootstrap variance estimation
#'
#' Returns a `function(data, params)` suitable for [sim_method()]'s `fn`
#' argument. On each call it runs `estimator` once on the observed data (to
#' produce the point estimate returned to the user) and `B` more times on
#' bootstrap resamples of `data`, then appends the requested bootstrap
#' summaries of `estimator`'s `statistic` field to the point estimate's
#' output.
#'
#' @param estimator A function `function(data, params)` returning a named
#'   list that includes `statistic`.
#' @param B Number of bootstrap replicates. Default `500`.
#' @param statistic Name of the element of `estimator`'s return list to
#'   bootstrap. Default `"est"`.
#' @param output Character vector, a subset of `c("se", "ci")`. `"se"` adds
#'   a single `$se` field (the bootstrap standard deviation of `statistic`).
#'   `"ci"` adds **two** fields, `$ci_lo` and `$ci_hi` (the 2.5th/97.5th
#'   percentiles of the bootstrap distribution) -- not a combined `$ci`
#'   field. Default `c("se", "ci")`.
#' @param parallel If `TRUE`, dispatch the `B` bootstrap replicates via
#'   [future.apply::future_lapply()] under whatever `future::plan()` is
#'   currently active, nested inside any outer replicate-level parallelism
#'   already running (coding-guidelines sec 5). Requires the caller to have
#'   already configured a nested `future::plan()` topology; `simkit` warns
#'   via [cli::cli_warn()] if the outer worker count (read from
#'   `getOption("simkit.outer_workers", 1L)`) times the inner worker count
#'   (the currently active plan's `future::nbrOfWorkers()`) exceeds
#'   `parallel::detectCores()`. Default `FALSE`.
#'
#' @return A function `function(data, params)` returning a named list with
#'   `statistic` (and any other fields `estimator` returns) plus the
#'   requested bootstrap summaries.
#' @export
#'
#' @examples
#' boot_mean <- sim_bootstrap(
#'   estimator = function(data, params) list(est = mean(data)),
#'   B = 20,
#'   statistic = "est",
#'   output = "se"
#' )
#' boot_mean(rnorm(30), list())
sim_bootstrap <- function(estimator, B = 500, statistic = "est",
                           output = c("se", "ci"), parallel = FALSE) {
  if (!is.function(estimator)) {
    cli::cli_abort("{.arg estimator} must be a function.")
  }
  if (!is.numeric(B) || length(B) != 1 || B < 2) {
    cli::cli_abort("{.arg B} must be a single number of at least 2.")
  }
  if (!is.character(statistic) || length(statistic) != 1 || is.na(statistic)) {
    cli::cli_abort("{.arg statistic} must be a single character string.")
  }
  if (!is.character(output) || length(output) == 0 || !all(output %in% c("se", "ci"))) {
    cli::cli_abort("{.arg output} must be a subset of {.val se} and {.val ci}.")
  }
  if (!is.logical(parallel) || length(parallel) != 1 || is.na(parallel)) {
    cli::cli_abort("{.arg parallel} must be a single {.code TRUE}/{.code FALSE}.")
  }

  function(data, params) {
    base_out <- estimator(data, params)
    if (!is.list(base_out) || !statistic %in% names(base_out)) {
      cli::cli_abort("{.arg estimator} must return a list containing {.val {statistic}}.")
    }

    if (isTRUE(parallel)) {
      inner_workers <- future::nbrOfWorkers()
      outer_workers <- getOption("simkit.outer_workers", 1L)
      simkit_check_oversubscription(outer_workers, inner_workers)

      # Deliberately inlined rather than calling simkit_boot_once(): a
      # future multisession worker attaches whatever package a dispatched
      # closure's free variables resolve into, and simkit_boot_once is
      # bound in the simkit namespace -- fatal when simkit is dev-loaded
      # rather than installed (see the analogous note on
      # simkit_replicate_runner() in sim_run.R). Inlining keeps every free
      # variable a plain local value.
      boot_once <- function(i) {
        n <- NROW(data)
        idx <- sample.int(n, n, replace = TRUE)
        boot_data <- if (is.data.frame(data) || is.matrix(data)) data[idx, , drop = FALSE] else data[idx]
        out <- estimator(boot_data, params)
        if (!is.list(out) || !statistic %in% names(out)) {
          cli::cli_abort("{.arg estimator} must return a list containing {.val {statistic}}.")
        }
        out[[statistic]]
      }
      boot_stats <- unlist(future.apply::future_lapply(seq_len(B), boot_once, future.seed = TRUE))
    } else {
      boot_stats <- vapply(
        seq_len(B),
        function(i) simkit_boot_once(data, params, estimator, statistic),
        numeric(1)
      )
    }

    if ("se" %in% output) {
      base_out$se <- stats::sd(boot_stats, na.rm = TRUE)
    }
    if ("ci" %in% output) {
      ci <- stats::quantile(boot_stats, probs = c(0.025, 0.975), na.rm = TRUE, names = FALSE)
      base_out$ci_lo <- ci[[1]]
      base_out$ci_hi <- ci[[2]]
    }
    base_out
  }
}

#' Run `estimator` once on a bootstrap resample of `data`
#'
#' `data` is resampled with replacement along its first dimension --
#' `data[idx, , drop = FALSE]` for a data frame or matrix, `data[idx]`
#' otherwise -- so this works for the vector, data-frame, and matrix shapes
#' a `dgp` can return.
#'
#' @noRd
simkit_boot_once <- function(data, params, estimator, statistic) {
  n <- NROW(data)
  idx <- sample.int(n, n, replace = TRUE)
  boot_data <- if (is.data.frame(data) || is.matrix(data)) data[idx, , drop = FALSE] else data[idx]
  out <- estimator(boot_data, params)
  if (!is.list(out) || !statistic %in% names(out)) {
    cli::cli_abort("{.arg estimator} must return a list containing {.val {statistic}}.")
  }
  out[[statistic]]
}

#' Warn when nested bootstrap parallelism would oversubscribe the machine
#'
#' `sim_bootstrap(parallel = TRUE)` runs a second, inner `future` plan
#' nested inside whatever outer plan [sim_parallel()] is already running.
#' Neither level automatically divides cores with the other, so if both
#' default to "use all cores," total worker processes multiply
#' (`outer_workers x inner_workers`), oversubscribing the machine.
#'
#' @noRd
simkit_check_oversubscription <- function(outer_workers, inner_workers) {
  total <- outer_workers * inner_workers
  cores <- parallel::detectCores()
  if (total > cores) {
    cli::cli_warn(c(
      "!" = paste0(
        "Nested parallelism requests {total} worker processes ",
        "({outer_workers} outer x {inner_workers} inner), but only {cores} cores are available."
      ),
      "i" = paste0(
        "Consider a nested {.fn future::plan} topology via {.fn future::tweak}, ",
        "or set {.arg parallel = FALSE} in {.fn sim_bootstrap}."
      )
    ))
  }
}
