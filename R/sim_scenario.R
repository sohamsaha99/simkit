# R/sim_scenario.R — sim_scenario(), new_SimScenario(), print.SimScenario()

new_SimScenario <- function(name, params, dgp, truth) {
  stopifnot(is.character(name), length(name) == 1)
  stopifnot(is.list(params))
  stopifnot(is.function(dgp), is.function(truth))
  structure(
    list(name = name, params = params, dgp = dgp, truth = truth),
    class = "SimScenario"
  )
}

#' Define a simulation scenario
#'
#' A scenario pairs a data-generating process (DGP) with a truth function.
#' Scenarios are added to a [sim_design()] with `+`.
#'
#' @param name A single character string, unique within a design.
#' @param params A named list of parameters passed to `dgp` and `truth`.
#' @param dgp A function `function(params)` that returns a simulated dataset.
#' @param truth A function `function(params)` that returns a named list of
#'   estimand values used in metric computation.
#'
#' @return A `SimScenario` object.
#' @export
#'
#' @examples
#' sim_scenario(
#'   name = "normal_n100",
#'   params = list(n = 100, mu = 2, sigma = 1),
#'   dgp = function(params) with(params, rnorm(n, mean = mu, sd = sigma)),
#'   truth = function(params) list(mean = params$mu, variance = params$sigma^2)
#' )
sim_scenario <- function(name, params, dgp, truth) {
  if (!is.character(name) || length(name) != 1 || is.na(name)) {
    cli::cli_abort("{.arg name} must be a single character string.")
  }
  if (!is.list(params)) {
    cli::cli_abort("{.arg params} must be a list.")
  }
  if (!is.function(dgp)) {
    cli::cli_abort("{.arg dgp} must be a function.")
  }
  if (!is.function(truth)) {
    cli::cli_abort("{.arg truth} must be a function.")
  }
  new_SimScenario(name, params, dgp, truth)
}

#' @export
print.SimScenario <- function(x, ...) {
  cli::cli_h1("SimScenario: {x$name}")
  cli::cli_bullets(c(
    "*" = "Params: {.val {names(x$params)}}"
  ))
  invisible(x)
}

#' Expand a parameter grid into one named-list combination per row
#'
#' Internal helper for [sim_scenario_grid()]. Takes a named list where each
#' entry is a scalar or vector of candidate values, and returns a list of
#' named lists -- one per combination in the full factorial cross of the
#' inputs -- via [expand.grid()].
#'
#' @param params A fully named list of parameter values.
#' @return A list of named lists, one per parameter combination.
#' @keywords internal
simkit_expand_params <- function(params) {
  grid <- expand.grid(params, stringsAsFactors = FALSE, KEEP.OUT.ATTRS = FALSE)
  lapply(seq_len(nrow(grid)), function(i) as.list(grid[i, , drop = FALSE]))
}

#' Generate a factorial grid of scenarios
#'
#' `sim_scenario_grid()` takes a named list of parameter values -- some
#' scalar, some vectors of candidates -- and expands it into every
#' combination via [expand.grid()], constructing one [sim_scenario()] per
#' combination. `name_fn` is called on each combination's parameter list to
#' derive its scenario name.
#'
#' @param params A fully named list of parameter values. Entries may be a
#'   single value (held fixed across the grid) or a vector of candidate
#'   values (varied factorially).
#' @param name_fn A function `function(params)` that returns a single
#'   character string naming the scenario for one parameter combination.
#' @param dgp A function `function(params)` that returns a simulated
#'   dataset, shared across all combinations.
#' @param truth A function `function(params)` that returns a named list of
#'   estimand values, shared across all combinations.
#'
#' @return A list of `SimScenario` objects, one per combination, with class
#'   `SimScenarioGrid`. Can be added directly to a [sim_design()] with `+`.
#' @export
#'
#' @examples
#' grid <- sim_scenario_grid(
#'   params = list(n = c(100, 500), mu = 2, sigma = c(0.5, 1.0)),
#'   name_fn = function(p) paste0("n", p$n, "_s", p$sigma),
#'   dgp = function(p) rnorm(p$n, p$mu, p$sigma),
#'   truth = function(p) list(mean = p$mu)
#' )
#' length(grid)
#' @rdname sim_scenario
sim_scenario_grid <- function(params, name_fn, dgp, truth) {
  if (!is.list(params) || length(params) == 0) {
    cli::cli_abort("{.arg params} must be a non-empty named list.")
  }
  if (is.null(names(params)) || any(!nzchar(names(params)))) {
    cli::cli_abort("{.arg params} must be a fully named list.")
  }
  if (!is.function(name_fn)) {
    cli::cli_abort("{.arg name_fn} must be a function.")
  }
  if (!is.function(dgp)) {
    cli::cli_abort("{.arg dgp} must be a function.")
  }
  if (!is.function(truth)) {
    cli::cli_abort("{.arg truth} must be a function.")
  }

  combos <- simkit_expand_params(params)

  scenarios <- lapply(combos, function(p) {
    name <- name_fn(p)
    if (!is.character(name) || length(name) != 1 || is.na(name)) {
      cli::cli_abort("{.arg name_fn} must return a single character string for each combination.")
    }
    sim_scenario(name = name, params = p, dgp = dgp, truth = truth)
  })

  scenario_names <- vapply(scenarios, `[[`, character(1), "name")
  if (anyDuplicated(scenario_names)) {
    cli::cli_abort(c(
      "Duplicate scenario name{?s} produced by {.arg name_fn}: {.val {unique(scenario_names[duplicated(scenario_names)])}}.",
      "i" = "Scenario names must be unique -- results are stored by name, so duplicates would silently overwrite each other."
    ))
  }

  structure(scenarios, class = "SimScenarioGrid")
}
