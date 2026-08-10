# R/sim_design.R -- sim_design(), new_SimDesign(), validate_SimDesign(),
# print.SimDesign(), +.SimDesign()

new_SimDesign <- function(replicates, seed, label, description,
                           scenarios = list(), methods = list(),
                           parallel = NULL, storage = NULL) {
  stopifnot(is.numeric(replicates), length(replicates) == 1)
  stopifnot(is.numeric(seed), length(seed) == 1)
  stopifnot(is.null(label) || (is.character(label) && length(label) == 1))
  stopifnot(is.null(description) || (is.character(description) && length(description) == 1))
  stopifnot(is.list(scenarios))
  stopifnot(is.list(methods))
  stopifnot(is.null(parallel) || inherits(parallel, "SimParallel"))
  stopifnot(is.null(storage) || inherits(storage, "SimStorage"))
  structure(
    list(
      replicates = replicates,
      seed = seed,
      label = label,
      description = description,
      scenarios = scenarios,
      methods = methods,
      parallel = parallel,
      storage = storage
    ),
    class = "SimDesign"
  )
}

#' Validate a SimDesign
#'
#' Checks structural invariants that must hold at every point in a design's
#' construction -- currently, that scenario names are unique and method names
#' are unique. Called by [`+.SimDesign`] after every append, so a naming
#' collision is caught at the call site that introduced it.
#'
#' @param x A `SimDesign` object.
#' @return `x`, invisibly, if valid; otherwise aborts via [cli::cli_abort()].
#' @export
validate_SimDesign <- function(x) {
  if (length(x$scenarios) > 0) {
    scenario_names <- vapply(x$scenarios, `[[`, character(1), "name")
    if (anyDuplicated(scenario_names)) {
      cli::cli_abort(c(
        "Duplicate scenario name{?s}: {.val {unique(scenario_names[duplicated(scenario_names)])}}.",
        "i" = "Scenario names must be unique -- results are stored by name, so duplicates would silently overwrite each other."
      ))
    }
  }

  if (length(x$methods) > 0) {
    method_names <- vapply(x$methods, `[[`, character(1), "name")
    if (anyDuplicated(method_names)) {
      cli::cli_abort(c(
        "Duplicate method name{?s}: {.val {unique(method_names[duplicated(method_names)])}}.",
        "i" = "Method names must be unique -- results are stored by name, so duplicates would silently overwrite each other."
      ))
    }
  }

  invisible(x)
}

#' Define a simulation design
#'
#' `sim_design()` creates the container for a simulation study. Scenarios,
#' methods, and configuration (parallelism, storage) are added with `+`.
#'
#' @param replicates Number of Monte Carlo replicates per (scenario, method)
#'   pair.
#' @param seed Global random seed; per-replicate seeds are derived
#'   deterministically from it.
#' @param label A short identifier for the design, embedded in the manifest.
#' @param description A longer free-text description of the design.
#'
#' @return A `SimDesign` object.
#' @export
#'
#' @examples
#' sim_design(replicates = 1000, seed = 42, label = "mean_study")
sim_design <- function(replicates, seed, label = NULL, description = NULL) {
  if (!is.numeric(replicates) || length(replicates) != 1 || replicates < 1) {
    cli::cli_abort("{.arg replicates} must be a single positive number.")
  }
  if (!is.numeric(seed) || length(seed) != 1) {
    cli::cli_abort("{.arg seed} must be a single number.")
  }
  if (!is.null(label) && (!is.character(label) || length(label) != 1)) {
    cli::cli_abort("{.arg label} must be a single character string.")
  }
  if (!is.null(description) && (!is.character(description) || length(description) != 1)) {
    cli::cli_abort("{.arg description} must be a single character string.")
  }
  new_SimDesign(replicates, seed, label, description)
}

#' Add a component to a SimDesign
#'
#' Appends a `SimScenario`, `SimScenarioGrid`, `SimMethod`, `SimParallel`, or
#' `SimStorage` to a `SimDesign`, returning a *new* design -- the original is
#' left untouched.
#'
#' @param lhs A `SimDesign`.
#' @param rhs A `SimScenario`, `SimScenarioGrid`, `SimMethod`, `SimParallel`,
#'   or `SimStorage`.
#' @return A new `SimDesign`.
#' @export
`+.SimDesign` <- function(lhs, rhs) {
  if (inherits(rhs, "SimScenarioGrid")) {
    lhs$scenarios <- c(lhs$scenarios, rhs)
  } else if (inherits(rhs, "SimScenario")) {
    lhs$scenarios <- c(lhs$scenarios, list(rhs))
  } else if (inherits(rhs, "SimMethod")) {
    lhs$methods <- c(lhs$methods, list(rhs))
  } else if (inherits(rhs, "SimParallel")) {
    lhs$parallel <- rhs
  } else if (inherits(rhs, "SimStorage")) {
    lhs$storage <- rhs
  } else {
    cli::cli_abort("Cannot add object of class {.cls {class(rhs)}} to a SimDesign.")
  }
  validate_SimDesign(lhs)
  lhs
}

#' @export
print.SimDesign <- function(x, ...) {
  cli::cli_h1("SimDesign")
  cli::cli_bullets(c(
    "*" = "Label:      {x$label %||% '(unnamed)'}",
    "*" = "Replicates: {x$replicates}",
    "*" = "Scenarios:  {length(x$scenarios)}",
    "*" = "Methods:    {length(x$methods)}",
    "*" = "Seed:       {x$seed}"
  ))
  invisible(x)
}

`%||%` <- function(x, y) if (is.null(x)) y else x
