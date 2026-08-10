# R/sim_plot.R -- sim_plot(); internal simkit_extract_param()

#' Escape regex metacharacters in a literal string
#' @noRd
simkit_escape_regex <- function(x) {
  gsub("([.^$|?*+()\\[\\]{}\\\\])", "\\\\\\1", x, perl = TRUE)
}

#' Extract a scenario parameter's value for each of a vector of scenario names
#'
#' `sim_plot()` maps scenario parameters onto plot aesthetics, but the
#' `SimEvaluation` tibble it receives only carries `scenario`/`method`
#' names -- not the parameter lists behind them (design.md Sec 4.9). This
#' recovers a parameter's value one of two ways:
#'
#' - If `path` is supplied, `param` is read directly out of that
#'   experiment's `manifest.json`, which stores each scenario's full
#'   `params` list verbatim (design.md Sec 4.4) -- exact regardless of how
#'   the scenario was named.
#' - Otherwise, `param` is parsed out of the scenario name itself, assuming
#'   the `key<value>` convention `sim_scenario_grid()`'s `name_fn` typically
#'   produces (e.g. "n100_s0.5" for `n = 100, sigma = 0.5`, or
#'   "normal_n100" -- `n` extracts, but `dist` does not, since its value
#'   has no `dist` label to key off of).
#'
#' @param scenario_names Character vector of scenario names (may repeat --
#'   one value is resolved per element).
#' @param param Name of the parameter to extract.
#' @param path Optional storage path; if supplied, `param` is read from
#'   that experiment's `manifest.json` instead of parsed from the name.
#'
#' @return A vector the same length as `scenario_names` -- numeric if every
#'   extracted value parses as numeric, character otherwise.
#' @noRd
simkit_extract_param <- function(scenario_names, param, path = NULL) {
  if (!is.null(path)) {
    manifest <- simkit_read_manifest(path)
    if (is.null(manifest)) {
      cli::cli_abort("No {.file manifest.json} found at {.path {path}}.")
    }
    lookup <- stats::setNames(
      lapply(manifest$scenarios, `[[`, "params"),
      vapply(manifest$scenarios, `[[`, character(1), "name")
    )
    values <- lapply(scenario_names, function(nm) {
      params <- lookup[[nm]]
      if (is.null(params) || is.null(params[[param]])) {
        cli::cli_abort(c(
          "Parameter {.val {param}} not found for scenario {.val {nm}} in {.path {simkit_manifest_path(path)}}.",
          "i" = "Available parameters: {.val {names(params)}}."
        ))
      }
      params[[param]]
    })
  } else {
    pattern <- paste0("(^|_)", simkit_escape_regex(param), "([^_]+)")
    values <- lapply(scenario_names, function(nm) {
      m <- regmatches(nm, regexec(pattern, nm))[[1]]
      if (length(m) < 3) {
        cli::cli_abort(c(
          "Could not find parameter {.val {param}} in scenario name {.val {nm}}.",
          "i" = paste(
            "Either encode it in the scenario name (e.g. via {.fn sim_scenario_grid}'s",
            "{.arg name_fn}), or pass {.arg path} to {.fn sim_plot} to read it from",
            "{.file manifest.json} instead."
          )
        ))
      }
      m[[3]]
    })
  }

  values <- unlist(values, use.names = FALSE)
  numeric_values <- suppressWarnings(as.numeric(values))
  if (!anyNA(numeric_values)) numeric_values else values
}

#' Plot evaluation results
#'
#' A `ggplot2`-based convenience layer over [sim_evaluate()] output. One
#' metric is plotted against an x aesthetic, optionally colored and/or
#' faceted -- `x`, `color`, and `facet` may each be `"scenario"`,
#' `"method"`, or a scenario parameter name, in which case its value is
#' recovered from scenario names or `manifest.json` (see `path`).
#'
#' @param evaluation A long-format tibble as returned by [sim_evaluate()],
#'   with columns `scenario`, `method`, `metric`, `value`.
#' @param metric Name of the metric to plot; must be present in
#'   `evaluation$metric`.
#' @param x Aesthetic for the x-axis: `"scenario"`, `"method"`, or a
#'   scenario parameter name.
#' @param color Optional aesthetic for point/line color, same rules as `x`.
#' @param facet Optional variable to facet by (via
#'   [ggplot2::facet_wrap()]), same rules as `x`.
#' @param path Optional storage path; if supplied, scenario parameters
#'   requested via `x`/`color`/`facet` are read from that experiment's
#'   `manifest.json` rather than parsed from scenario names.
#'
#' @return A `ggplot` object. Additional layers (`+ scale_y_log10()`, etc.)
#'   can be appended directly.
#' @export
#' @importFrom rlang .data
#'
#' @examples
#' design <- sim_design(replicates = 20, seed = 1) +
#'   sim_scenario_grid(
#'     params  = list(n = c(30, 100)),
#'     name_fn = function(p) paste0("n", p$n),
#'     dgp     = function(p) rnorm(p$n),
#'     truth   = function(p) list(mean = 0)
#'   ) +
#'   sim_method("m1", function(data, params) list(est = mean(data))) +
#'   sim_storage(path = tempfile("simkit_"))
#' evaluation <- sim_run(design, progress = FALSE) |>
#'   sim_evaluate(bias = function(sims, truth) mean(sims$est) - truth$mean)
#' sim_plot(evaluation, metric = "bias", x = "n", color = "method")
sim_plot <- function(evaluation, metric, x, color = NULL, facet = NULL, path = NULL) {
  rlang::check_installed("ggplot2", reason = "to use `sim_plot()`.")

  required <- c("scenario", "method", "metric", "value")
  if (!is.data.frame(evaluation) || !all(required %in% names(evaluation))) {
    cli::cli_abort(
      "{.arg evaluation} must be the tibble returned by {.fn sim_evaluate}, with columns {.val scenario}, {.val method}, {.val metric}, and {.val value}."
    )
  }
  if (!is.character(metric) || length(metric) != 1) {
    cli::cli_abort("{.arg metric} must be a single character string.")
  }

  available <- unique(evaluation$metric)
  if (!metric %in% available) {
    cli::cli_abort(c(
      "Metric {.val {metric}} was not found in {.arg evaluation}.",
      "i" = "Available metric{?s}: {.val {available}}."
    ))
  }

  df <- evaluation[evaluation$metric == metric, , drop = FALSE]

  resolve <- function(aes_name) {
    if (is.null(aes_name)) {
      return(NULL)
    }
    if (aes_name %in% names(df)) {
      df[[aes_name]]
    } else {
      simkit_extract_param(df$scenario, aes_name, path = path)
    }
  }

  plot_data <- data.frame(value = df$value, x = resolve(x), stringsAsFactors = FALSE)
  if (!is.null(color)) plot_data$color <- resolve(color)
  if (!is.null(facet)) plot_data$facet <- resolve(facet)

  mapping <- ggplot2::aes(x = .data$x, y = .data$value)
  if (!is.null(color)) {
    mapping <- utils::modifyList(mapping, ggplot2::aes(colour = .data$color, group = .data$color))
  }

  p <- ggplot2::ggplot(plot_data, mapping) +
    ggplot2::geom_line() +
    ggplot2::geom_point() +
    ggplot2::labs(x = x, y = metric, colour = color)

  if (!is.null(facet)) {
    p <- p + ggplot2::facet_wrap(ggplot2::vars(.data$facet))
  }

  p
}
