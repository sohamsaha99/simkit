# R/sim_catalog.R -- sim_catalog(), new_SimCatalog(), print.SimCatalog(), sim_load()

new_SimCatalog <- function(df) {
  stopifnot(is.data.frame(df))
  structure(df, class = c("SimCatalog", class(df)))
}

#' Index a root directory of simkit experiments
#'
#' Scans the immediate subdirectories of `path`, treating each as one
#' candidate experiment, and returns a tidy index of every `simkit` result
#' folder found among them. A folder is recognized as a `simkit` result
#' folder if it has a `manifest.json` (a finished run), an
#' `in_progress.json` (a run still in flight, or one whose process died
#' before ever finishing), or on-disk replicate files with neither marker
#' present (a run that started writing results and then lost its
#' in-progress marker, e.g. because it crashed hard or the marker was
#' removed by hand once the process was confirmed dead). Any other
#' subdirectory is silently skipped.
#'
#' @param path Root directory to scan.
#'
#' @return A `SimCatalog`: a tibble subclass with one row per experiment
#'   found, so `dplyr::filter()` and friends work directly on the result.
#'   Columns: `label`, `status` (`"complete"`, `"partial"`, or
#'   `"errored"`), `date`, `scenarios`, `methods`, `completed`, `total`,
#'   `size`, `path`, `description`, `seed`, `r_version`, `simkit_version`,
#'   `design_hash`, `created_at`, `completed_at`.
#'
#' @details
#' `total` (the expected replicate count) is only known once a run has
#' finished and written its `manifest.json` -- per coding-guidelines.md
#' Sec. 6, the manifest is deliberately written only at the end of
#' [sim_run()], so status detection is unambiguous. For `"partial"` and
#' `"errored"` experiments `total` is reported as `NA` rather than guessed;
#' `completed` still reflects the successful replicates actually found on
#' disk.
#'
#' @export
#'
#' @examples
#' root <- tempfile("simkit_catalog_")
#' fs::dir_create(root)
#' design <- sim_design(replicates = 3, seed = 1) +
#'   sim_scenario(
#'     name = "s1", params = list(n = 10, mu = 0),
#'     dgp = function(params) rnorm(params$n, params$mu),
#'     truth = function(params) list(mean = params$mu)
#'   ) +
#'   sim_method(name = "m1", fn = function(data, params) list(est = mean(data))) +
#'   sim_storage(path = fs::path(root, "exp1"))
#' sim_run(design, progress = FALSE)
#' sim_catalog(root)
sim_catalog <- function(path) {
  if (!is.character(path) || length(path) != 1 || is.na(path)) {
    cli::cli_abort("{.arg path} must be a single character string.")
  }
  if (!fs::dir_exists(path)) {
    cli::cli_abort("{.path {path}} does not exist.")
  }

  candidates <- fs::dir_ls(path, type = "directory")
  rows <- lapply(candidates, simkit_catalog_row)
  rows <- rows[!vapply(rows, is.null, logical(1))]

  df <- if (length(rows) == 0) {
    dplyr::tibble(
      label = character(), status = character(), date = as.POSIXct(character()),
      scenarios = numeric(), methods = numeric(),
      completed = numeric(), total = numeric(),
      size = fs::fs_bytes(numeric()), path = character(),
      description = character(), seed = numeric(),
      r_version = character(), simkit_version = character(),
      design_hash = character(), created_at = character(), completed_at = character()
    )
  } else {
    dplyr::bind_rows(rows)
  }

  catalog <- new_SimCatalog(df)
  attr(catalog, "simkit_root") <- path
  catalog
}

#' Parse a manifest/in_progress ISO-8601 UTC timestamp, or `NA` if absent
#' @noRd
simkit_parse_iso8601 <- function(x) {
  if (is.null(x)) {
    return(as.POSIXct(NA_character_, tz = "UTC"))
  }
  as.POSIXct(x, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
}

#' Total on-disk size of an experiment directory, recursively
#' @noRd
simkit_dir_size <- function(dir) {
  files <- fs::dir_ls(dir, recurse = TRUE, type = "file")
  if (length(files) == 0) {
    return(fs::fs_bytes(0))
  }
  sum(fs::file_size(files))
}

#' Count scenario/method pairs and completed replicates found on disk
#'
#' Used when no manifest is available to describe a `"partial"` or
#' `"errored"` experiment -- `scenarios`/`methods` are then a lower bound
#' (only pairs whose directories were actually created so far are counted),
#' since the true totals live only in the manifest [sim_run()] has not yet
#' written.
#'
#' @noRd
simkit_scan_pairs <- function(dir) {
  scenario_dirs <- fs::dir_ls(dir, type = "directory")
  method_names <- character(0)
  completed <- 0L
  has_any_files <- FALSE

  for (sdir in scenario_dirs) {
    method_dirs <- fs::dir_ls(sdir, type = "directory")
    method_names <- union(method_names, fs::path_file(method_dirs))
    for (mdir in method_dirs) {
      completed <- completed + length(simkit_completed_reps(mdir))
      if (length(fs::dir_ls(mdir, regexp = "rep_[0-9]+(_ERROR)?\\.rds$")) > 0) {
        has_any_files <- TRUE
      }
    }
  }

  list(
    scenarios     = length(scenario_dirs),
    methods       = length(method_names),
    completed     = completed,
    has_any_files = has_any_files
  )
}

#' Build one SimCatalog row for a candidate experiment directory, or `NULL`
#' if it isn't a simkit result folder at all
#' @noRd
simkit_catalog_row <- function(dir) {
  manifest <- simkit_read_manifest(dir)
  in_progress <- simkit_read_in_progress(dir)
  pairs <- simkit_scan_pairs(dir)

  if (is.null(manifest) && is.null(in_progress) && !pairs$has_any_files) {
    return(NULL)
  }

  if (!is.null(manifest)) {
    status         <- "complete"
    label          <- manifest$label %||% fs::path_file(dir)
    description    <- manifest$description %||% NA_character_
    date           <- simkit_parse_iso8601(manifest$created_at)
    n_scenarios    <- length(manifest$scenarios)
    n_methods      <- length(manifest$methods)
    total          <- as.numeric(manifest$replicates) * n_scenarios * n_methods
    seed           <- as.numeric(manifest$seed)
    r_version      <- manifest$r_version %||% NA_character_
    simkit_version <- manifest$simkit_version %||% NA_character_
    design_hash    <- manifest$design_hash %||% NA_character_
    created_at     <- manifest$created_at %||% NA_character_
    completed_at   <- manifest$completed_at %||% NA_character_
  } else if (!is.null(in_progress)) {
    status         <- "partial"
    label          <- fs::path_file(dir)
    description    <- NA_character_
    date           <- simkit_parse_iso8601(in_progress$started_at)
    n_scenarios    <- pairs$scenarios
    n_methods      <- pairs$methods
    total          <- NA_real_
    seed           <- NA_real_
    r_version      <- NA_character_
    simkit_version <- NA_character_
    design_hash    <- NA_character_
    created_at     <- in_progress$started_at %||% NA_character_
    completed_at   <- NA_character_
  } else {
    status         <- "errored"
    label          <- fs::path_file(dir)
    description    <- NA_character_
    date           <- simkit_parse_iso8601(NULL)
    n_scenarios    <- pairs$scenarios
    n_methods      <- pairs$methods
    total          <- NA_real_
    seed           <- NA_real_
    r_version      <- NA_character_
    simkit_version <- NA_character_
    design_hash    <- NA_character_
    created_at     <- NA_character_
    completed_at   <- NA_character_
  }

  dplyr::tibble(
    label = label, status = status, date = date,
    scenarios = as.numeric(n_scenarios), methods = as.numeric(n_methods),
    completed = as.numeric(pairs$completed), total = total,
    size = simkit_dir_size(dir), path = as.character(dir),
    description = description, seed = seed,
    r_version = r_version, simkit_version = simkit_version,
    design_hash = design_hash, created_at = created_at, completed_at = completed_at
  )
}

#' Recover a SimDesign and its results from a catalog entry
#'
#' Reconstructs a `SimDesign` from the on-disk `manifest.json` referenced by
#' a single-row `SimCatalog`, and wraps it as a `SimResults` ready for
#' [sim_collect()]. Only the JSON-safe fields `manifest.json` stores --
#' scenario/method names, parameters, and `store` mode -- survive the round
#' trip (design.md Sec 4.4): the original `dgp`/`truth`/`fn` functions
#' cannot be recovered from disk, so the reconstructed scenarios and methods
#' carry placeholder functions that abort loudly if ever invoked. This is
#' enough for [sim_collect()], which only reads files already on disk and
#' never calls back into `dgp`/`truth`/`fn`.
#'
#' @param catalog A `SimCatalog` with exactly one row -- index (`cat[1, ]`)
#'   or `dplyr::filter()` down to a single experiment first.
#'
#' @return A `SimResults` object.
#' @export
#'
#' @examples
#' root <- tempfile("simkit_catalog_")
#' fs::dir_create(root)
#' design <- sim_design(replicates = 3, seed = 1) +
#'   sim_scenario(
#'     name = "s1", params = list(n = 10, mu = 0),
#'     dgp = function(params) rnorm(params$n, params$mu),
#'     truth = function(params) list(mean = params$mu)
#'   ) +
#'   sim_method(name = "m1", fn = function(data, params) list(est = mean(data))) +
#'   sim_storage(path = fs::path(root, "exp1"))
#' sim_run(design, progress = FALSE)
#' sim_catalog(root) |> sim_load() |> sim_collect()
sim_load <- function(catalog) {
  if (!inherits(catalog, "SimCatalog")) {
    cli::cli_abort("{.arg catalog} must be a SimCatalog. See {.fn sim_catalog}.")
  }
  if (nrow(catalog) != 1) {
    cli::cli_abort(c(
      "{.arg catalog} must have exactly one row, but has {nrow(catalog)}.",
      "i" = "Index or {.fn dplyr::filter} down to a single experiment first, e.g. {.code cat[1, ]}."
    ))
  }

  exp_path <- catalog$path[[1]]
  manifest <- simkit_read_manifest(exp_path)
  if (is.null(manifest)) {
    cli::cli_abort(c(
      "No {.file manifest.json} found at {.path {exp_path}}.",
      "i" = paste0(
        "{.fn sim_load} can only reconstruct experiments that finished writing a ",
        "manifest; this entry's status is {.val {catalog$status[[1]]}}."
      )
    ))
  }

  scenarios <- lapply(manifest$scenarios, function(s) {
    new_SimScenario(s$name, s$params, simkit_stub_dgp, simkit_stub_truth)
  })
  methods <- lapply(manifest$methods, function(m) {
    new_SimMethod(m$name, simkit_stub_fn, m$store)
  })

  design <- new_SimDesign(
    replicates  = manifest$replicates,
    seed        = manifest$seed,
    label       = manifest$label,
    description = manifest$description,
    scenarios   = scenarios,
    methods     = methods
  )

  new_SimResults(path = exp_path, design = design)
}

#' Placeholder functions for scenarios/methods reconstructed by [sim_load()]
#'
#' `manifest.json` never stores user functions (design.md Sec 4.4), so a
#' loaded design's `dgp`/`truth`/`fn` are unusable stand-ins. `truth()`
#' degrades gracefully to an empty list -- [sim_collect()] calls it for
#' every scenario when attaching truths, and an empty list is a safe,
#' non-aborting "unknown" value; `dgp()`/`fn()` are only ever needed to
#' generate new replicates, which a loaded design cannot do, so they abort
#' loudly instead of failing silently.
#'
#' @noRd
simkit_stub_dgp <- function(params) {
  cli::cli_abort(paste(
    "The data-generating function for this scenario cannot be recovered from",
    "a SimCatalog entry -- manifest.json stores only scenario parameters,",
    "not functions."
  ))
}

#' @noRd
simkit_stub_truth <- function(params) list()

#' @noRd
simkit_stub_fn <- function(data, params) {
  cli::cli_abort(paste(
    "The estimator function for this method cannot be recovered from a",
    "SimCatalog entry -- manifest.json stores only its name and store mode,",
    "not the function."
  ))
}

#' Format a SimCatalog as an aligned, header-plus-rows text table
#' @noRd
simkit_format_catalog_table <- function(x) {
  status_symbol <- c(
    complete = cli::symbol$tick,
    partial  = cli::symbol$warning,
    errored  = cli::symbol$cross
  )
  status_col <- paste(status_symbol[x$status], x$status)
  progress_col <- paste0(x$completed, "/", ifelse(is.na(x$total), "?", x$total))
  size_col <- vapply(x$size, format, character(1))
  date_col <- ifelse(is.na(x$date), "--", format(x$date, "%Y-%m-%d"))

  cols <- list(
    `#`         = as.character(seq_len(nrow(x))),
    label       = x$label,
    date        = date_col,
    status      = status_col,
    scenarios   = as.character(x$scenarios),
    methods     = as.character(x$methods),
    progress    = progress_col,
    size        = size_col
  )

  widths <- mapply(function(name, values) max(nchar(name), nchar(values)), names(cols), cols)
  pad_row <- function(values) paste(mapply(function(v, w) formatC(v, width = -w), values, widths), collapse = "  ")

  c(pad_row(names(cols)), vapply(seq_len(nrow(x)), function(i) {
    pad_row(lapply(cols, `[[`, i))
  }, character(1)))
}

#' @export
print.SimCatalog <- function(x, full = FALSE, ...) {
  if (isTRUE(full)) {
    if (nrow(x) != 1) {
      cli::cli_abort("{.arg full = TRUE} requires exactly one row, but {.arg x} has {nrow(x)} rows.")
    }
    or_unknown <- function(v, placeholder = "unknown") if (is.na(v)) placeholder else v

    cli::cli_h1("SimCatalog entry: {x$label[[1]]}")
    cli::cli_bullets(c(
      "*" = "Label:       {x$label[[1]]}",
      "*" = "Description: {or_unknown(x$description[[1]], '(none)')}",
      "*" = "Status:      {x$status[[1]]}",
      "*" = "Date:        {if (is.na(x$date[[1]])) 'unknown' else format(x$date[[1]], '%Y-%m-%d %H:%M')}",
      "*" = "R version:   {or_unknown(x$r_version[[1]])} {cli::symbol$dot} simkit: {or_unknown(x$simkit_version[[1]])}",
      "*" = "Seed:        {or_unknown(x$seed[[1]])} {cli::symbol$dot} Replicates: {or_unknown(x$total[[1]])}"
    ))
    return(invisible(x))
  }

  root <- attr(x, "simkit_root")
  cli::cli_rule(paste0("simkit catalog", if (!is.null(root)) paste0(": ", root) else ""))

  if (nrow(x) == 0) {
    cli::cli_text("(no experiments found)")
  } else {
    cli::cli_verbatim(simkit_format_catalog_table(x))
  }
  cli::cli_rule()

  n <- nrow(x)
  n_complete <- sum(x$status == "complete")
  n_partial  <- sum(x$status == "partial")
  n_errored  <- sum(x$status == "errored")
  cli::cli_bullets(c(
    "*" = "{n} experiment{?s} {cli::symbol$dot} {n_complete} complete {cli::symbol$dot} {n_partial} partial {cli::symbol$dot} {n_errored} errored"
  ))

  invisible(x)
}
