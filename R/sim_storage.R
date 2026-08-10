# R/sim_storage.R — sim_storage(), new_SimStorage()
#
# File I/O helpers (simkit_write_rep(), simkit_completed_reps(),
# simkit_check_storage_conflict(), manifest read/write, simkit_design_hash())
# are added in Session 2. This file only defines the configuration object.

new_SimStorage <- function(path, format, resume) {
  stopifnot(is.character(path), length(path) == 1)
  stopifnot(is.character(format), length(format) == 1)
  stopifnot(is.logical(resume), length(resume) == 1)
  structure(
    list(path = path, format = format, resume = resume),
    class = "SimStorage"
  )
}

#' Configure result storage for a simulation design
#'
#' Added to a [sim_design()] with `+`. If omitted, `simkit` uses a temporary
#' directory.
#'
#' @param path Directory in which results are stored; created if it doesn't
#'   exist.
#' @param format Storage format for replicate files. Only `"rds"` is
#'   currently supported; alternative backends (`qs`, `arrow`) are deferred
#'   -- see `dev/session-plan.md` Session 10.
#' @param resume If `TRUE` (default), replicates already on disk are
#'   skipped on the next `sim_run()`.
#'
#' @return A `SimStorage` object.
#' @export
#'
#' @examples
#' sim_storage(path = tempfile(), format = "rds", resume = TRUE)
sim_storage <- function(path, format = "rds", resume = TRUE) {
  if (!is.character(path) || length(path) != 1 || is.na(path)) {
    cli::cli_abort("{.arg path} must be a single character string.")
  }
  if (!is.character(format) || length(format) != 1 || !format %in% c("rds")) {
    cli::cli_abort("{.arg format} must be {.val rds} -- no other backends are supported yet.")
  }
  if (!is.logical(resume) || length(resume) != 1 || is.na(resume)) {
    cli::cli_abort("{.arg resume} must be a single {.code TRUE}/{.code FALSE}.")
  }
  new_SimStorage(path, format, resume)
}

#' @export
print.SimStorage <- function(x, ...) {
  cli::cli_h1("SimStorage")
  cli::cli_bullets(c(
    "*" = "Path:   {x$path}",
    "*" = "Format: {x$format}",
    "*" = "Resume: {x$resume}"
  ))
  invisible(x)
}

# ---- File I/O helpers (Session 2) ------------------------------------------

#' Atomically write a single replicate result to disk
#'
#' Writes to a `.tmp` sibling file first, then renames into place, so a
#' crash mid-write never leaves a corrupt file at `path`.
#'
#' @noRd
simkit_write_rep <- function(result, path) {
  tmp <- paste0(path, ".tmp")
  saveRDS(result, tmp)
  file.rename(tmp, path)
}

#' List the replicate indices already completed in a (scenario, method) dir
#'
#' Only matches `rep_XXXX.rds` -- `.tmp` files and `_ERROR.rds` sentinels
#' are excluded, so a previously-failed replicate is retried on resume.
#'
#' @noRd
simkit_completed_reps <- function(dir) {
  if (!fs::dir_exists(dir)) {
    return(integer(0))
  }
  files <- fs::dir_ls(dir, regexp = "rep_[0-9]+\\.rds$")
  if (length(files) == 0) {
    return(integer(0))
  }
  file_names <- fs::path_file(files)
  as.integer(regmatches(file_names, regexpr("[0-9]+", file_names)))
}

# ---- Manifest and in-progress marker ---------------------------------------

simkit_manifest_path <- function(path) fs::path(path, "manifest.json")
simkit_in_progress_path <- function(path) fs::path(path, "in_progress.json")

#' Mark a storage directory as having an in-progress run
#'
#' Written at the start of [sim_run()] and removed once the manifest is
#' written, so a later catalog function (`sim_catalog()`, a later session)
#' can distinguish "partial" from "never started".
#'
#' @noRd
simkit_write_in_progress <- function(path, started_at) {
  info <- list(
    pid        = Sys.getpid(),
    hostname   = Sys.info()[["nodename"]],
    started_at = started_at
  )
  jsonlite::write_json(info, simkit_in_progress_path(path), auto_unbox = TRUE)
}

#' Remove the in-progress marker once a run completes
#' @noRd
simkit_remove_in_progress <- function(path) {
  marker <- simkit_in_progress_path(path)
  if (fs::file_exists(marker)) {
    fs::file_delete(marker)
  }
}

#' Read in_progress.json for a simulation run, or `NULL` if absent
#'
#' Used by [sim_catalog()] to detect runs that are active (or died without
#' ever writing a `manifest.json`).
#'
#' @noRd
simkit_read_in_progress <- function(path) {
  marker <- simkit_in_progress_path(path)
  if (!fs::file_exists(marker)) {
    return(NULL)
  }
  jsonlite::read_json(marker, simplifyVector = FALSE)
}

#' Write manifest.json for a completed simulation run
#'
#' Schema matches design.md sec 4.4. Written at the *end* of [sim_run()],
#' not the beginning -- an in-progress run has no manifest, a complete run
#' does.
#'
#' @noRd
simkit_write_manifest <- function(path, design, created_at, completed_at) {
  manifest <- list(
    simkit_version = as.character(utils::packageVersion("simkit")),
    r_version       = paste(R.version$major, R.version$minor, sep = "."),
    label           = design$label,
    description     = design$description,
    created_at      = created_at,
    completed_at    = completed_at,
    seed            = design$seed,
    replicates      = design$replicates,
    design_hash     = simkit_design_hash(design),
    scenarios       = lapply(design$scenarios, function(s) list(name = s$name, params = s$params)),
    methods         = lapply(design$methods, function(m) list(name = m$name, store = m$store)),
    storage         = list(
      format = design$storage$format,
      path   = as.character(design$storage$path)
    )
  )
  jsonlite::write_json(manifest, simkit_manifest_path(path), auto_unbox = TRUE, pretty = TRUE)
}

#' Read manifest.json for a simulation run, or `NULL` if absent
#'
#' Normalizes `label`/`description` back to real `NULL` when the design
#' left them unset: `jsonlite::write_json()` serializes an R `NULL` list
#' element as JSON `{}`, and `read_json(simplifyVector = FALSE)`
#' deserializes that back as an empty named list, not `NULL` -- these are
#' the only two manifest fields the schema allows to be genuinely absent
#' (design.md Sec 4.4), so this is the only place that needs correcting.
#'
#' @noRd
simkit_read_manifest <- function(path) {
  manifest_path <- simkit_manifest_path(path)
  if (!fs::file_exists(manifest_path)) {
    return(NULL)
  }
  manifest <- jsonlite::read_json(manifest_path, simplifyVector = FALSE)
  manifest$label <- simkit_manifest_null(manifest$label)
  manifest$description <- simkit_manifest_null(manifest$description)
  manifest
}

#' Treat an empty list the same as `NULL` -- see `simkit_read_manifest()`
#' @noRd
simkit_manifest_null <- function(x) {
  if (is.null(x) || (is.list(x) && length(x) == 0)) NULL else x
}

#' Hash a design's scenarios, methods, and replicate count
#'
#' Hashes the *deparsed source* of each function, not the closure object
#' itself -- closures carry captured environments and source references
#' that can change the hash for reasons unrelated to behavior. `label` and
#' `description` are deliberately excluded; they are cosmetic.
#'
#' @noRd
simkit_design_hash <- function(design) {
  fn_text <- function(fn) paste(deparse(fn), collapse = "\n")

  scenario_repr <- lapply(design$scenarios, function(s) {
    list(name = s$name, params = s$params, dgp = fn_text(s$dgp), truth = fn_text(s$truth))
  })
  method_repr <- lapply(design$methods, function(m) {
    list(name = m$name, fn = fn_text(m$fn), store = m$store)
  })

  digest::digest(list(
    scenarios  = scenario_repr,
    methods    = method_repr,
    replicates = design$replicates
  ))
}

#' Abort if a storage path holds results from a different design
#'
#' Only meaningful when resuming -- `resume = FALSE` is the user's explicit
#' signal that they intend to overwrite, so [sim_run()] skips this check in
#' that case.
#'
#' @noRd
simkit_check_storage_conflict <- function(path, design) {
  manifest_path <- simkit_manifest_path(path)
  if (fs::file_exists(manifest_path)) {
    existing <- jsonlite::read_json(manifest_path)
    if (!identical(existing$design_hash, simkit_design_hash(design))) {
      cli::cli_abort(c(
        "Storage path {.path {path}} contains results from a different design.",
        "i" = "Use a new path, or set {.arg resume = FALSE} to overwrite."
      ))
    }
  }
}
