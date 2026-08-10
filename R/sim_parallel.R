# R/sim_parallel.R — sim_parallel(), new_SimParallel(), seed management

new_SimParallel <- function(workers, backend, chunk_size) {
  stopifnot(is.numeric(workers), length(workers) == 1)
  stopifnot(is.character(backend), length(backend) == 1)
  stopifnot(is.numeric(chunk_size), length(chunk_size) == 1)
  structure(
    list(workers = workers, backend = backend, chunk_size = chunk_size),
    class = "SimParallel"
  )
}

#' Configure parallel execution for a simulation design
#'
#' Added to a [sim_design()] with `+`. If omitted, `simkit` runs
#' sequentially.
#'
#' @param workers Number of parallel workers.
#' @param backend One of `"future"` (default), `"parallel"`, or `"sequential"`.
#' @param chunk_size Number of replicates dispatched per worker call; raise
#'   for very fast methods to reduce dispatch overhead.
#'
#' @return A `SimParallel` object.
#' @export
#'
#' @examples
#' sim_parallel(workers = 4, backend = "future", chunk_size = 1)
sim_parallel <- function(workers = 1, backend = "future", chunk_size = 1) {
  if (!is.numeric(workers) || length(workers) != 1 || workers < 1) {
    cli::cli_abort("{.arg workers} must be a single positive number.")
  }
  if (!is.character(backend) || length(backend) != 1 ||
      !backend %in% c("future", "parallel", "sequential")) {
    cli::cli_abort(
      "{.arg backend} must be one of {.val future}, {.val parallel}, or {.val sequential}."
    )
  }
  if (!is.numeric(chunk_size) || length(chunk_size) != 1 || chunk_size < 1) {
    cli::cli_abort("{.arg chunk_size} must be a single positive number.")
  }
  new_SimParallel(workers, backend, chunk_size)
}

#' @export
print.SimParallel <- function(x, ...) {
  cli::cli_h1("SimParallel")
  cli::cli_bullets(c(
    "*" = "Backend:    {x$backend}",
    "*" = "Workers:    {x$workers}",
    "*" = "Chunk size: {x$chunk_size}"
  ))
  invisible(x)
}

# ---- Seed management --------------------------------------------------------

#' Derive `n_reps` independent L'Ecuyer-CMRG RNG streams from a global seed
#'
#' Naive parallel seeding (`global_seed + rep_index`) produces correlated
#' streams across replicates. L'Ecuyer-CMRG's `parallel::nextRNGStream()`
#' instead advances a well-separated stream for each replicate, so results
#' are identical regardless of which worker executes which replicate or in
#' what order -- the property `sim_run()` relies on for parallel results to
#' match sequential ones under the same design seed.
#'
#' The caller's `.Random.seed` and `RNGkind()` are saved and restored on
#' exit; seeds are threaded through the loop as a local variable rather than
#' mutating global state with `<<-` (coding-guidelines sec 5).
#'
#' @param global_seed A single number.
#' @param n_reps Number of seeds to generate.
#' @return A list of length `n_reps`, each element a `.Random.seed`-compatible
#'   integer vector.
#' @noRd
simkit_make_seeds <- function(global_seed, n_reps) {
  old_kind <- RNGkind()[1]
  old_seed <- if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
    get(".Random.seed", envir = .GlobalEnv)
  } else {
    NULL
  }
  on.exit({
    RNGkind(old_kind)
    if (!is.null(old_seed)) {
      assign(".Random.seed", old_seed, envir = .GlobalEnv)
    } else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
      rm(".Random.seed", envir = .GlobalEnv)
    }
  })

  RNGkind("L'Ecuyer-CMRG")
  set.seed(global_seed)
  seed <- .Random.seed
  seeds <- vector("list", n_reps)
  for (i in seq_len(n_reps)) {
    seeds[[i]] <- seed
    seed <- parallel::nextRNGStream(seed)
  }
  seeds
}

#' Warn when a user-supplied function's captured globals are expensive to ship
#'
#' `object.size(environment(fn))` does not recurse into an environment's
#' bindings -- it reports a small constant regardless of what the
#' environment holds -- so it cannot detect a large capture. This instead
#' sizes exactly the free variables `future` would identify and serialize to
#' a worker, via the same globals-scanning `future` itself uses internally.
#' That also avoids false positives from unrelated objects that merely share
#' `fn`'s enclosing environment (e.g. other large objects sitting in the
#' caller's global environment that `fn` never actually references).
#'
#' @param fn A function (typically a scenario's `dgp` or a method's `fn`).
#' @param arg_name Name to use in the warning message.
#' @noRd
simkit_check_closure <- function(fn, arg_name) {
  globals <- future::getGlobalsAndPackages(fn, envir = environment(fn))$globals
  total_size <- sum(vapply(globals, function(g) as.numeric(utils::object.size(g)), numeric(1)))
  if (total_size > 50 * 1024^2) {
    size_obj <- structure(total_size, class = "object_size")
    cli::cli_warn(c(
      "!" = "{.arg {arg_name}} captures a large environment ({format(size_obj, units = 'MB')}).",
      "i" = "This will be serialized to every parallel worker.",
      "i" = "Consider wrapping in a clean function or using {.fn local}."
    ))
  }
}
