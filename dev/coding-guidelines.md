# `simkit` — Coding Guidelines

---

## 1. Project Setup

### Initialize correctly, once, before any logic

```r
usethis::create_package("simkit")
usethis::use_mit_license()
usethis::use_readme_rmd()
usethis::use_news_md()
usethis::use_testthat()
usethis::use_github_actions()   # R CMD CHECK on push
usethis::use_pkgdown()          # documentation site
```

Commit this skeleton to GitHub before writing a single function. CI failures caught against an empty package are free; CI failures caught six weeks in are expensive.

### `DESCRIPTION` hygiene

- List every package used in code under `Imports:`, not `Depends:`.
- Optional backends (e.g. `future.batchtools`, or an alternative storage
  backend, if one is added later) go under `Suggests:`. `qs`/`arrow` as
  storage backends are deferred for now (dev/design.md Sec. 10) -- `simkit`
  currently has no optional storage-backend dependency.
- Never put `dplyr` or `ggplot2` in `Depends:` — that pollutes the user's namespace.
- Pin minimum versions: `future (>= 1.30.0)`, not just `future`.

```
Imports:
    cli (>= 3.4.0),
    digest (>= 0.6.30),
    fs (>= 1.5.0),
    future (>= 1.30.0),
    future.apply (>= 1.10.0),
    jsonlite (>= 1.8.0),
    progressr (>= 0.13.0),
    rlang (>= 1.1.0),
    dplyr (>= 1.1.0),
    tidyr (>= 1.3.0)
Suggests:
    ggplot2 (>= 3.4.0),
    testthat (>= 3.0.0),
    future.batchtools
```

---

## 2. File and Directory Layout

Mirror the object model in the file structure. One primary class or closely related group of functions per file.

```
R/
  sim_design.R          # sim_design(), +.SimDesign, print.SimDesign
  sim_scenario.R        # sim_scenario(), sim_scenario_grid()
  sim_method.R          # sim_method(), sim_bootstrap()
  sim_check.R           # sim_check(), print.SimCheck
  sim_run.R             # sim_run() and internal dispatch helpers
  sim_storage.R         # sim_storage(), read/write helpers, atomic write
  sim_collect.R         # sim_collect()
  sim_evaluate.R        # sim_evaluate(), sim_summarize()
  sim_catalog.R         # sim_catalog(), sim_load(), print.SimCatalog
  sim_plot.R            # sim_plot()
  sim_parallel.R        # sim_parallel(), seed management
  utils.R               # small internal helpers shared across files
  zzz.R                 # .onLoad(), .onAttach() if needed

tests/
  testthat/
    test-classes.R
    test-check.R
    test-run-sequential.R
    test-run-parallel.R
    test-storage.R
    test-evaluate.R
    test-catalog.R
    helper-fixtures.R   # shared test DGPs, methods, designs
```

Prefix every internal (non-exported) function with a dot or `simkit_`: e.g. `simkit_write_rep()`, `.derive_seed()`. This makes the public API immediately obvious when reading source.

---

## 3. S3 Class Design

### Constructor pattern

Every class has a dedicated low-level constructor prefixed with `new_` and a user-facing constructor without the prefix. The `new_` constructor is strict — it validates types and structure. The user-facing function provides defaults and friendly errors.

```r
# Internal: fast, strict, no defaults
new_SimScenario <- function(name, params, dgp, truth) {
  stopifnot(is.character(name), length(name) == 1)
  stopifnot(is.list(params))
  stopifnot(is.function(dgp), is.function(truth))
  structure(
    list(name = name, params = params, dgp = dgp, truth = truth),
    class = "SimScenario"
  )
}

# User-facing: validates with informative errors, applies defaults
sim_scenario <- function(name, params, dgp, truth) {
  if (!is.character(name) || length(name) != 1)
    cli::cli_abort("{.arg name} must be a single character string.")
  if (!is.function(dgp))
    cli::cli_abort("{.arg dgp} must be a function.")
  new_SimScenario(name, params, dgp, truth)
}
```

### Validator functions

For complex objects, write a separate `validate_*` function so validation can be called independently (useful in tests):

```r
validate_SimDesign <- function(x) {
  if (length(x$scenarios) == 0)
    cli::cli_abort("Design has no scenarios. Add one with {.fn sim_scenario}.")
  if (length(x$methods) == 0)
    cli::cli_abort("Design has no methods. Add one with {.fn sim_method}.")

  scenario_names <- vapply(x$scenarios, `[[`, character(1), "name")
  if (anyDuplicated(scenario_names))
    cli::cli_abort(c(
      "Duplicate scenario name{?s}: {.val {unique(scenario_names[duplicated(scenario_names)])}}.",
      "i" = "Scenario names must be unique — results are stored by name, so duplicates would silently overwrite each other."
    ))

  method_names <- vapply(x$methods, `[[`, character(1), "name")
  if (anyDuplicated(method_names))
    cli::cli_abort(c(
      "Duplicate method name{?s}: {.val {unique(method_names[duplicated(method_names)])}}.",
      "i" = "Method names must be unique — results are stored by name, so duplicates would silently overwrite each other."
    ))

  invisible(x)
}
```

Call `validate_SimDesign()` from `+.SimDesign` after every append, not just once at the end — a collision is then caught immediately at the call site that introduced it, not later when `sim_run()` tries to write results.

### Print methods

Write `print.*` methods early. Polished output surfaces structural bugs immediately and signals to users that the package is well-made.

```r
print.SimDesign <- function(x, ...) {
  cli::cli_h1("SimDesign")
  cli::cli_bullets(c(
    "*" = "Label:      {x$label}",
    "*" = "Replicates: {x$replicates}",
    "*" = "Scenarios:  {length(x$scenarios)}",
    "*" = "Methods:    {length(x$methods)}",
    "*" = "Seed:       {x$seed}"
  ))
  invisible(x)
}
```

Always return `invisible(x)` from print methods.

---

## 4. Functional Design: Immutability and the `+` Operator

### Never mutate; always return a modified copy

The `+` operator must return a *new* `SimDesign`, leaving the original untouched. This is not optional — it allows researchers to branch designs from a common base:

```r
base <- sim_design(replicates = 1000, seed = 42) +
  sim_scenario_grid(...)

# These two designs are independent
design_a <- base + sim_parallel(workers = 4)
design_b <- base + sim_parallel(workers = 8)
```

Implementation using `modifyList`:

```r
`+.SimDesign` <- function(lhs, rhs) {
  if (inherits(rhs, "SimScenario")) {
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
  lhs   # lhs was copied on entry by R's copy-on-modify semantics
}
```

### Avoid global state

Do not use `options()`, package-level mutable variables, or `<<-` for simulation state. All state should live in the objects being passed through the pipeline. This makes behaviour predictable, testable, and parallelism-safe.

---

## 5. Parallel Computing

### Test with `plan(multisession)` first, not `plan(sequential)`

Sequential execution hides the most common parallel bugs: unresolved symbols, missing package loads on workers, and captured environments. Always run your test suite against `multisession` before releasing a new feature.

### The closure trap

User-supplied functions (DGP, method `fn`) are closures. If they capture large objects in their enclosing environment, `future` serializes the entire environment and ships it to each worker — silently, and very slowly.

Document this clearly, and add a runtime check that warns when a user function's `environment()` contains objects above a size threshold:

```r
simkit_check_closure <- function(fn, arg_name) {
  env_size <- utils::object.size(environment(fn))
  if (env_size > 50 * 1024^2) {  # 50 MB
    cli::cli_warn(c(
      "!" = "{.arg {arg_name}} captures a large environment ({format(env_size, units='MB')}).",
      "i" = "This will be serialized to every parallel worker.",
      "i" = "Consider wrapping in a clean function or using {.fn local}."
    ))
  }
}
```

### Package references inside parallel functions

Workers in a `multisession` plan load packages fresh. User code that relies on a function from `simkit` or another package must use the `::` operator or ensure the package is loaded via a `future` global hook. Document that method functions should use explicit namespacing for any non-base function they call.

### Seed management

Use L'Ecuyer-CMRG streams for reproducible parallel RNG. Do not derive seeds as `global_seed + rep_index` — this produces correlated streams.

This is the one place a naive implementation is tempted to reach for `<<-` (mutating `.Random.seed` across loop iterations). Don't — it's exactly the global-mutable-state pattern ruled out above, and it also leaks a changed RNG state into the caller's session. Thread the seed through the loop as a local variable instead, and restore the caller's prior RNG state on exit:

```r
simkit_make_seeds <- function(global_seed, n_reps) {
  old_kind <- RNGkind()[1]
  old_seed <- if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
    get(".Random.seed", envir = .GlobalEnv)
  } else {
    NULL
  }
  on.exit({
    RNGkind(old_kind)
    if (!is.null(old_seed)) assign(".Random.seed", old_seed, envir = .GlobalEnv)
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
```

Each worker receives its own seed and calls `assign(".Random.seed", seed, envir = .GlobalEnv)` at the start of its replicate — safe inside a worker process, since each `future` worker has its own R session and global environment. That's different from mutating the *orchestrating* session's RNG state, which is what the rewrite above avoids.

### Nested parallelism (bootstrap)

`sim_bootstrap(parallel = TRUE)` (Session 7) runs a second `future` plan nested inside the outer replicate-level plan set up by `sim_parallel()`. Nested `future` plans do not automatically divide cores between levels — if both levels default to "use all cores," total worker processes multiply (`outer_workers × inner_workers`), oversubscribing the machine.

Before dispatching bootstrap work, check the product against `parallel::detectCores()` and warn:

```r
simkit_check_oversubscription <- function(outer_workers, inner_workers) {
  total <- outer_workers * inner_workers
  cores <- parallel::detectCores()
  if (total > cores) {
    cli::cli_warn(c(
      "!" = "Nested parallelism requests {total} worker processes ({outer_workers} outer x {inner_workers} inner), but only {cores} cores are available.",
      "i" = "Consider a nested {.fn future::plan} topology via {.fn future::tweak}, or set {.arg parallel = FALSE} in {.fn sim_bootstrap}."
    ))
  }
}
```

Do not attempt to auto-tune worker counts on the user's behalf — just warn. Silent auto-tuning would make performance unpredictable across machines.

---

## 6. File I/O and Fault Tolerance

### Atomic writes

Never write directly to the final path. A crash mid-write produces a corrupt file that looks complete. Use a `.tmp` extension and rename:

```r
simkit_write_rep <- function(result, path) {
  tmp <- paste0(path, ".tmp")
  saveRDS(result, tmp)
  file.rename(tmp, path)   # atomic on POSIX; best-effort on Windows
}
```

The resume logic in `sim_run()` must explicitly ignore `.tmp` files:

```r
simkit_completed_reps <- function(dir) {
  files <- fs::dir_ls(dir, regexp = "rep_\\d+\\.rds$")  # excludes .tmp
  names <- fs::path_file(files)
  as.integer(regmatches(names, regexpr("\\d+", names)))
}
```

### Manifest writes

Write the manifest at the *end* of `sim_run()`, not the beginning. An in-progress run has no manifest; a complete run does. This makes status detection unambiguous in `sim_catalog()`.

For long runs, also write an `in_progress.json` at the start (with start time, PID, hostname) and delete it when the manifest is written. `sim_catalog()` uses its presence to distinguish "partial" from "never started".

### Directory naming collisions

If a user runs `sim_run()` twice with the same storage path but a modified design, warn loudly rather than silently overwriting:

```r
simkit_check_storage_conflict <- function(path, design) {
  manifest_path <- fs::path(path, "manifest.json")
  if (fs::file_exists(manifest_path)) {
    existing <- jsonlite::read_json(manifest_path)
    if (existing$design_hash != simkit_design_hash(design)) {
      cli::cli_abort(c(
        "Storage path {.path {path}} contains results from a different design.",
        "i" = "Use a new path, or set {.arg resume = FALSE} to overwrite."
      ))
    }
  }
}
```

`simkit_design_hash()` hashes the design's scenarios, methods, and `replicates` — but not `label` or `description`, which are cosmetic. Hash the *deparsed source* of each function, not the closure object itself: raw closures carry captured environments and (with `keep.source`) source references that can change the hash for reasons unrelated to behavior, e.g. a different session's environment pointer. `deparse()`'s default `control` omits `"useSource"`, so it always deparses from the parsed body rather than original source text — a comment- or whitespace-only edit upstream doesn't change the hash.

```r
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
```

---

## 7. Error Handling

### Use conditions, not bare `stop()`

`cli::cli_abort()` produces structured conditions with better formatting. For domain-specific errors, define custom condition classes so users can handle them programmatically:

```r
simkit_rep_error <- function(message, scenario, method, rep, call = rlang::caller_env()) {
  structure(
    class = c("simkit_rep_error", "error", "condition"),
    list(message = message, scenario = scenario, method = method, rep = rep, call = call)
  )
}
```

### The `on_error` contract

The three `on_error` modes must be consistent and tested:

- `"warn"`: catch error, write sentinel file, emit `cli::cli_warn()`, continue.
- `"skip"`: catch error, write sentinel file, continue silently.
- `"stop"`: re-throw the error, terminating the run.

Wrap replicate execution in a single `tryCatch` at the dispatcher level, not inside user functions. User functions should never need to know about `on_error`.

### Metric errors in `sim_evaluate()`

This is a different contract from replicate errors above: metric-function errors are *always* caught and degraded to `NA`, with a single `cli::cli_warn()` per failing (metric, scenario, method) triple — there is no `on_error`-style argument for `sim_evaluate()`. Evaluation has no per-replicate checkpoint to protect and is cheap to rerun, so there's no reason to offer a `"stop"` mode; always failing soft keeps one bad metric from aborting an otherwise-successful evaluation.

---

## 8. Testing

### Shared fixtures in `helper-fixtures.R`

Define canonical test objects once, reuse everywhere. `testthat` loads all `helper-*.R` files automatically.

```r
# tests/testthat/helper-fixtures.R

simple_dgp <- function(params) rnorm(params$n, mean = params$mu, sd = 1)
simple_truth <- function(params) list(mean = params$mu)
simple_method <- function(data, params) list(est = mean(data))

base_design <- function(replicates = 50, seed = 1) {
  sim_design(replicates = replicates, seed = seed) +
    sim_scenario(
      name = "s1", params = list(n = 30, mu = 0),
      dgp = simple_dgp, truth = simple_truth
    ) +
    sim_method(name = "m1", fn = simple_method)
}
```

### The known-answer test

This is the most important test in the suite. Assert statistical correctness:

```r
test_that("bias is near zero and MSE matches 1/n for sample mean", {
  design <- base_design(replicates = 2000) +
    sim_storage(path = withr::local_tempdir(), resume = FALSE)
  res <- design |> sim_run() |> sim_collect()
  eval <- sim_evaluate(res,
    bias = function(s, t) mean(s$est) - t$mean,
    mse  = function(s, t) mean((s$est - t$mean)^2)
  )
  summ <- sim_summarize(eval)

  expect_lt(abs(summ$bias[summ$method == "m1"]), 0.05)
  expect_lt(abs(summ$mse[summ$method == "m1"] - 1/30), 0.01)
})
```

### The fault tolerance test

```r
test_that("on_error = 'warn' stores sentinel and continues", {
  erroring_method <- sim_method(
    name = "bad",
    fn   = function(data, params) {
      if (params$rep_index == 3) stop("deliberate failure")   # see note below
      list(est = mean(data))
    }
  )
  # ... run 5 replicates, assert 4 succeed and 1 has error != NA
})
```

> **Note:** `rep_index` is an example of metadata `simkit` can inject into the method call. Decide during implementation whether to expose this.

### Parallel tests

Run a reduced version of the known-answer test under `plan(multisession, workers = 2)` and assert results match the sequential run exactly (same seed → same results):

```r
test_that("parallel results match sequential", {
  dir <- withr::local_tempdir()
  res_seq <- base_design() +
    sim_storage(path = fs::path(dir, "seq")) |>
    sim_run()
  res_par <- base_design() +
    sim_parallel(workers = 2) +
    sim_storage(path = fs::path(dir, "par")) |>
    sim_run()

  seq_vals <- sim_collect(res_seq) |> dplyr::arrange(rep) |> dplyr::pull(est)
  par_vals <- sim_collect(res_par) |> dplyr::arrange(rep) |> dplyr::pull(est)
  expect_equal(seq_vals, par_vals)
})
```

### Snapshot tests for print methods

```r
test_that("print.SimDesign renders correctly", {
  expect_snapshot(print(base_design()))
})
```

Run `testthat::snapshot_review()` after intentional formatting changes to update the snapshots deliberately.

### Keep tests fast

- Use `replicates = 20–50` in tests, never 1000.
- Use `withr::local_tempdir()` for all storage paths — it cleans up automatically.
- Tag slow tests with `testthat::skip_on_cran()` and `testthat::skip_on_ci()` where appropriate.

---

## 9. Documentation

### Roxygen2 conventions

Every exported function needs `@param`, `@return`, `@examples`, and `@export`. Group related functions on a shared doc page using `@rdname`:

```r
#' @rdname sim_scenario
#' @export
sim_scenario_grid <- function(...) { ... }
```

### Examples must be self-contained and fast

Examples are run by `R CMD CHECK`. They must complete in under a few seconds. Use `replicates = 5` and `plan(sequential)` in all examples. Wrap anything slower in `\dontrun{}`.

### Write a Getting Started vignette early

A vignette using the worked example from the design doc serves as both documentation and an integration test. Build it as part of CI via `pkgdown`.

---

## 10. Naming Conventions

| Thing | Convention | Example |
|---|---|---|
| Exported functions | `snake_case`, `sim_` prefix | `sim_run`, `sim_check` |
| Internal functions | `simkit_` prefix | `simkit_write_rep` |
| S3 classes | `PascalCase` | `SimDesign`, `SimCheck` |
| S3 methods | `method.Class` | `print.SimDesign` |
| Low-level constructors | `new_` prefix | `new_SimScenario` |
| Validators | `validate_` prefix | `validate_SimDesign` |
| Test fixtures | `base_` or descriptive | `base_design()` |
| Constants | `SCREAMING_SNAKE` | `SIMKIT_SENTINEL_EXT` |

---

## 11. Versioning and Changelog

Follow [Semantic Versioning](https://semver.org): `MAJOR.MINOR.PATCH`.

- `PATCH`: bug fixes, no API changes.
- `MINOR`: new features, fully backward-compatible.
- `MAJOR`: breaking API changes (avoid until 1.0.0).

Maintain `NEWS.md` as a human-readable changelog, updated with every PR. Use the format:

```markdown
# simkit 0.3.0

## New features
- `sim_catalog()` scans a root directory and returns a tidy index of all experiments (#42).
- `sim_check()` performs pre-flight validation before a full run (#38).

## Bug fixes
- `sim_collect()` no longer fails silently when a scenario directory is missing (#45).
```
