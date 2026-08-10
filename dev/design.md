# `simkit` — Design Document
### A Tidy Framework for Statistical Simulation Studies

---

## 1. Vision & Motivation

Statistical methodological research follows a well-worn path: design novel estimators, define data-generating scenarios, implement comparator methods, run Monte Carlo simulations, and report performance. Despite this shared structure, every researcher re-implements the same scaffolding from scratch — parallel dispatch, result storage, crash recovery, progress tracking.

`simkit` provides a composable, opinionated framework for this workflow. It handles the infrastructure so researchers can focus on what is actually novel: the estimators, the scenarios, and the metrics.

### Design Philosophy

- **Two vocabularies, each familiar.** Scenarios and methods are assembled with `+` (like `ggplot2` — additive composition). Running and evaluating use `|>` (like `dplyr` — sequential transformation). The grammar maps naturally onto the mental model: you *build* a design, then *pipe it* through execution and analysis.
- **Fail gracefully, resume cheaply.** Every replicate is checkpointed to disk immediately upon completion. A crash loses at most one replicate.
- **Parallelism as a one-liner.** The user declares a parallel backend; `simkit` handles dispatch.
- **Results stay tidy.** Every piece of output is a tidy data frame, ready for `ggplot2` or `dplyr`.

---

## 2. Conceptual Architecture

```
┌─────────────────────────────────────────────────────────────┐
│                        DESIGN PHASE                         │
│                                                             │
│   sim_design()  +  sim_scenario()  +  sim_method()          │
│                 +  sim_parallel()  +  sim_storage()         │
│                                                             │
└────────────────────────┬────────────────────────────────────┘
                         │  |>  sim_check()   ← validate first
                         │  |>  sim_run()
┌────────────────────────▼────────────────────────────────────┐
│                    EXECUTION PHASE                          │
│                                                             │
│   Dispatch replicates → checkpoint → handle errors          │
│   (optional) Bootstrap variance within replicates           │
│                                                             │
└────────────────────────┬────────────────────────────────────┘
                         │  |>  sim_collect()
┌────────────────────────▼────────────────────────────────────┐
│                    EVALUATION PHASE                         │
│                                                             │
│   sim_collect()  →  sim_evaluate()  →  sim_summarize()      │
│                                                             │
└─────────────────────────────────────────────────────────────┘
```

---

## 3. Core Object Model

`simkit` has six primary S3 classes. They are lightweight lists with a class attribute — easy to inspect, print, and extend.

| Class | Created by | Role |
|---|---|---|
| `SimDesign` | `sim_design()` | Container for all simulation settings |
| `SimScenario` | `sim_scenario()` / `sim_scenario_grid()` | A data-generating process + truth |
| `SimMethod` | `sim_method()` | An estimator or comparator |
| `SimCheck` | `sim_check()` | Pre-flight validation report |
| `SimResults` | `sim_run()` | Handle referencing on-disk replicate results (storage path + manifest); pass to `sim_collect()` to materialize a tidy tibble |
| `SimEvaluation` | `sim_evaluate()` | Tidy performance metrics |

The `+` operator is overloaded for `SimDesign` to append scenarios, methods, and configuration.

---

## 4. API Design

### 4.1 Defining a Simulation Design

```r
library(simkit)

design <- sim_design(
  replicates  = 1000,
  seed        = 42,           # global seed; per-replicate seeds derived deterministically
  label       = "mean_study", # short identifier
  description = "Comparing sample mean, trimmed mean, and median under
                 normal and heavy-tailed errors. Varying n = 100, 500."
)
```

`sim_design()` returns a `SimDesign` object. Scenarios, methods, and options are added via `+`.

---

### 4.2 Defining Scenarios

A **scenario** pairs a data-generating process (DGP) with a truth function. The DGP takes a named list `params` and returns a dataset. The truth function takes `params` and returns a named list of estimand values used in metric computation.

```r
design <- design +
  sim_scenario(
    name   = "normal_n100",
    params = list(n = 100, mu = 2, sigma = 1),
    dgp    = function(params) {
      with(params, rnorm(n, mean = mu, sd = sigma))
    },
    truth  = function(params) {
      list(mean = params$mu, variance = params$sigma^2)
    }
  ) +
  sim_scenario(
    name   = "heavy_tail_n100",
    params = list(n = 100, mu = 2, df = 3),
    dgp    = function(params) {
      with(params, mu + rt(n, df = df))
    },
    truth  = function(params) {
      list(mean = params$mu, variance = params$df / (params$df - 2))
    }
  )
```

#### Scenario Grids (Factorial Designs)

Researchers often vary parameters systematically. `sim_scenario_grid()` takes a parameter grid and generates all combinations as named scenarios automatically.

```r
design <- sim_design(replicates = 1000, seed = 42) +
  sim_scenario_grid(
    params = list(
      n     = c(100, 500, 1000),
      mu    = 2,
      sigma = c(0.5, 1.0, 2.0)   # 3 × 3 = 9 scenarios
    ),
    name_fn = function(p) paste0("n", p$n, "_sigma", p$sigma),
    dgp     = function(params) with(params, rnorm(n, mu, sigma)),
    truth   = function(params) list(mean = params$mu, sd = params$sigma)
  )
```

---

### 4.3 Defining Methods

A **method** is a named estimator. Its function receives the dataset and the scenario's `params`, and returns a named list of estimates. Multiple quantities (point estimate, confidence interval bounds, SE) can be returned together.

```r
design <- design +
  sim_method(
    name = "sample_mean",
    fn   = function(data, params) {
      se <- sd(data) / sqrt(length(data))
      list(
        est    = mean(data),
        se     = se,
        ci_lo  = mean(data) - 1.96 * se,
        ci_hi  = mean(data) + 1.96 * se
      )
    }
  ) +
  sim_method(
    name = "trimmed_mean",
    fn   = function(data, params) {
      est <- mean(data, trim = 0.1)
      # Bootstrap SE for trimmed mean (see Section 4.10)
      list(est = est)
    }
  ) +
  sim_method(
    name = "median",
    fn   = function(data, params) {
      list(est = median(data))
    }
  )
```

#### Replicate Metadata

`simkit` injects two internal fields into every `params` list passed to a method's `fn` at call time: `params$.rep_index` (the 1-based replicate number within its scenario/method pair) and `params$.rng_seed` (the seed used for that replicate, for debugging). These are prefixed with `.` to avoid colliding with user-defined parameter names, and are stripped before `params` reaches anywhere else that reads it (e.g. `truth()`, `sim_plot()`'s parameter extraction). Method and DGP functions may read `params$.rep_index`, but should treat it as diagnostic only — it exists so fault-tolerance tests can force a deterministic failure on a specific replicate (see coding-guidelines §8).

---

### 4.4 Configuring Parallelism and Storage

These are added to the design with `+`. If omitted, `simkit` uses sensible defaults (sequential, temporary storage).

```r
design <- design +
  sim_parallel(
    workers    = 4,
    backend    = "future",   # "future" (default), "parallel", or "sequential"
    chunk_size = 1           # replicates dispatched per worker call; raise for very fast methods
  ) +
  sim_storage(
    path     = "./sim_results",   # directory; created if it doesn't exist
    format   = "rds",             # only "rds" is currently supported;
                                   # "qs"/"arrow" backends are deferred (Sec. 10)
    resume   = TRUE               # skip replicates already on disk
  )
```

The storage layout on disk is:

```
sim_results/
  manifest.json              ← design metadata, seed, timestamp, R/simkit versions
  normal_n100/
    sample_mean/
      rep_0001.rds
      rep_0002.rds
      ...
    trimmed_mean/
      rep_0001.rds
      ...
  heavy_tail_n100/
    ...
```

Each `rep_XXXX.rds` stores the named list returned by the method, plus metadata (replicate index, elapsed time, any warnings).

`manifest.json` has the following shape:

```json
{
  "simkit_version": "0.3.0",
  "r_version":      "4.3.2",
  "label":          "mean_estimators_v1",
  "description":    "Comparing sample mean, trimmed mean, and median...",
  "created_at":      "2025-01-15T14:32:00Z",
  "completed_at":    "2025-01-15T16:47:00Z",
  "seed":            42,
  "replicates":      1000,
  "design_hash":    "a1b2c3d4...",
  "scenarios": [
    {"name": "normal_n100",     "params": {"n": 100, "mu": 2, "sigma": 1}},
    {"name": "heavy_tail_n100", "params": {"n": 100, "mu": 2, "df": 3}}
  ],
  "methods": [
    {"name": "sample_mean",  "store": "summary"},
    {"name": "trimmed_mean", "store": "summary"}
  ],
  "storage": {"format": "rds", "path": "./sim_results"}
}
```

`scenarios[].params` and `methods[].store` are what `sim_plot()` and `sim_catalog()` read back — they never need to deserialize user functions, only these JSON-safe fields. While a run is in progress (before `manifest.json` is written), a lightweight `in_progress.json` (`{"pid": ..., "hostname": ..., "started_at": ...}`) marks the directory as active; `sim_catalog()` uses its presence, combined with the absence of `manifest.json`, to report `"partial"` rather than `"never started"`.

**Duplicate names are a hard error, not a warning.** Because results are written to `{path}/{scenario}/{method}/rep_XXXX.rds`, two scenarios (or methods) sharing a name would silently interleave unrelated replicates in the same directory. `sim_design()`'s `+` operator and `sim_scenario_grid()`'s `name_fn` must both validate name uniqueness and call `cli::cli_abort()` on collision — never just `cli::cli_warn()`.

---

### 4.5 Pre-flight Check

Before committing to a full simulation run, `sim_check()` validates the design by running a small number of replicates of each (scenario, method) pair **sequentially**. It catches bugs cheaply, before any parallel infrastructure is spun up, and provides a timing estimate for the full run.

```r
design |> sim_check(replicates = 2)
```

```
── simkit pre-flight check ────────────────────────────────────────
✔ normal_n100     × sample_mean    [2/2 ok · 14 ms/rep · ~2.3 hrs total]
✔ normal_n100     × trimmed_mean   [2/2 ok · 31 ms/rep · ~5.2 hrs total]
✔ heavy_tail_n100 × sample_mean    [2/2 ok · 15 ms/rep · ~2.5 hrs total]
✗ heavy_tail_n100 × trimmed_mean   [0/2 ok]
  Error: argument "trim" not found in params
  Traceback: fn(data, params) → mean(data, trim = params$trim)
──────────────────────────────────────────────────────────────────
4 pairs checked · 3 passed · 1 failed · fix errors before running
```

`sim_check()` returns a `SimCheck` object invisibly. It prints a formatted report and, on failure, provides full error messages and call stacks. It can also be integrated directly into `sim_run()` via the `check` argument, which aborts the full run automatically if any pair fails:

```r
results <- design |> sim_run(progress = TRUE, check = TRUE)
```

`check = TRUE` runs `sim_check()` first (in its own scratch directory, per below), and only proceeds to the real run — writing into the design's actual `sim_storage()` path — if every pair passes. Check replicates never count toward `resume` progress; the full run's replicate numbering starts independently (from 1, or from wherever `resume = TRUE` finds it).

Two behaviors of `sim_check()` are worth stating explicitly:

- **The ETA is always computed from a sequential timing, but adjusted for the configured backend.** `sim_check()` times replicates one at a time, by design, before any parallel backend is spun up. If the design has `sim_parallel(workers = N)` configured, the printed report divides the naive sequential estimate by `N` and labels it accordingly (e.g. `~23 min total (parallel, 6 workers)`), rather than reporting a sequential-only figure that would overstate the real run's cost.
- **`sim_check()` never touches the design's configured storage.** It always writes its trial replicates to an ephemeral temp directory and removes them when it returns (pass or fail), regardless of what `sim_storage(path = ...)` is set to. This keeps pre-flight checks side-effect-free and re-runnable without disturbing `resume` state.

---

### 4.6 Running the Simulation

```r
results <- design |>
  sim_run(
    progress = TRUE,    # live progress bar (via progressr)
    on_error = "warn"   # "warn" (store NA + warning), "skip", or "stop"
  )
```

`sim_run()` returns a `SimResults` object. Internally it:

1. Enumerates all (scenario, method, replicate) triples.
2. Subtracts already-completed replicates when `resume = TRUE`.
3. Dispatches remaining work to the parallel backend.
4. Writes each result to disk atomically before moving to the next.
5. Emits progress via `progressr` with ETA.

If the session crashes and `resume = TRUE`, rerunning `sim_run()` on the same design picks up exactly where it left off.

---

### 4.7 Collecting Results

After `sim_run()`, results are on disk. `sim_collect()` reads them into memory as a tidy data frame. It accepts either a storage path — so it can be run in a fresh R session, independent of any `SimDesign`/`SimResults` object — or the `SimResults` handle returned directly by `sim_run()`:

```r
results <- design |> sim_run(progress = TRUE)
df <- results |> sim_collect()      # same session: pipe the handle directly

df <- sim_collect("./sim_results")  # fresh session: read by path
```

```r
results <- sim_collect("./sim_results")

# Returns a tibble:
# scenario        method        rep   est    se    ci_lo  ci_hi  elapsed_ms warnings
# normal_n100     sample_mean   1     2.031  0.102  1.831  2.231  14         NA
# normal_n100     sample_mean   2     1.978  0.099  1.784  2.172  13         NA
# ...
```

`sim_collect()` can also subset by scenario or method:

```r
results <- sim_collect(
  "./sim_results",
  scenarios = c("normal_n100", "heavy_tail_n100"),
  methods   = "sample_mean"
)
```

---

### 4.8 Defining and Computing Metrics

Metrics are functions that map (replicate results, truth) → scalar. The first argument is a data frame where each row is one replicate and each column is one returned quantity. The second argument is the named list returned by the scenario's `truth` function.

```r
evaluation <- results |>
  sim_evaluate(
    bias     = function(sims, truth) mean(sims$est) - truth$mean,
    mse      = function(sims, truth) mean((sims$est - truth$mean)^2),
    variance = function(sims, truth) var(sims$est),
    coverage = function(sims, truth) {
      mean(sims$ci_lo <= truth$mean & truth$mean <= sims$ci_hi)
    }
  )

# Returns a tibble:
# scenario        method        metric    value
# normal_n100     sample_mean   bias      0.0012
# normal_n100     sample_mean   mse       0.0102
# normal_n100     sample_mean   variance  0.0101
# normal_n100     sample_mean   coverage  0.948
# ...
```

`sim_evaluate()` maps over every (scenario, method) combination and calls each metric with the matching results and truth. It handles missing replicates gracefully.

If a metric function itself errors (e.g. it references a `truth` field that doesn't exist, or divides by zero), `sim_evaluate()` catches the error, emits a single `cli::cli_warn()` naming the metric/scenario/method triple, and records `NA` for that cell rather than aborting the whole evaluation. This is the same fail-soft philosophy as `on_error` during `sim_run()`, but it is not user-configurable: evaluation has no per-replicate checkpoint to protect and is cheap to rerun, so there's no reason to offer a `"stop"` mode — one bad metric should never take down an otherwise-successful evaluation.

---

### 4.9 Summarizing and Visualizing

```r
evaluation |> sim_summarize()

# Returns a wide-format tibble grouped by scenario + method:
# scenario        method        bias    mse     variance  coverage
# normal_n100     sample_mean   0.0012  0.0102  0.0101    0.948
# normal_n100     trimmed_mean  0.0008  0.0111  0.0110    NA
# heavy_tail_n100 sample_mean   0.0021  0.0850  0.0848    0.891
# heavy_tail_n100 trimmed_mean  0.0006  0.0210  0.0209    NA
```

A companion `sim_plot()` provides opinionated defaults for common visualizations:

```r
evaluation |>
  sim_plot(
    metric = "mse",
    x      = "n",           # extract from scenario params
    color  = "method",
    facet  = "dist"
  )
```

This returns a `ggplot2` object, so users can layer on additional customizations with `+`.

---

### 4.10 Bootstrap Variance Estimation

When a method's variance estimate requires bootstrapping, `sim_bootstrap()` wraps the method function and handles the inner replicates — optionally in parallel using a separate worker pool.

```r
design <- design +
  sim_method(
    name = "trimmed_mean_boot",
    fn   = sim_bootstrap(
      estimator  = function(data, params) list(est = mean(data, trim = 0.1)),
      B          = 500,
      statistic  = "est",          # which return value to bootstrap
      output     = "se",           # add $se to output
      parallel   = FALSE           # TRUE to nest parallelism
    )
  )
```

After running, `sims$se` contains the bootstrap SE for each replicate, and standard metrics like `coverage` work out of the box.

`output` accepts any subset of `c("se", "ci")`. `"se"` adds a single `$se` field (the bootstrap standard deviation of `statistic`). `"ci"` adds **two** fields, `$ci_lo` and `$ci_hi` (by default, the 2.5th/97.5th percentiles of the bootstrap distribution) — matching the naming convention methods use elsewhere in `simkit`, not a single combined `$ci` field.

**Oversubscription warning.** `parallel = TRUE` runs a second, inner `future` plan for the `B` bootstrap replicates, nested inside whatever outer plan `sim_parallel(workers = N)` is already running. If both levels default to "use all cores," total worker processes multiply (`N × inner_workers`), oversubscribing the machine and making everything slower, not faster. `sim_bootstrap(parallel = TRUE)` requires the user to have already configured a nested `future::plan()` topology (see `future`'s nested-plan documentation); `simkit` checks the worker count at both levels at runtime and warns if their product exceeds `parallel::detectCores()` (see `simkit_check_oversubscription()` in coding-guidelines §5).

---

## 5. Fault Tolerance in Detail

`simkit` uses a **write-on-complete** model:

- Each worker writes `rep_XXXX.rds` only after the estimator function returns (or errors).
- An error is caught per-replicate. With `on_error = "warn"`, a sentinel file `rep_XXXX_ERROR.rds` is written containing the error message and call stack, and simulation continues.
- The manifest file is written at the end of `sim_run()` (or updated incrementally).
- `resume = TRUE` inspects the directory for existing `.rds` files and skips those replicates.

This means:
- A single replicate failure never propagates.
- A session crash loses at most the in-flight replicates (≤ number of workers).
- Long simulations can be stopped and resumed without rerunning completed work.

After collection, failed replicates appear with `NA` values in the results data frame and a non-NA `error` column, which `sim_evaluate()` excludes by default.

---

## 6. Parallelism in Detail

`simkit` uses the `future` ecosystem by default. This gives backend-agnostic parallelism that works on laptops, HPC clusters, and cloud VMs.

```r
# Multicore (shared memory, Unix/Mac)
plan(multicore, workers = 8)

# Multisession (separate R processes, cross-platform)
plan(multisession, workers = 4)

# HPC cluster via SLURM (via future.batchtools)
plan(batchtools_slurm, resources = list(ncpus = 1, memory = "4gb"))
```

When `sim_parallel(backend = "future")` is set, `simkit` internally calls `future.apply::future_lapply()` over the replicate list, so whichever `plan()` the user has set (or `simkit` sets via `workers=`) is respected automatically.

The default grain of parallelism is the replicate. For very fast methods, users can set `chunk_size` to batch multiple replicates per worker call, reducing overhead.

---

## 7. Advanced Features

### Scenario Parameters in Methods

Methods receive the scenario `params` as a second argument. This is useful when a method needs to know (say) the sample size `n` to set a tuning parameter, without hardcoding it.

```r
sim_method(
  name = "adaptive_estimator",
  fn   = function(data, params) {
    bw <- bw_rule(params$n)
    list(est = kernel_mean(data, bw = bw))
  }
)
```

### Custom Seeds

`simkit` derives per-replicate seeds from the global seed using L'Ecuyer-CMRG streams, ensuring reproducibility regardless of the parallel backend and replicate ordering.

### Thinning / Storing Large Objects

Sometimes a method returns a large object (fitted model, posterior samples). The `store` argument of `sim_method()` controls what gets written to disk:

```r
sim_method(
  name  = "bayes_estimator",
  fn    = function(data, params) {
    fit <- run_mcmc(data)
    list(
      est       = mean(fit$samples),
      se        = sd(fit$samples),
      .full_fit = fit          # prefixed with . → stored separately, excluded from collect()
    )
  },
  store = "summary"   # "summary" (default): drop .prefixed fields from main results
                      # "full": store everything
)
```

### Simulation Labels and Versioning

`sim_design()` accepts a `label`, `version`, and `description` argument. These are embedded in the manifest and appear in the results data frame, making it easy to compare across simulation runs.

### Simulation Catalog

Researchers who accumulate many experiments over time can use `sim_catalog()` to scan a root directory for all `simkit` result folders and produce a tidy index. Each folder is identified by its `manifest.json`.

```r
sim_catalog("~/simulations")
```

```
── simkit catalog: ~/simulations ──────────────────────────────────
  # label                 date        status     scenarios  methods  progress    size  
  1 mean_estimators_v1    2025-01-15  ✔ complete  4          3        1000/1000   245 MB
  2 regression_study_v2   2025-02-03  ⚠ partial   9          2        847/1000    1.2 GB
  3 bootstrap_test        2025-02-10  ✗ errored   2          1        0/500       12 KB 
──────────────────────────────────────────────────────────────────
3 experiments · 1 complete · 1 partial · 1 errored
```

The returned `SimCatalog` is also a regular tibble, so it can be filtered and manipulated with `dplyr`. Printing a single row with `full = TRUE` reveals provenance details stored in the manifest:

```r
cat <- sim_catalog("~/simulations")

# Detailed view of one experiment
cat[1, ] |> print(full = TRUE)
# Label:       mean_estimators_v1
# Description: Comparing sample mean, trimmed mean, and median under
#              normal and heavy-tailed errors. Varying n = 100, 500.
# Date:        2025-01-15 14:32
# R version:   4.3.2 · simkit: 0.2.0
# Seed:        42 · Replicates: 1000

# Load results directly from the catalog entry
cat[1, ] |> sim_load() |> sim_collect()

# Filter with dplyr, then load
cat |> filter(status == "partial") |> sim_load()
```

---

## 8. Complete Worked Example

```r
library(simkit)

# ── 1. Design ──────────────────────────────────────────────────────────────

design <- sim_design(
    replicates  = 1000,
    seed        = 42,
    label       = "mean_estimators_v1",
    description = "Comparing sample mean, trimmed mean, and median under
                   normal and heavy-tailed errors. Varying n = 100, 500."
  ) +

  sim_scenario_grid(
    params  = list(n = c(100, 500), dist = c("normal", "t3")),
    name_fn = function(p) paste0(p$dist, "_n", p$n),
    dgp     = function(params) {
      if (params$dist == "normal") rnorm(params$n, mean = 2, sd = 1)
      else                         2 + rt(params$n, df = 3)
    },
    truth = function(params) list(mean = 2)
  ) +

  sim_method(
    name = "sample_mean",
    fn   = function(data, params) {
      se <- sd(data) / sqrt(params$n)
      list(est = mean(data), se = se,
           ci_lo = mean(data) - 1.96 * se,
           ci_hi = mean(data) + 1.96 * se)
    }
  ) +

  sim_method(
    name = "trimmed_mean",
    fn   = sim_bootstrap(
      estimator = function(data, params) list(est = mean(data, trim = 0.1)),
      B = 300, statistic = "est", output = c("se", "ci")
    )
  ) +

  sim_method(
    name = "median",
    fn   = sim_bootstrap(
      estimator = function(data, params) list(est = median(data)),
      B = 300, statistic = "est", output = c("se", "ci")
    )
  ) +

  sim_parallel(workers = 6) +
  sim_storage(path = "./mean_sim", resume = TRUE)


# ── 2. Validate ────────────────────────────────────────────────────────────

design |> sim_check(replicates = 2)   # fix any errors before committing


# ── 3. Run ─────────────────────────────────────────────────────────────────

results <- design |> sim_run(progress = TRUE, on_error = "warn")


# ── 4. Evaluate ────────────────────────────────────────────────────────────

evaluation <- results |>
  sim_collect() |>
  sim_evaluate(
    bias     = function(sims, truth) mean(sims$est)             - truth$mean,
    mse      = function(sims, truth) mean((sims$est - truth$mean)^2),
    variance = function(sims, truth) var(sims$est),
    coverage = function(sims, truth) mean(sims$ci_lo <= truth$mean &
                                          truth$mean <= sims$ci_hi)
  )


# ── 5. Report ──────────────────────────────────────────────────────────────

evaluation |> sim_summarize()

evaluation |>
  sim_plot(metric = "mse", x = "n", color = "method", facet = "dist") +
  scale_y_log10() +
  labs(title = "MSE of mean estimators under normal vs. heavy-tailed errors")


# ── 6. Revisit later ───────────────────────────────────────────────────────

sim_catalog("~/simulations")           # find the experiment again
sim_catalog("~/simulations")[1, ] |>
  sim_load() |>
  sim_collect()                        # recover results in a fresh session
```

---

## 9. Implementation Roadmap

| Phase | Milestone | Key dependencies |
|---|---|---|
| 0 | Core S3 classes, `+` operator, print methods | base R |
| 1 | Sequential runner, RDS storage, `sim_collect()`, `sim_check()` | `fs`, `cli` |
| 2 | Parallelism via `future` | `future`, `future.apply`, `progressr` |
| 3 | `sim_evaluate()`, `sim_summarize()`, tidy output | `dplyr`, `tidyr` |
| 4 | `sim_bootstrap()`, nested parallelism | `future` |
| 5 | `sim_plot()`, scenario param extraction | `ggplot2` |
| 6 | `qs` + `arrow` storage backends, SLURM docs | `qs`, `arrow` |
| 7 | `sim_scenario_grid()`, scenario versioning, `sim_catalog()` | — |

---

## 10. Dependencies

| Package | Role |
|---|---|
| `future` + `future.apply` | Backend-agnostic parallelism |
| `progressr` | Progress bars compatible with parallel backends |
| `fs` | Cross-platform file system operations |
| `cli` | Pretty console output and messages |
| `rlang` | Structured conditions, `caller_env()` for error attribution |
| `jsonlite` | Reading/writing `manifest.json` and `in_progress.json` |
| `digest` | Hashing designs for storage-conflict detection (`design_hash`) |
| `dplyr` + `tidyr` | Tidy output data frames |
| `ggplot2` | `sim_plot()` |

Alternative storage backends (`qs` for fast binary serialization, `arrow`
for columnar storage of very large simulations) are deferred for now --
`sim_storage()` supports only `format = "rds"` at present. See
`dev/session-plan.md` Session 10 for the intended future scope.
