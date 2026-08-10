# `simkit` — Claude Code Session Plan

Each session is scoped to produce working, tested code without overrunning context limits. Sessions build strictly on prior ones. Do not skip ahead.

---

## How to Start Every Session

1. Open a fresh Claude Code session in your `simkit/` project directory.
2. Paste the **context block** specified for that session (design doc sections + guidelines sections).
3. Add the task statement: *"Implement [session goal] according to the spec and guidelines above. Write tests as you go. Run `devtools::test()` before finishing."*
4. At the end of the session, run `devtools::check()` and fix any warnings or notes before closing.

Claude Code reads your existing files automatically — you do not need to paste prior code, only the spec sections relevant to the new work.

---

## Session 0 — Project Setup

**Not a Claude Code session.** Run these commands in R before any coding begins. Tested against `usethis` 3.2.1 / `devtools` 2.5.2 / R 4.6.1 — adjust if your installed versions differ (check with `packageVersion("usethis")` first; older usethis exposes `use_github_actions()` instead of `use_github_action()`, for example).

### 1. Never install dependencies into the global R library

Before anything else, decide how package dependencies will be isolated from your global/system R library. `renv` is the standard tool and is assumed below. If you don't already have it: `install.packages("renv")` once, globally — `renv` itself is the one exception, since it's the tool that keeps everything *else* local.

### 2. Scaffold the package

```r
pkg_name <- "simkit"        # the actual R package name — must be letters/numbers/dots only
pkg_path <- getwd()         # or wherever your repo root is

usethis::create_package(
  path       = pkg_path,
  fields     = list(
    Package  = pkg_name,
    `Authors@R` = utils::person("Your", "Name", email = "you@example.com", role = c("aut", "cre"))
  ),
  check_name = FALSE,   # needed if the repo directory name != pkg_name (e.g. contains a hyphen)
  rstudio    = FALSE,
  open       = FALSE
)

setwd(pkg_path)  # create_package()'s active-project context does not persist past its own call

usethis::use_mit_license()
usethis::use_readme_rmd()
usethis::use_news_md()
usethis::use_testthat(edition = 3)
usethis::use_github_action("check-standard")  # usethis >= 3.0; use use_github_actions() on older versions
usethis::use_pkgdown()
usethis::use_git()
usethis::use_test("placeholder")  # testthat 3e's R CMD check ERRORs on zero test files — replace in Session 1
```

If you're scaffolding into an already-git-initialized repo that has other files in it (READMEs, planning docs, agent config directories, etc.), add each of them to `.Rbuildignore` so `R CMD check` doesn't flag them as "non-standard top-level files" — e.g. `usethis::use_build_ignore(c("dev", "CLAUDE.md", ".claude"))`.

`usethis::use_news_md()` writes a `NEWS.md` header as `# pkgname (development version)`. R's news parser requires a real version number in the heading to register any entries, or `R CMD check` emits a "No news entries found" NOTE — change the header to `# pkgname 0.0.0.9000` (matching `DESCRIPTION`'s `Version`) to avoid this.

### 3. Fill in DESCRIPTION

Edit `DESCRIPTION` manually: write `Title` and `Description`, and add the `Imports`/`Suggests` fields from the coding guidelines (Section 1).

### 4. Set up the isolated dependency library and install

```r
renv::init(bare = TRUE, force = TRUE)   # bare = TRUE: don't let init auto-install yet, we control that next
```

Then hydrate/install everything `DESCRIPTION` declares, **except** any optional/heavy `Suggests` backends not needed until a later session (check the coding guidelines and later session scopes for which packages those are — e.g. anything that compiles a large C/C++ library from source, like `arrow`, is worth deferring). `renv::hydrate()` links already-installed packages from your existing library straight into the project-local one at zero cost, and only downloads what's genuinely missing:

```r
renv::hydrate(packages = c(
  # DESCRIPTION Imports + the near-term Suggests, deliberately excluding
  # heavy/optional backends slated for a much later session
  "cli", "digest", "fs", "future", "future.apply", "jsonlite",
  "progressr", "rlang", "dplyr", "tidyr", "testthat", "ggplot2",
  "future.batchtools",
  # dev tooling used to build/check/test the package itself
  "devtools", "usethis", "roxygen2", "pkgdown"
))

renv::snapshot(prompt = FALSE)
```

`devtools::check()` treats a declared-but-uninstalled `Suggests` package as informational ("suggested but not available"), not a failure — so deferring heavy optional backends is safe. Install them for real only in the session that first needs them.

### 5. Verify and commit

```r
devtools::test()    # 1 placeholder test should pass
devtools::check()   # must be 0 errors, 0 warnings, 0 notes
```

Commit `DESCRIPTION`, `NAMESPACE`, `R/`, `tests/`, `.github/`, `_pkgdown.yml`, `.Rbuildignore`, `.gitignore`, `renv.lock`, `.Rprofile`, and `renv/` (renv's own `.gitignore` excludes the actual library contents, so only lockfile + activation script are tracked). Push before Session 1.

**Done when:** `devtools::check()` passes with 0 errors, 0 warnings, 0 notes, and dependencies resolve from the project-local `renv` library rather than the global R library.

---

## Session 1 — S3 Classes and the `+` Operator

**Goal:** Define all six core S3 classes with constructors, validators, and print methods, plus the `+.SimDesign` operator. No simulation logic yet — just the data structures.

**Context to provide:**
- Design doc: Sections 2 (Architecture), 3 (Object Model), 4.1–4.4
- Coding guidelines: Sections 2 (File Layout), 3 (S3 Class Design), 4 (Functional Design)

**Scope:**
- `R/sim_design.R` — `sim_design()`, `new_SimDesign()`, `validate_SimDesign()`, `print.SimDesign()`, `+.SimDesign()`
- `R/sim_scenario.R` — `sim_scenario()`, `new_SimScenario()`, `print.SimScenario()`
- `R/sim_method.R` — `sim_method()`, `new_SimMethod()`, `print.SimMethod()`
- `R/sim_parallel.R` — `sim_parallel()`, `new_SimParallel()`
- `R/sim_storage.R` — `sim_storage()`, `new_SimStorage()` (file I/O helpers come in Session 2)
- `tests/testthat/helper-fixtures.R` — `base_design()`, `simple_dgp`, `simple_truth`, `simple_method`
- `tests/testthat/test-classes.R`

**Deliverables:** All classes construct without error, print readably, and the `+` operator correctly assembles a `SimDesign`. Duplicate scenario/method names must raise a `cli::cli_abort()` (not just a warning) — see coding-guidelines §3.

**Verification:**
```r
devtools::test(filter = "classes")
d <- sim_design(replicates = 100, seed = 1) +
  sim_scenario("s1", list(n=30, mu=0), function(p) rnorm(p$n), function(p) list(mean=p$mu)) +
  sim_method("m1", function(data, params) list(est = mean(data)))
print(d)   # should render cleanly
```

**Do not include:** Any file I/O, running simulations, or parallel logic.

---

## Session 2 — Sequential Runner, Storage, and `sim_collect()`

**Goal:** Make simulations actually run — sequentially, with checkpointed RDS storage, resume support, and fault tolerance. This is the most critical session; get it right before adding parallelism.

**Context to provide:**
- Design doc: Sections 4.5, 4.6, 4.7, 5 (Fault Tolerance in Detail)
- Coding guidelines: Sections 6 (File I/O), 7 (Error Handling), 8 (Testing — known-answer and fault-tolerance tests)

**Scope:**
- `R/sim_run.R` — `sim_run()` (sequential only), internal dispatch loop, `on_error` handling
- `R/sim_storage.R` — `simkit_write_rep()` (atomic write), `simkit_completed_reps()`, `simkit_check_storage_conflict()`, manifest read/write, `simkit_design_hash()`
- `R/sim_collect.R` — `sim_collect()`, sentinel file detection
- `tests/testthat/test-run-sequential.R` — known-answer test, fault-tolerance test, resume test

**Deliverables:** `sim_run()` completes without error, writes one `.rds` per replicate, and `sim_collect()` returns a tidy tibble. A deliberate per-replicate error produces a sentinel file and does not halt the run. Rerunning with `resume = TRUE` skips completed replicates. `sim_run()` injects `params$.rep_index` and `params$.rng_seed` before calling each method's `fn` (design.md §4.3). `sim_collect()` accepts either a storage path or the `SimResults` object returned by `sim_run()` (design.md §4.7). `manifest.json` is written using the schema in design.md §4.4, using `jsonlite`; `simkit_design_hash()` uses `digest` over deparsed function source, not raw closures (coding-guidelines §6).

**Verification:**
```r
devtools::test(filter = "run-sequential")
# Also run manually:
dir <- tempdir()
design <- base_design(replicates = 20) + sim_storage(path = dir, resume = FALSE)
res <- sim_run(design, progress = FALSE)
df  <- sim_collect(dir)
nrow(df) == 20
```

**Do not include:** Parallel dispatch, seed management beyond `set.seed()`, or `sim_check()`.

---

## Session 3 — Pre-flight Check (`sim_check()`)

**Goal:** Implement the pre-flight validator that runs a small number of replicates sequentially, reports pass/fail per pair with timing and ETA, and integrates with `sim_run(check = TRUE)`.

**Context to provide:**
- Design doc: Section 4.5 (Pre-flight Check) in full
- Coding guidelines: Sections 3 (print methods), 7 (error handling)

**Scope:**
- `R/sim_check.R` — `sim_check()`, `new_SimCheck()`, `print.SimCheck()`
- Update `R/sim_run.R` — add `check` argument that calls `sim_check()` and aborts on failure
- `tests/testthat/test-check.R` — passing check, failing check, check with `sim_run(check = TRUE)`

**Deliverables:** `sim_check()` produces a formatted report, returns a `SimCheck` object invisibly, shows full tracebacks for failures, and provides a per-pair ETA. `sim_run(check = TRUE)` aborts with a clear message if any pair fails. Per design.md §4.5, `sim_check()` always executes in an ephemeral scratch directory — never the design's configured `sim_storage()` path — and its ETA divides the sequential per-replicate timing by the design's configured worker count when `sim_parallel()` is set, rather than always reporting a sequential-only estimate.

**Verification:**
```r
devtools::test(filter = "check")
# Manually: inject an error into a method and confirm sim_check catches it
```

**Do not include:** Any changes to the parallel path.

---

## Session 4 — Parallelism and Seed Management

**Goal:** Extend `sim_run()` to dispatch replicates via `future`, with L'Ecuyer-CMRG seeds that guarantee parallel results match sequential results under the same global seed.

**Context to provide:**
- Design doc: Section 6 (Parallelism in Detail)
- Coding guidelines: Section 5 (Parallel Computing) in full

**Scope:**
- `R/sim_parallel.R` — `simkit_make_seeds()` (L'Ecuyer-CMRG, threaded through a local variable — no `<<-`, per coding-guidelines §5), `simkit_check_closure()`
- `R/sim_run.R` — branch on `parallel$backend`; add `future.apply::future_lapply()` path; closure size warning
- `tests/testthat/test-run-parallel.R` — parallel matches sequential, closure warning fires

**Deliverables:** `sim_run()` with `sim_parallel(workers = 2)` produces identical results to the sequential run given the same seed. A method function with a large captured environment emits a `cli_warn()`.

**Verification:**
```r
devtools::test(filter = "run-parallel")
# Key assertion: seq_results and par_results have identical `est` columns after sorting by rep
```

**Do not include:** Nested parallelism for bootstrap (that comes in Session 7), SLURM configuration.

---

## Session 5 — Evaluation and Summarization

**Goal:** Implement `sim_evaluate()` and `sim_summarize()`, producing tidy metric outputs from collected results.

**Context to provide:**
- Design doc: Sections 4.8, 4.9
- Coding guidelines: Section 8 (Testing — known-answer test structure)

**Scope:**
- `R/sim_evaluate.R` — `sim_evaluate()`, `sim_summarize()`
- Update `tests/testthat/test-run-sequential.R` — extend the known-answer test to assert `bias ≈ 0` and `mse ≈ 1/n` through the full pipeline
- `tests/testthat/test-evaluate.R` — metric edge cases: `NA` replicates excluded, missing CI columns produce `NA` for coverage gracefully

**Deliverables:** `sim_evaluate()` maps over every (scenario, method) pair, calls each user-supplied metric function, and returns a long-format tidy tibble. `sim_summarize()` pivots to wide format. Both handle `NA` replicates without error. A metric function that itself errors produces `NA` for that (metric, scenario, method) cell plus a single `cli_warn()`, per coding-guidelines §7 — it must not abort the rest of the evaluation.

**Verification:**
```r
devtools::test(filter = "evaluate")
# Full pipeline smoke test:
base_design(replicates = 500) |>
  sim_storage(path = tempdir()) |>
  sim_run() |>
  sim_collect() |>
  sim_evaluate(bias = function(s,t) mean(s$est) - t$mean) |>
  sim_summarize()
```

**Do not include:** `sim_plot()`, scenario parameter extraction from names.

---

## Session 6 — Scenario Grids

**Goal:** Implement `sim_scenario_grid()` to generate factorial scenario sets from a parameter list.

**Context to provide:**
- Design doc: Section 4.2 (Scenario Grids subsection)
- Coding guidelines: Section 3 (constructor pattern)

**Scope:**
- `R/sim_scenario.R` — add `sim_scenario_grid()`; internal `simkit_expand_params()` using `expand.grid()`
- `tests/testthat/test-classes.R` — grid generates correct number of scenarios, `name_fn` applied correctly, duplicate names detected

**Deliverables:** `sim_scenario_grid()` with `n = c(100, 500)` and `dist = c("normal", "t3")` produces exactly 4 named `SimScenario` objects, each with the correct `params` list.

**Verification:**
```r
devtools::test(filter = "classes")
grid <- sim_scenario_grid(
  params  = list(n = c(100, 500), mu = 2, sigma = c(0.5, 1.0)),
  name_fn = function(p) paste0("n", p$n, "_s", p$sigma),
  dgp     = function(p) rnorm(p$n, p$mu, p$sigma),
  truth   = function(p) list(mean = p$mu)
)
length(grid) == 4
```

**Do not include:** Any runner or evaluation changes.

---

## Session 7 — Bootstrap Wrapper

**Goal:** Implement `sim_bootstrap()` as a method function wrapper that runs B bootstrap replicates of an inner estimator and appends SE and CI to the output.

**Context to provide:**
- Design doc: Section 4.10 (Bootstrap Variance Estimation)
- Coding guidelines: Section 5 (parallel computing, nested parallelism note)

**Scope:**
- `R/sim_method.R` — add `sim_bootstrap()`; internal `simkit_boot_once()`
- `tests/testthat/test-bootstrap.R` — bootstrap SE is in the right ballpark for `mean`, `parallel = TRUE` and `parallel = FALSE` give consistent results

**Deliverables:** A method wrapped with `sim_bootstrap()` returns `est`, `se`, `ci_lo`, and `ci_hi` in its output list — `output = "ci"` adds `$ci_lo`/`$ci_hi`, not a combined `$ci` field (design.md §4.10). Works as a drop-in for `sim_method(fn = sim_bootstrap(...))`. The `parallel = TRUE` path uses a nested `future` plan correctly, and calls `simkit_check_oversubscription()` (coding-guidelines §5) before dispatching, warning if outer × inner workers exceeds `parallel::detectCores()`.

**Verification:**
```r
devtools::test(filter = "bootstrap")
# Smoke test: bootstrap SE for mean of N(0,1), n=100 should be ~0.1
```

**Do not include:** Any changes to `sim_run()` beyond what's needed to pass bootstrap output through cleanly.

---

## Session 8 — Simulation Catalog

**Goal:** Implement `sim_catalog()` to index a root directory of experiments, and `sim_load()` to recover a `SimDesign` and its results from a catalog entry.

**Context to provide:**
- Design doc: Section 7 (Simulation Catalog subsection) in full
- Coding guidelines: Sections 6 (manifest writes), 10 (naming conventions)

**Scope:**
- `R/sim_catalog.R` — `sim_catalog()`, `new_SimCatalog()`, `print.SimCatalog()`, `sim_load()`
- Update `R/sim_storage.R` — ensure manifest stores `label`, `description`, `seed`, R version, simkit version, `design_hash`
- `tests/testthat/test-catalog.R` — catalog finds all experiments, status detection (complete / partial / errored), `sim_load()` recovers results

**Deliverables:** `sim_catalog("~/sims")` prints a formatted table. The returned object is also a tibble. `dplyr::filter()` works on it. `sim_load()` on a single-row catalog returns a `SimResults` object ready for `sim_collect()`. The manifest schema is fully specified in design.md §4.4 — read exactly those fields rather than inferring structure.

**Verification:**
```r
devtools::test(filter = "catalog")
# Create two experiments in tempdir(), then:
cat <- sim_catalog(tempdir())
nrow(cat) == 2
cat |> dplyr::filter(status == "complete") |> sim_load() |> sim_collect()
```

**Do not include:** `sim_diff()` (design comparison) — a potential future feature.

---

## Session 9 — Plots

**Goal:** Implement `sim_plot()` as a `ggplot2`-based convenience layer that extracts scenario parameters and maps them to aesthetics.

**Context to provide:**
- Design doc: Section 4.9 (`sim_plot()` paragraph)
- No guidelines section needed; keep this session short

**Scope:**
- `R/sim_plot.R` — `sim_plot()`; internal `simkit_extract_param()` that parses scenario names or reads params from manifest
- `tests/testthat/test-plot.R` — returns a `ggplot` object, fails informatively when `metric` is not in the evaluation

**Deliverables:** `sim_plot(eval, metric = "mse", x = "n", color = "method")` returns a `ggplot` object. Users can append `+ scale_y_log10()` etc. Fails with a clear message if the requested metric or scenario parameter does not exist.

**Verification:**
```r
devtools::test(filter = "plot")
p <- evaluation |> sim_plot(metric = "bias", x = "n", color = "method")
inherits(p, "ggplot")
```

**Do not include:** Any new metrics or evaluation logic.

---

## Session 10 — Alternative Storage Backends (deferred)

**Status: deferred.** An implementation attempt found that `arrow` has no prebuilt binary for at least one target platform (Fedora 44 / R 4.6) and must build its C++ library from source, which is a heavy, slow, environment-fragile install (it exhausted a tmpfs `/tmp` quota during testing). Given `qs`'s speed advantage is marginal for `simkit`'s typical payload (small per-replicate scalar lists, dominated by per-file I/O rather than serialization cost) and `arrow`'s one-row-per-replicate layout doesn't get Parquet's real columnar benefit, this session is on hold until there's a concrete need. `sim_storage()` currently accepts only `format = "rds"`; revisit this session before changing that.

**Goal:** Add `qs` and `arrow`/parquet as optional storage formats, dispatched by the `format` argument of `sim_storage()`.

**Context to provide:**
- Design doc: Section 4.4 (storage format argument), Section 10 (Dependencies)
- Coding guidelines: Section 1 (`Suggests:` vs `Imports:`)

**Scope:**
- `R/sim_storage.R` — refactor `simkit_write_rep()` and `simkit_read_rep()` to dispatch on format; add `simkit_check_backend()` that calls `rlang::check_installed()` for optional deps
- `tests/testthat/test-storage.R` — round-trip tests for each format; graceful error when backend not installed

**Deliverables:** `sim_storage(format = "qs")` and `sim_storage(format = "arrow")` work end-to-end. If the required package is not installed, a clear installation prompt is shown. Default `"rds"` is entirely unaffected.

**Verification:**
```r
devtools::test(filter = "storage")
```

**Do not include:** Any schema changes to the manifest format.

---

## Session 11 — Vignette and Documentation Polish

**Goal:** Write the Getting Started vignette, complete all roxygen2 docs, and ensure `pkgdown` builds cleanly.

**Context to provide:**
- Design doc: Section 8 (Complete Worked Example) — this becomes the vignette body
- Coding guidelines: Section 9 (Documentation)

**Scope:**
- `vignettes/getting-started.Rmd` — worked example from the design doc, rendered with `replicates = 50` for build speed
- Full `@param`, `@return`, `@examples` on every exported function (Claude Code can audit which are missing with `devtools::check()` output)
- `_pkgdown.yml` — organize reference page by section (Design, Run, Evaluate, Utilities)

**Deliverables:** `devtools::check()` produces zero warnings and zero notes related to documentation. `pkgdown::build_site()` completes without error.

**Verification:**
```r
devtools::check()
pkgdown::build_site(preview = TRUE)
```

**Do not include:** New features. This session is documentation only.

---

## Quick Reference

| Session | Goal | New Files | Est. Lines |
|---|---|---|---|
| 0 | Manual scaffold | `DESCRIPTION`, GH Actions | — |
| 1 | S3 classes + `+` | 5 R files, 2 test files | ~350 |
| 2 | Sequential runner + storage | 3 R files, 1 test file | ~450 |
| 3 | `sim_check()` | 1 R file, 1 test file | ~200 |
| 4 | Parallelism + seeds | 2 R files, 1 test file | ~250 |
| 5 | Evaluate + summarize | 1 R file, 1 test file | ~250 |
| 6 | Scenario grids | extends existing | ~150 |
| 7 | Bootstrap wrapper | 1 R file, 1 test file | ~250 |
| 8 | Catalog + load | 2 R files, 1 test file | ~300 |
| 9 | Plots | 1 R file, 1 test file | ~150 |
| 10 | Alt. storage backends | extends existing | ~150 |
| 11 | Vignette + docs | 1 Rmd, `_pkgdown.yml` | ~200 |
