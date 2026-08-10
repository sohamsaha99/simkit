# CLAUDE.md

This file guides AI coding agents working on the `simkit` R package.

---

## What This Project Is

`simkit` is an R package that provides a tidy, pipeline-based framework for statistical simulation studies. It handles parallelism, checkpointed storage, fault tolerance, progress reporting, and performance evaluation so researchers can focus on estimators and scenarios.

---

## Key Documents

Read these before writing any code. They are the authoritative source of truth.

| File | Purpose |
|---|---|
| `dev/design.md` | Full API design, object model, syntax examples, and worked example |
| `dev/coding-guidelines.md` | S3 conventions, parallelism rules, file I/O, testing strategy, naming |
| `dev/session-plan.md` | What each Claude Code session should build, its scope, and verification steps |

When in doubt about intended behavior, check `dev/design.md` first.

---

## Non-Negotiable Rules

- **Never mutate a `SimDesign` in place.** The `+` operator must return a modified copy.
- **Never write directly to the final `.rds` path.** Always write to `.tmp` first, then `file.rename()`.
- **Never use `stop()` directly.** Use `cli::cli_abort()` for user-facing errors.
- **Never add packages to `Imports:` without updating `DESCRIPTION`.** Optional backends (`qs`, `arrow`) belong in `Suggests:`.
- **Always run `devtools::test()` before ending a session.** Do not leave failing tests.

---

## Running Tests

```r
devtools::test()                        # full suite
devtools::test(filter = "classes")      # single file
devtools::check()                       # full CRAN check — run before closing a session
```

Test fixtures live in `tests/testthat/helper-fixtures.R` and are auto-loaded by `testthat`. Use `base_design()` as the canonical starting point in tests; do not define ad-hoc designs inline.

---

## Current Implementation Phase

Update this line at the start of each session:

**Active session:** Session 11 — Vignette and Documentation Polish
---


