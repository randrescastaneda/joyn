---
date: 2026-05-08
title: "joyn Internal Performance Optimization and Test Hardening"
status: completed
completed-date: 2026-05-08
completed-phases: [1, 2, 3, 4]
scope: "Deep"
brainstorm: ".cg-docs/brainstorms/2026-05-08-performance-testing-audit.md"
language: "R"
estimated-effort: "large"
phases: 4
review-findings: [P1.1-merged-into-step2, P1.2-flush-in-joyn_msg, P2.1-7-clear-sites, P2.2-steps-merged, P3.1-clarified, P3.2-moved, P3.3-bench-dep]
tags: [performance, testing, refactoring, benchmarks, messaging, glue-removal]
---

# Plan: joyn Internal Performance Optimization and Test Hardening

## Objective

Optimize joyn's internal performance by fixing identified bottlenecks in the
messaging system, workhorse join, type-checking, and dependency chain — while
hardening reliability by replacing fragile patterns and closing test coverage
gaps. The user-facing API (function signatures, parameter order, output
structure) must remain unchanged.

## Context

- **Current state**: 4,551 lines of R source across 16 files; 6,187 lines of
  tests across 13 test files. All tests pass (6 skipped, 0 failures).
- **Brainstorm decision**: Approach 1 (Targeted Internal Optimization) plus
  benchmarking from Approach 3.
- **Key bottlenecks identified**:
  1. Messaging system (`store_msg`) creates a new data.frame per message and
     `rowbind()`s on every call — O(n²) for n messages.
  2. `joyn_workhorse` applies two `ftransform()` calls plus a `frename()` to
     remap the `.joyn1` column.
  3. `check_var_class` calls `inherits()` 8 times per key variable via
     `vapply()` inner loop.
  4. `glue::glue()` used in `check_var_class` when `cli` already handles
     interpolation — redundant dependency.
  5. `clear_joynenv()` uses `sys.call(-1)` — fragile across call stacks.
  6. `store_joyn_msg` uses `parent.frame(1)` for `cli::format_inline()` —
     fragile scope resolution.
- **Files with no test coverage**: `prep_m_to_m_data.R` (entirely commented
  out — 147 lines of dead code), `joyn-merge.R` (covered indirectly by
  `test-joyn.R` but no dedicated file).
- **Constraints**: API frozen, `collapse` stays, `glue` can be dropped.

## Requirements

| ID  | Requirement                                                      | Source     |
|-----|------------------------------------------------------------------|------------|
| R1  | Messaging system must accumulate messages in O(1) amortized time | brainstorm |
| R2  | Workhorse `.joyn1` remapping must use a single operation         | brainstorm |
| R3  | `check_var_class` must avoid per-class `inherits()` loop         | brainstorm |
| R4  | Remove `glue` from Imports; replace with `cli` interpolation     | brainstorm |
| R5  | `clear_joynenv()` must not rely on `sys.call(-1)`                | brainstorm |
| R6  | Benchmark suite must measure before/after for all join types     | brainstorm |
| R7  | Add tests for missing coverage areas                             | brainstorm |
| R8  | Fix documentation typos (e.g., "argumet")                        | brainstorm |
| R9  | All existing tests must continue to pass after each change       | implicit   |
| R10 | User-facing API (signatures, parameter order) must not change    | brainstorm |

## Implementation Steps

## Phase 1: Benchmarks and Baseline

### 1. Create benchmark infrastructure
- **Requirements**: R6
- **Files**: create `inst/benchmarks/bench_joyn.R`; add `bench` to `Suggests` in `DESCRIPTION`
- **Details** (revised per P3.3):
  - Add `bench` to `Suggests` in `DESCRIPTION` (not `Imports` — benchmarks are optional).
  - Place all benchmark scripts under `inst/benchmarks/` — never under `tests/` (would
    require `bench` in `Suggests` AND a `skip_if_not_installed` guard in every test).
  - Use `bench::mark()` with `skip_if_not_installed("bench")` guard at the top of the
    script so it fails gracefully when bench is absent.
  - Time: `joyn()`, `left_join()`, `right_join()`, `inner_join()`, `full_join()`,
    `anti_join()` across dataset sizes: 1K, 10K, 100K rows (skip 1M in CI; note it).
  - Test with 1-key and 3-key joins; match types: "1:1", "m:1", "1:m", "m:m".
  - Also time internal functions: `store_msg()` (100 calls), `check_var_class()`,
    `joyn_workhorse()`.
  - Save baseline output as a comment block at top of script for before/after comparison.
- **Test Scenarios**:
  - ✅ Benchmark script runs without error when `bench` is installed
  - ✅ Script skips gracefully when `bench` is not installed
  - 🛑 All six join functions complete at 100K rows
  - ❌ Out-of-memory on very large inputs — documented, not tested
- **Acceptance criteria**: Baseline timings recorded; `bench` in DESCRIPTION Suggests;
  script lives in `inst/benchmarks/`; re-runnable after optimizations.

## Phase 2: Core Optimizations

### 2. Redesign messaging pipeline — list accumulator + eager flush in `joyn_msg()` (addresses P1.1 + P1.2 + P2.2)
- **Requirements**: R1, R9
- **Files**: `R/info_display.R`
- **Design decision** (co-designed to resolve P1.1, P1.2, P2.2 simultaneously):
  - **Keep** `cli::format_inline(.envir = parent.frame(1))` inside `store_joyn_msg()` —
    this is load-bearing and removing it requires updating 20+ call sites with silent
    regression risk. Fragility is accepted in exchange for stability.
  - **Change only the accumulation mechanism**: `store_msg()` stores pre-formatted
    strings into a `list()` instead of building a data.frame row-by-row.
  - **Flush is eager, not lazy**: `joyn_msg()` always materializes the list into a
    data.frame before reading. This ensures all three mid-join sinks work correctly:
    - `checks.R`: `joyn_msg("err")` in `check_xy()` before `cli::cli_abort()`
    - `merge-data.table.R`: `joyn_msg()` before abort
    - `joyn-merge.R`: `joyn_msg(msg_type)` at normal completion
- **Details**:
  - In `store_msg()`: replace the `do.call(msg_type_dt, ...) |> rowbind()` pattern
    with `list(type = type, msg = styled_text)` appended to a list in `.joynenv`.
  - Add `flush_joyn_msgs()` internal function: converts list to data.frame via
    `rbindlist()` (from data.table, already a dependency).
  - Modify `joyn_msg()` to call `flush_joyn_msgs()` at its top before any read.
  - Modify `joyn_msgs_exist()` to check the list (not the data.frame).
  - Modify `clear_joynenv()` to clear the list (not the data.frame).
  - Apply `funique()` dedup once inside `flush_joyn_msgs()`, not per-message.
- **Test Scenarios**:
  - ✅ `store_msg()` → `joyn_msg()` round-trip produces identical output to before
  - ✅ `joyn_msg("err")` called mid-join (error path) shows messages correctly
  - 🛑 Storing 100+ messages doesn't degrade (O(1) per store call)
  - ❌ Reading messages before any `store_msg()` call → proper error from `joyn_msgs_exist()`
- **Tests**: Extend `test-info_display.R`:
  - High-volume test: store 50 messages, verify all appear
  - Error-path test: verify `joyn_msg("err")` displays messages even when called from `check_xy()`
- **Acceptance criteria**: `store_msg()` is O(1) per call; `joyn_msg()` always flushes
  before reading; all existing `test-info_display.R` and `test-joyn.R` tests pass.

### 3. Consolidate workhorse `.joyn1` remapping
- **Requirements**: R2, R9
- **Files**: `R/joyn_workhorse.R`
- **Details**:
  - Replace the two `ftransform()` + `frename()` calls:
    ```r
    dt_result <- dt_result |>
      ftransform(.joyn1 = as.numeric(.joyn1)) |>
      ftransform(.joyn1 = mapping[as.character(.joyn1)]) |>
      frename(.joyn1 = reportvar, .nse = FALSE)
    ```
    with a single `ftransform()` using a direct integer lookup:
    ```r
    mapping <- c(`1` = 3L, `2` = 1L, `3` = 2L)
    dt_result[[reportvar]] <- mapping[dt_result[[".joyn1"]]]
    dt_result[[".joyn1"]] <- NULL
    ```
  - This avoids two full-column transformations and a rename.
- **Test Scenarios**:
  - ✅ All join types produce correct `.joyn` values (1=x, 2=y, 3=x&y)
  - 🛑 `.joyn1` column values include all 3 source types
  - ❌ Unknown `.joyn1` value → NA in output (defensive)
- **Tests**: Existing `test-joyn_workhorse.R` covers this. Add explicit
  assertion that `.joyn1` column is absent from output.
- **Acceptance criteria**: All `test-joyn_workhorse.R` and `test-joyn.R`
  tests pass; no `.joyn1` leak.

### 4. Simplify `check_var_class()`
- **Requirements**: R3, R9
- **Files**: `R/checks.R`
- **Details**:
  - Replace the `vapply()` + inner `vapply(allowed_classes, inherits, ...)`
    pattern with a set-based check:
    ```r
    ok <- any(class(value) %in% allowed_classes)
    ```
    This handles S3 classes correctly (a `POSIXct` object has class
    `c("POSIXct", "POSIXt")` — `%in%` catches it).
  - Keep the allowed list as a package-level constant in `aaa.R` instead of
    re-creating it per call.
- **Test Scenarios**:
  - ✅ Character, integer, numeric, factor, logical, Date, POSIXct, fs_path
    all pass
  - 🛑 Multi-class objects (POSIXct) pass
  - ❌ Unsupported class (e.g., `raw`) triggers warning
- **Tests**: Add `test-checks.R` cases for each allowed class and one
  unsupported class.
- **Acceptance criteria**: All `test-checks.R` pass; `check_var_class` handles
  all declared types.

### 5. Drop `glue` dependency
- **Requirements**: R4, R9
- **Files**: `R/checks.R`, `DESCRIPTION`
- **Details**:
  - Replace the two `glue::glue(...)` calls in `check_var_class()` with
    `cli::format_inline()` or direct `paste0()`.
  - Remove `glue` from `Imports` in `DESCRIPTION`.
  - Verify no other file uses `glue::`.
- **Test Scenarios**:
  - ✅ Warning messages for unsupported class still render correctly
  - 🛑 `R CMD check` passes with no missing-import notes
  - ❌ Any residual `glue::` call → build error (caught by check)
- **Tests**: Existing tests cover the warning path.
- **Acceptance criteria**: `glue` removed from DESCRIPTION Imports;
  `R CMD check` clean; all tests pass.

### 6. Harden `clear_joynenv()` — all 7 call sites (revised per P2.1)
- **Requirements**: R5, R9
- **Files**: `R/info_display.R`
- **Details**:
  - The plan review identified **7 call sites** (not 5). All must be updated:
    1. `R/dplyr-joins.R` line ~59: `left_join()`
    2. `R/dplyr-joins.R` line ~219: `right_join()`
    3. `R/dplyr-joins.R` line ~382: `full_join()`
    4. `R/dplyr-joins.R` line ~541: `inner_join()`
    5. `R/dplyr-joins.R` line ~702: `anti_join()`
    6. `R/joyn-merge.R`: the primary `joyn()` function
    7. `R/merge-data.table.R`: the `merge.data.table` wrapper
  - Replace the `sys.call(-1)` / `first_source == "joyn"` pattern with a
    **session flag** approach:
    - `joyn()` and all 6 wrapper functions set `.joynenv$joyn_session <- TRUE`
      at the top of their body (before any other call).
    - `clear_joynenv()` checks for `joyn_session` flag to decide whether
      to clear. On first call: clears and sets the flag. On re-entry from
      a nested call (e.g., `left_join()` → `joyn()`): detects the flag is
      already set, skips clearing.
    - On `joyn()` exit (via `on.exit`): unbind `joyn_session`.
  - This eliminates `sys.call(-1)` entirely while correctly handling nesting.
- **Test Scenarios**:
  - ✅ `clear_joynenv()` clears when called from each of the 7 entry points
  - ✅ Nested call (`left_join()` → `joyn()`) does not double-clear
  - 🛑 `merge.data.table` wrapper correctly resets session flag
  - ❌ Direct user call to `clear_joynenv()` does not error
- **Tests**: Add 7 targeted tests in `test-info_display.R`, one per entry point.
- **Acceptance criteria**: No `sys.call(-1)` in the codebase; all 7 call
  sites updated; all existing and new tests pass.

### 7. ~~Harden `store_joyn_msg` scope resolution~~ — MERGED INTO STEP 2

> **Removed per review finding P1.1 + P2.2**: This step was merged into Step 2.
> The design decision: `parent.frame(1)` is retained in `store_joyn_msg()` because
> removing it requires updating 20+ call sites with high silent-regression risk.
> The performance gain from the list accumulator (Step 2) is captured without
> touching the interpolation mechanism. If a future session decides to remove
> `parent.frame(1)`, it must be done as a standalone fully-audited refactor with
> a checklist of all interpolated call sites.

## Phase 3: Test Coverage

### 8. ~~Clean up dead code in `prep_m_to_m_data.R`~~ — MOVED TO PHASE 4

> **Moved per review finding P3.2**: This step has no relation to test coverage.
> The file header `# this needs work` is a WIP marker suggesting intentional
> preservation for future m:m expansion. Moved to Phase 4 (Documentation/Validation)
> where it requires explicit author confirmation before deletion.

### 9. Add edge-case tests for type coercion in joins
- **Requirements**: R7, R9
- **Files**: create or extend `tests/testthat/test-type-coercion.R`
- **Details**:
  - Test joining on keys with mixed types:
    - integer x ↔ numeric y (e.g., `1L` vs `1.0`)
    - character x ↔ factor y
    - Date x ↔ POSIXct y
    - numeric x ↔ character y (should warn/error)
  - Test with NA keys in both tables
  - Test with all-NA key column
  - Test multi-key join where one key is character and another is integer
- **Test Scenarios**:
  - ✅ Compatible types join correctly
  - 🛑 Partially compatible types (integer/numeric) join with correct coercion
  - ❌ Incompatible types (numeric/character) produce clear error or warning
- **Acceptance criteria**: At least 10 new test cases covering type edge cases.

### 10. Add integration tests for `joyn-merge.R`
- **Requirements**: R7, R9
- **Files**: create `tests/testthat/test-joyn-merge.R`
- **Details**:
  - Dedicated tests for the `joyn()` function covering:
    - All `keep` options: "full", "left", "right", "inner", "anti"
    - All `match_type` options: "1:1", "1:m", "m:1", "m:m"
    - `update_NAs` and `update_values` interaction
    - `reporttype` options: "factor", "character", "numeric"
    - `keep_common_vars` TRUE/FALSE
    - `y_vars_to_keep` with TRUE, FALSE, NULL, and specific variable names
    - Multi-key joins with `by` expressions (e.g., `c("a = b", "z")`)
    - Zero-row input tables
    - Tables with no common variables (error path)
    - `sort` = TRUE/FALSE
  - These may overlap with `test-joyn.R` but should be organized by feature.
- **Test Scenarios**:
  - ✅ Each keep × match_type combination produces correct output
  - 🛑 Deprecated arguments still work with lifecycle warnings
  - ❌ Invalid inputs produce clear errors
- **Acceptance criteria**: Full coverage of `joyn()` parameter space.

## Phase 4: Documentation and Validation

### 11. Resolve `prep_m_to_m_data.R` dead code (moved from Phase 3, per P3.2)
- **Requirements**: R7
- **Files**: `R/prep_m_to_m_data.R`
- **Details**:
  - The file is 147 lines of entirely commented-out code. The header `# this needs work`
    suggests intentional WIP preservation.
  - **Requires explicit author confirmation** before deletion. Ask: "Is
    `prep_m_to_m_data.R` intentional WIP for future m:m expansion, or safe to remove?"
  - If **safe to remove**: delete the file; verify `devtools::check()` still clean.
  - If **keep**: add a `# Status: WIP — not yet implemented` header and a
    one-line comment explaining the planned purpose.
- **Test Scenarios**:
  - ✅ Package builds and `devtools::check()` clean after decision
- **Acceptance criteria**: No ambiguous dead code; file either removed with
  author confirmation or clearly marked as intentional WIP.

### 12. Fix documentation typos
- **Requirements**: R8
- **Files**: `R/checks.R`, `R/info_display.R`, and any others found
- **Details**:
  - Known typos:
    - `R/checks.R`: "argumet" → "argument" (appears at least twice)
    - Scan all roxygen comments for common misspellings
  - Run `devtools::spell_check()` if available.
- **Test Scenarios**:
  - ✅ `R CMD check` produces no roxygen warnings
- **Acceptance criteria**: Zero typos in user-facing documentation.

### 12. Run post-optimization benchmarks
- **Requirements**: R6
- **Files**: `inst/benchmarks/bench_joyn.R`
- **Details**:
  - Re-run the benchmark script from Step 1 after all optimizations.
  - Compare before/after timings.
  - Document improvement percentages.
- **Test Scenarios**:
  - ✅ All benchmarks complete successfully
  - ✅ No performance regression in any category
- **Acceptance criteria**: Measurable improvement in messaging-heavy
  operations; no regressions.

### 13. Full test suite validation
- **Requirements**: R9
- **Files**: all test files
- **Details**:
  - Run `devtools::test()` — all tests must pass.
  - Run `devtools::check()` — no ERRORs or WARNINGs.
  - Verify code coverage hasn't decreased.
- **Acceptance criteria**: Clean `devtools::check()` with 0 errors,
  0 warnings, and ideally 0 notes (excluding CRAN-specific notes).

## Testing Strategy

- **Unit tests**: Each optimization step gets targeted tests verifying
  correctness is preserved.
- **Integration tests**: New `test-joyn-merge.R` covers the full parameter
  matrix of `joyn()`.
- **Edge-case tests**: New `test-type-coercion.R` covers type handling.
- **Performance tests**: Benchmark script in `inst/benchmarks/` validates
  no regressions and measures gains.
- **Regression gate**: Full `devtools::test()` after every step.

## Documentation Checklist
- [ ] Fix roxygen typos in `checks.R` and `info_display.R`
- [ ] Update `DESCRIPTION` to remove `glue` from Imports
- [ ] Ensure all modified functions have accurate roxygen docs
- [ ] No README changes needed (API unchanged)

## Risks & Mitigations

| Risk | Impact | Mitigation |
|------|--------|------------|
| Messaging refactor breaks message dedup | Messages appear duplicated | Keep `funique()` inside `flush_joyn_msgs()`; test with known-duplicate scenario |
| Mid-join `joyn_msg("err")` shows empty messages | User sees error with no context | `joyn_msg()` always flushes before reading — tested explicitly |
| `clear_joynenv()` change misses one of 7 call sites | Session flag not cleared; stale messages persist | Enumerate all 7 call sites explicitly; test each one |
| Removing `glue` breaks NAMESPACE | Build failure | Run `devtools::check()` immediately after removal |
| `parent.frame()` kept but accumulator changes semantics | Messages resolved at wrong frame depth | Keep `parent.frame(1)` unchanged; only change accumulation |
| Benchmark results not reproducible | Can't validate improvements | Run benchmarks 3 times; report median |
| `prep_m_to_m_data.R` deleted without author confirmation | Lost future WIP | Explicit confirmation required before deletion (Step 11) |

## Out of Scope

- New join types or new exported functions
- Vignette rewrites (separate session)
- CRAN submission (separate session)
- Architecture-level change to avoid full join (Approach 2 from brainstorm)
- Changing `collapse` dependency
- Modifying user-facing API (function signatures, parameter order)
