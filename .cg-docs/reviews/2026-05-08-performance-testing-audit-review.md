---
plan: .cg-docs/plans/2026-05-08-performance-testing-audit.md
review-date: 2026-05-08
review-mode: autofix
review-depth: standard
agents: [cg-code-quality, cg-testing, cg-documentation, cg-version-control, cg-reproducibility, cg-performance, cg-architecture, cg-data-quality]
test-result: "PASS 600 | FAIL 0 | SKIP 6 | WARN 1"
findings:
  # P1 — Must Fix
  P1.1-dropreport-T: fixed      # dropreport==T → isTRUE(dropreport) ×5 in dplyr-joins.R
  P1.2-roxygen-double-hash: fixed  # ##' → #' in checks.R
  P1.3-flush-return-docs: fixed    # flush_joyn_msgs @return corrected
  P1.4-flush-conversion: fixed     # do.call(rbind,lapply) → vapply ×2 in flush_joyn_msgs
  # P2 — Should Fix (safe_auto applied)
  P2.1-unused-l: fixed         # l <- lapply(...) → lapply(...) in info_display.R:59
  P2.2-scalar-and-merge: fixed # & → && in joyn-merge.R:432
  P2.3-scalar-and-info: fixed  # & → && in info_display.R:62
  P2.4-O1-comment: fixed       # "O(1) per call" → "O(n) per call" comment
  P2.5-tautological-assert: fixed  # class(out_warn)|>expect_equal(class(out_warn)) → expect_equal("data.frame")
  P2.6-side-effects-test: fixed    # deleted top-level collapse::join + fselect() from test-info_display.R
  P2.7-mm-test-weak: fixed     # added row count + value range assertions to m:m test
  P2.8-nesting-warn-weak: fixed  # strengthened copy=TRUE warning assertion in nesting test
  P2.9-invisible-noops: fixed  # invisible() in expect_identical → expect_equal in test-checks.R
  P2.10-merge-flag-test: fixed # added joyn_active flag lifecycle test for merge()
  P2.11-remapping-alloc: fixed # named char vector lookup → unnamed double positional (joyn_workhorse.R)
  P2.12-roworder-flush: fixed  # moved roworder(type) from joyn_msg() to flush_joyn_msgs()
  # P3 — Advisory (safe_auto applied)
  P3.1-joun-typo: fixed        # "joun env" → "joyn env" in merge-data.table.R
  P3.2-hot-typo: fixed         # "Hot to pass" → "How to pass" in info_display.R
  P3.3-step4-comment: fixed    # "(Step 4)" removed from aaa.R comment
  P3.4-fs-path-redundant: fixed  # "fs_path" removed from .joyn_allowed_classes (passes via "character")
  P3.5-keywords-internal: fixed  # @keywords internal added to .joyn_allowed_classes in aaa.R
  P3.6-flush-examples: fixed   # @examples added to flush_joyn_msgs
  P3.7-clear-docs: fixed       # clear_joynenv() description extended with nesting-guard contract
  P3.8-gitignore: fixed        # .Renviron, *.Rcheck/, .DS_Store, Thumbs.db added
  P3.9-rbuildignore: fixed     # inst/benchmarks, roadmap.json, compound-gpid.md added
  P3.10-devtools-guard: fixed  # devtools::load_all() guarded with requireNamespace check
  P3.11-devtools-suggests: fixed  # devtools added to DESCRIPTION Suggests
  P3.12-seed-benchmark: fixed  # set.seed(42) added before rnorm() in bench_joyn.R
  # Manual — Requires human decision
  M1-sprint-vs-cli: open       # sprintf() in checks.R bypasses cli markup theming [P2.2 code-quality]
  M2-constant-naming: open     # .joyn_allowed_classes vs .JOYN_ALLOWED_CLASSES [P3.3 code-quality]
  M3-joyn-depth-refcount: open # boolean joyn_active → integer joyn_depth refcount [P1.1 architecture]
  M4-abort-on-bad-class: open  # abort vs warn for unsupported key classes [P1.1 data-quality]
  M5-haven-labelled: open      # haven_labelled integer vs double inconsistency [P1.2 data-quality]
  M6-set-collapse-mask: open   # set_collapse(mask="%in%") at package load — CRAN risk [P2.2 performance]
---

# Review: Performance & Testing Audit (2026-05-08)

**Plan**: [.cg-docs/plans/2026-05-08-performance-testing-audit.md](.cg-docs/plans/2026-05-08-performance-testing-audit.md)
**Final test result**: PASS 600 | FAIL 0 | SKIP 6 | WARN 1

---

## Applied Fixes Summary

### P1 — Critical (all fixed)

| ID | File | Change |
|---|---|---|
| P1.1 | `R/dplyr-joins.R` | `dropreport == T` → `isTRUE(dropreport)` ×5 |
| P1.2 | `R/checks.R:240` | `##'` → `#'` — roxygen block was silently ignored |
| P1.3 | `R/info_display.R` | `flush_joyn_msgs @return` corrected (FALSE on empty/absent) |
| P1.4 | `R/info_display.R` | `do.call(rbind, lapply(...))` → two `vapply` passes (O(n²) → O(n)) |

### P2 — Important (all safe_auto applied)

| ID | File | Change |
|---|---|---|
| P2.1 | `R/info_display.R:59` | Removed unused `l <-` assignment |
| P2.2 | `R/joyn-merge.R:432` | `&` → `&&` for scalar boolean guard |
| P2.3 | `R/info_display.R:62` | `&` → `&&` for scalar boolean guard |
| P2.4 | `R/info_display.R` | `O(1)` comment corrected to `O(n) due to R copy-on-modify` |
| P2.5 | `tests/testthat/test-info_display.R:44` | Tautological assertion fixed |
| P2.6 | `tests/testthat/test-info_display.R:19-24` | Side-effectful top-level code deleted |
| P2.7 | `tests/testthat/test-joyn-merge.R` | m:m test: added row count + value range assertions |
| P2.8 | `tests/testthat/test-joyn-merge.R` | Nesting copy=TRUE warning: added `grepl("copy", ...)` check |
| P2.9 | `tests/testthat/test-checks.R` | `invisible()` no-ops removed from `expect_identical` calls |
| P2.10 | `tests/testthat/test-merge-data.table.R` | New test: `joyn_active` flag lifecycle for `merge()` |
| P2.11 | `R/joyn_workhorse.R` | Named char vector → unnamed double positional lookup (no `as.character()`) |
| P2.12 | `R/info_display.R` | `roworder(type)` moved from `joyn_msg()` to `flush_joyn_msgs()` |

### P3 — Advisory / Hygiene (safe_auto applied)

| ID | File | Change |
|---|---|---|
| P3.1 | `R/merge-data.table.R:58` | "joun env" → "joyn env" typo |
| P3.2 | `R/info_display.R:131` | "Hot to pass" → "How to pass" roxygen typo |
| P3.3 | `R/aaa.R` | "(Step 4)" stale plan reference removed |
| P3.4 | `R/aaa.R` | `"fs_path"` removed from `.joyn_allowed_classes` (passes via `"character"`) |
| P3.5 | `R/aaa.R` | `@keywords internal` added to `.joyn_allowed_classes` |
| P3.6 | `R/info_display.R` | `@examples` block added to `flush_joyn_msgs` |
| P3.7 | `R/info_display.R` | `clear_joynenv()` description extended with nesting-guard contract |
| P3.8 | `.gitignore` | `.Renviron`, `*.Rcheck/`, `.DS_Store`, `Thumbs.db` added |
| P3.9 | `.Rbuildignore` | `inst/benchmarks`, `roadmap.json`, `compound-gpid.md` excluded |
| P3.10 | `inst/benchmarks/bench_joyn.R` | `devtools::load_all()` guarded with `requireNamespace` check |
| P3.11 | `DESCRIPTION` | `devtools` added to `Suggests` |
| P3.12 | `inst/benchmarks/bench_joyn.R` | `set.seed(42)` added before `rnorm()` calls |

---

## Open Manual Findings

These require a human decision before applying.

### M1 — `sprintf()` bypasses cli markup in `checks.R` [P2.2 code-quality / P2.3 architecture]
**File**: `R/checks.R:273,285`
**Issue**: Two warning messages in `check_var_class()` use `sprintf()` and produce plain strings. All other joyn messages use cli inline markup (`{.strongVar x}`, `{.cls class}`) for consistent highlighting. These two bypass that styling.
**Fix**: Replace `sprintf(...)` with cli interpolation strings, e.g. `"Join variable {.strongVar {v}} has class {.cls {class(value)}} which may cause issues."`.
**Risk**: Low — purely cosmetic change. No logic change.

### M2 — Constant naming: `.joyn_allowed_classes` [P3.3 code-quality]
**File**: `R/aaa.R`, `R/checks.R`
**Issue**: Per R conventions for exported package constants (and `cg-skill-r-shared`), package-wide constants should use `UPPER_SNAKE_CASE`. The constant is currently `.joyn_allowed_classes`; it should be `.JOYN_ALLOWED_CLASSES`.
**Fix**: Rename symbol and update all references in `checks.R`.
**Risk**: Low — purely internal, not exported. Rename with `vscode_renameSymbol`.

### M3 — `joyn_active` boolean → `joyn_depth` refcount [P1.1 architecture]
**File**: `R/joyn-merge.R`, `R/dplyr-joins.R`, `R/merge-data.table.R`, `R/info_display.R`
**Issue**: The boolean `joyn_active` flag loses ownership when the inner `joyn()` call exits and unbinds it. If future code in a dplyr wrapper calls `clear_joynenv()` after `joyn()` returns, the guard is gone. An integer `joyn_depth` refcount would be safe at all nesting levels.
**Fix**: Replace `env_poke("joyn_active", TRUE)` + `env_unbind("joyn_active")` with increment/decrement of `joyn_depth`. Update `clear_joynenv()` to check `joyn_depth > 0`.
**Risk**: Medium — touches 7 entry points. Requires careful coordination.

### M4 — Abort vs. warn for unsupported key classes [P1.1 data-quality]
**File**: `R/checks.R:226`
**Issue**: The `cli::cli_abort()` call for non-NULL bad key classes is commented out. Joins on unsupported types (list, complex, raw) proceed silently with a stored warning. If `verbose = FALSE`, users see nothing and get potentially wrong output.
**Fix option A**: Restore the `cli::cli_abort()` — strict mode.
**Fix option B**: Use immediate `cli::cli_warn()` instead of stored `store_joyn_msg(warn=...)` so the warning always prints regardless of `verbose`.
**Decision needed**: What is the intended behavior for unsupported key types?

### M5 — `haven_labelled` double vs. integer inconsistency [P1.2 data-quality]
**File**: `R/aaa.R`, `R/checks.R`
**Issue**: `haven::labelled()` objects backed by integers silently pass key-class validation (via `"integer"` in `.joyn_allowed_classes`), while double-backed `haven_labelled` columns trigger a stored warning. Both are semantically equivalent labelled variables.
**Decision needed**: Should `haven_labelled` be explicitly allowed (add to constant) or explicitly rejected (with a clear coercion message)?

### M6 — `set_collapse(mask = "%in%")` at package load [P2.2 performance]
**File**: `R/aaa.R:3`
**Issue**: Per `cg-skill-r-collapse`, `set_collapse(mask=...)` should never be used. It modifies the global `collapse.mask` option at package load time, affecting all packages in the user's session. There is no reset in `.onUnload()` in `zzz.R`. CRAN may flag this.
**Fix**: Remove line 3. Audit all `%in%` uses — all are on short vectors where base `%in%` is fine. If collapse hash lookup is specifically needed, use `fmatch()` explicitly.
**Risk**: Low — all current `%in%` uses are on small vectors (≤ 8 elements on one side).

---

## Regression Note

During fix application, two test regressions were introduced and immediately corrected:

1. **Mapping integer type**: Changed `c(3L, 1L, 2L)` back to `c(3, 1, 2)` (double) — the `reporttype = "numeric"` contract requires the report column to be `class == "numeric"` (double), not integer.

2. **m:m test assertion**: The test was missing `reporttype = "numeric"` — without it, the default report type is "character" (`"x"`, `"y"`, `"x & y"`), so `%in% c(1L, 2L, 3L)` always fails. Added `reporttype = "numeric"` to the test.

Both regressions were caught by the test suite before commit.
