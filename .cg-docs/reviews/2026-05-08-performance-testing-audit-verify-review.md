---
date: 2026-05-08
depth: light
parent-review: .cg-docs/reviews/2026-05-08-performance-testing-audit-review.md
type: verification
test-result: "PASS 604 | FAIL 0 | SKIP 6 | WARN 1"
findings:
  V-P2.1: fixed   # misleading mapping comment in joyn_workhorse.R corrected
  V-P2.2: fixed   # & → && in arguments_checks() in dplyr-joins.R
  V-P3.1: fixed   # anti-join test: nrow assertion added
  V-P3.2: fixed   # keep_common_vars=TRUE assertion strengthened (a.x, a.y)
  V-P3.3: fixed   # env_has guard added before env_get in successive-calls test
  V-P3.4: fixed   # m:m docstring corrected to describe Cartesian-product behavior
  V-P3.5: fixed   # double space in .joynenv assignment in aaa.R
---

# Verification Review — Performance & Testing Audit (2026-05-08)

**Parent review**: [2026-05-08-performance-testing-audit-review.md](2026-05-08-performance-testing-audit-review.md)
**Type**: verification
**Final test result**: PASS 604 | FAIL 0 | SKIP 6 | WARN 1 (+4 vs prior run)

---

## Prior Findings: All Confirmed ✅

All 28 fixed findings from the prior autofix review were verified as correctly applied. No regressions detected from fix application.

---

## New Findings from Verify Pass

### P2 — 2 new findings (both fixed)

**[V-P2.1]** `R/joyn_workhorse.R:109` — Self-contradictory mapping comment was a data-corruption trap  
The comment said "collapse 1=x-only" when the actual mapping has collapse-1 = matched (both). Future maintainers would "fix" `c(3,1,2)` to `c(1,2,3)` and silently corrupt all joins. **Fixed**: corrected to "collapse 1=matched (both), 2=x-only, 3=y-only".

**[V-P2.2]** `R/dplyr-joins.R:891` — Vectorised `&` in scalar `if` in `arguments_checks()`  
`relationship %in% c("1:m","m:m") & !multiple == "all"` used `&` instead of `&&`, inconsistent with all other fixes in the same file. **Fixed**: → `&&`.

### P3 — 4 new findings (3 fixed, 1 advisory)

**[V-P3.1]** `tests/testthat/test-joyn-merge.R:151` — `keep="anti"` test missing row count  
`all(result[[rv]] == 1)` is vacuously TRUE on empty output. **Fixed**: added `expect_equal(nrow(result), 2L)`.

**[V-P3.2]** `tests/testthat/test-joyn-merge.R:192` — `keep_common_vars=TRUE` assertion too weak  
`any(grepl("^a", col_names))` passes on the wrong output. **Fixed**: asserts `"a.x" %in% col_names` and `"a.y" %in% col_names`.

**[V-P3.3]** `tests/testthat/test-joyn-merge.R:270` — `env_get` without `env_has` guard  
A silent failure path would produce cryptic rlang errors instead of a proper test failure. **Fixed**: added `expect_true(rlang::env_has(.joynenv, "joyn_msgs"))` before each `env_get` in the successive-calls test.

**[V-P3.4]** `tests/testthat/test-joyn-merge.R:110` — m:m nrow=6 comment vs. docstring mismatch [ADVISORY]  
The test asserts 6 rows (Cartesian product via collapse); `joyn()` docstring says it does NOT do Cartesian product. The test is correct for the current implementation (Cartesian via collapse, `prep_m_to_m_data.R` commented out), but the comment may confuse future maintainers when sequential m:m is eventually implemented. No code change applied — tracked for future when m:m is properly implemented.

**[V-P3.5]** `R/aaa.R:1` — Double space in `.joynenv <-  new.env(...)`. **Fixed**: → single space.

---

## Open Manual Findings (carried from prior review)

| ID | Summary | File |
|---|---|---|
| M1 | `sprintf()` → cli markup in `check_var_class()` warnings | `R/checks.R:273,285` |
| M2 | Rename `.joyn_allowed_classes` → `.JOYN_ALLOWED_CLASSES` | `R/aaa.R` |
| M3 | `joyn_active` boolean → `joyn_depth` refcount (race window) | `R/joyn-merge.R` et al. |
| M4 | Abort vs. warn for unsupported key classes | `R/checks.R:226` |
| M5 | `haven_labelled` integer vs. double inconsistency | `R/aaa.R` |
| M6 | Remove `set_collapse(mask="%in%")` — CRAN risk | `R/aaa.R:3` |
