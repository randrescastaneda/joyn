---
title: "CRAN submission notes — joyn 0.3.0"
---

Package: joyn
Version: 0.3.0

Submitted on: (dev) branch — DEV

Maintainer: R. Andres Castañeda <acastanedaa@worldbank.org>

Short description
---------------
This release contains bug fixes, improved input validation, and small API improvements
that make joins with zero-row inputs non-fatal (they now produce a warning) and improve
`by`-expression handling. See NEWS.md for full details.

R version
---------
Requires: R (>= 4.2.0)

Summary of changes in 0.3.0
--------------------------
- Changed minimum R to 4.2.0 (native pipe `|>` and  placeholder are used).
- Input validation: functions now warn (not error) for zero-row tables so valid join
  operations (e.g., left join with empty rhs) proceed with a warning.
- Improved handling of complex `by` expressions and variable-class validation.
- Tests updated: expanded unit tests and fixed cases where zero-column vs zero-row
  dataframes were confused.

Check results performed locally and on CI
----------------------------------------
- `devtools::check()` (local, Windows R 4.5.1): 0 errors | 0 warnings | 0 notes
- GitHub Actions (ubuntu-latest, macOS-latest, windows-latest): all workflows pass
  (no errors/warnings/notes for the package checks run in CI).

Reverse dependency checks
-------------------------
No reverse-dependencies were checked for this release.

Known issues & decisions
------------------------
- The package references `data.table` documentation in a few Rd files. During
  CRAN incoming checks this previously generated a NOTE due to an external URL
  failing SSL validation in some environments. We have reviewed the references
  and kept only stable links; this NOTE no longer appears in current checks.

Additional notes for CRAN
-------------------------
- Test suite: 151 tests (all pass in local and CI runs used for this submission).
- Vignettes and examples: All examples run during package checks; vignettes built
  successfully in the devtools build used for testing.

Contact
-------
If you need additional information please contact the maintainer:

R. Andres Castañeda <acastanedaa@worldbank.org>

Signed-off-by: DEV automation
