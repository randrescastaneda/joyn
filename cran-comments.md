## R CMD check results

0 errors | 0 warnings | 0 notes

## Test environments

- Local macOS (aarch64), R 4.5.1: 0 errors | 0 warnings | 0 notes
- GitHub Actions ubuntu-latest, macOS-latest, windows-latest: all pass
- win-builder (r-devel): pending

## Summary of changes in 0.3.0

- Minimum R version raised to 4.2.0 (uses native pipe `|>` and pipe placeholder).
- Zero-row input handling: `check_xy()` now warns (not errors) for zero-row tables.
- New internal helpers: `check_var_class()` for join-variable validation,
  `filter_vars()` for variable inclusion/exclusion in `possible_ids()`.
- Improved `by`-expression handling; original column names preserved where possible.
- Expanded test suite and documentation updates.

## Reverse dependencies

joyn has no reverse dependencies on CRAN.
