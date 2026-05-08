---
project-name: "joyn"
team: "DECDG / GPID -- World Bank"
created: "2026-05-07"
last-reviewed: "2026-05-07"
---

# joyn

## Objective

Tool for diagnosing table joins that combines the speed of `collapse` and
`data.table` with the flexibility of `dplyr` and diagnosis features inspired
by the `merge` command in Stata. `joyn` empowers you to assess the results of
joining data frames, making it easier and more efficient to combine your tables
by offering intuitive join handling, informative reports that identify duplicate
observations and missing values, and comprehensive validation to prevent errors.

## Key Deliverables

- Core functions: `joyn()`, `full_join()`, `left_join()`, `right_join()`, `inner_join()`, `anti_join()`
- Diagnosis & reporting tools: `joyn_report()`, `joyn_msg()`, `is_balanced()`, `is_id()`, `freq_table()`
- Utility functions: `merge()` wrapper (base R / data.table syntax), `possible_ids()`, `rename_to_valid()`
- Join validation: automatic detection of duplicate observations, missing values, unmatched keys, relationship type validation
- Flexible variable handling: `update_values`, `update_NA`, `keep_common_vars`, `y_vars_to_keep` arguments
- Multiple join interfaces: base R merge syntax, data.table native pipe, dplyr verb-based syntax
- Documentation: 6 vignettes (main-functionalities, dplyr-joins, merge-wrapper, aux-functions, adv-functionalities, messages)
- Installation: available on CRAN (`install.packages("joyn")`)

## Constraints

- Minimum R version: R ≥ 4.2.0 (native pipe `|>` and pipe placeholder syntax)
- Opinionated design: intentionally restricts certain actions to prevent inaccurate joins; provides clear error messages for unexpected data configurations
- Relationship awareness required: users must specify correct relationship using `by` arguments for accurate results
- Supported join variable classes: character, integer, numeric, factor, logical, Date, POSIXct, fs_path; warns for unsupported types
- Zero-row input handling: detects and reports when input tables have zero rows

## Current Focus

We are working on improving the efficiency and accuracy of the whole package. We need to make sure that all the edge cases are covered, and we also need to make sure that the package is as fast as possible. In addition, we could improve a little bit more the documentation if needed, and add more unit tests.
