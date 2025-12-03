# joyn (development version)

# joyn 0.3.0

## Breaking Changes
- **Minimum R version:** Now requires R ≥ 4.2.0 (previously R ≥ 2.10.0) due to use of native pipe operator (`|>`) introduced in R 4.1.0 and  uses the pipe
  placeholder syntax added in R 4.2.0. Users on R 4.0.x must upgrade to use this version.
- **Default verbosity:** `possible_ids.verbose` default changed from `TRUE` to `FALSE` to reduce console output by default.

## New Features
- **Variable class validation:** New internal function `check_var_class()` validates that join (`by`) variables have supported classes (character, integer, numeric, factor, logical, Date, POSIXct, fs_path). Warns users when unsupported classes (e.g., list, complex, raw) are detected and suggests coercion.
- **Variable filtering helper:** New `filter_vars()` function simplifies inclusion/exclusion of variables based on names or classes in `possible_ids()` and related functions.
- **Test data helper:** New `make_test_data()` function provides standardized test datasets to improve test maintainability.

## Improvements
- **Zero-row input handling:** `check_xy()` now detects and reports when input tables have zero rows (previously would silently proceed).
- **Enhanced `by` expression support:** Improved handling of `by` expressions (e.g., `"x == y"`). Temporary keys are now created only when necessary, and original column names are preserved when possible, reducing unnecessary data.table modifications.
- **CI/CD updates:** GitHub Actions workflow upgraded from `upload-artifact@v3` to `v4` for better compatibility.
- **Dependencies:** Added `glue` package to Imports for improved string interpolation in messages.

## Bug Fixes
- **Test suite improvements:** Rewrote and expanded test coverage in `test-checks.R` to use helper data and explicit expectations. Added error tests for unsupported `by` variable types and formats.
- **Documentation updates:** Added complete documentation for `check_var_class()`, `filter_vars()`, and other internal helpers. Updated `man/possible_ids.Rd` to reflect new default parameter values.
- **Code formatting:** Minor formatting improvements throughout codebase for consistency.


# joyn 0.2.4

* Improve displaying messages. Now, they are clickable.

* Improve algorithm to find variables that work as possible IDs see `possible_ids()`.

* Improve algorithm in `is_id()` and `freq_table()`. 

# joyn 0.2.3

* Fix bug where the left join did not work when updating values and a full join was maintained.

# joyn 0.2.2

* Remove unmasking functions as we realized that they are not necessary. 

# joyn 0.2.1 (Not in CRAN)


* Add `anti_join()` function.

* Add `unmask_joyn()` function to unmask `joyn` functions that mask `dplyr` equivalents.

* Add information about duplicated obs in `by` variable when match type is `1` rathern than `m`. 

* improve inefficiencies in deep copies with `m:m` joins

* Replace `m:m` joins from `data.table::merge.data.table` to `collapse::join`. Thanks to @SebKrantz for the suggestion (#58).

* Add information about duplicated obs in `by` variable when match type is `1` rather than `m`.

* Internal: improve storing of joyn messages.

* Improve creation of reporting variable. Now, it is created in [collapse::join] rather than in `joyn` function. In addition, the reporting variable is created as factor to improve performance. Thanks to @SebKrantz for the suggestion (#58)

## breaking changes

* Now, by default, `joyn` will not sort the data. This is to avoid unnecessary 
computational time that most of the time is not needed. 
If the user wants to sort the data, they can use the `sort` argument, which triggers 
the sorting mechanism of `collapse` package.

* report variable (named ".join" by default) is now a factor instead of character. Yet, users can still use character if they want with the `reporttype = "character"`.

# joyn 0.2.0

* `joyn` has gained two new authors: Zander Prinsloo and Rossana Tatulli.


## Breaking changes

* Function `joyn::merge()` was replaced by `joyn::joyn()`. This is now the main function of the `joyn` package.

* Arguments `allow.cartesian`, `yvars`, and `keep_y_in_x` have been deprecated. The latter two have been replaced by `y_vars_to_keep` and `keep_common_vars`, respectively. The new argument names bring more clarity about what they arguments do.

## New features

* New function `joyn::merge()` works as a mask for the `base::merge()` or `data.table::merge.data.table()`. `joyn::merge()` has the same features as the previous two, but includes the features of `joyn::joyn()`.

* Messages style have been improved and categorized. See [message vignette](https://randrescastaneda.github.io/joyn/articles/messages.html) for more information.

* New functions to mimic [dplyr joins](https://dplyr.tidyverse.org/reference/mutate-joins.html). The `joyn` variants have all the features for `joyn::joyn()` but lack some of the most advance features of `dplyr` joins like [`joyn::join_by()`](https://dplyr.tidyverse.org/reference/join_by.html)


## Minor improvements and fixes

* Minor inconsistency bugs were fixed.

# joyn 0.1.4
* update_NAs now could be FALSE even if update_values is TRUE

* Select rows-to-keep before transformation of updated values and NAs to avoid keeping rows from y that did not match in x but whose values got updated because `update_values = TRUE`

* Solve issues #1 and #19

* Change to data.table::merge.data.table syntax in all joins. It makes it easier to work with and consistent across different join types. 

* Remove previous lazy-loaded data.


# joyn 0.1.3
* Convert external data to external data.

# joyn 0.1.2

* Add function `possible_ids()` to identify what variables are suitable for 
uniquely identify the database.

## joyn 0.1.1

* Add function `is_id()` to check whether the table is uniquely identified by 
key variables

* Add function `freq_table()` as a substitute for janitor::tabyl. This makes it 
more convenient for users who do not have janitor installed. 

## joyn 0.1.0

Fix bug on `by` argument when using "=" or "==". 

## joyn 0.0.1
First Public release
