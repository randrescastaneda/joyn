# Left join two data frames

This is a `joyn` wrapper that works in a similar fashion to
[dplyr::left_join](https://dplyr.tidyverse.org/reference/mutate-joins.html)

## Usage

``` r
left_join(
  x,
  y,
  by = intersect(names(x), names(y)),
  copy = FALSE,
  suffix = c(".x", ".y"),
  keep = NULL,
  na_matches = c("na", "never"),
  multiple = "all",
  unmatched = "drop",
  relationship = NULL,
  y_vars_to_keep = TRUE,
  update_values = FALSE,
  update_NAs = update_values,
  reportvar = getOption("joyn.reportvar"),
  reporttype = c("factor", "character", "numeric"),
  roll = NULL,
  keep_common_vars = FALSE,
  sort = TRUE,
  verbose = getOption("joyn.verbose"),
  ...
)
```

## Arguments

- x:

  data frame: referred to as *left* in R terminology, or *master* in
  Stata terminology.

- y:

  data frame: referred to as *right* in R terminology, or *using* in
  Stata terminology.

- by:

  a character vector of variables to join by. If NULL, the default, joyn
  will do a natural join, using all variables with common names across
  the two tables. A message lists the variables so that you can check
  they're correct (to suppress the message, simply explicitly list the
  variables that you want to join). To join by different variables on x
  and y use a vector of expressions. For example, `by = c("a = b", "z")`
  will use "a" in `x`, "b" in `y`, and "z" in both tables.

- copy:

  If `x` and `y` are not from the same data source, and `copy` is
  `TRUE`, then `y` will be copied into the same src as `x`. This allows
  you to join tables across srcs, but it is a potentially expensive
  operation so you must opt into it.

- suffix:

  If there are non-joined duplicate variables in `x` and `y`, these
  suffixes will be added to the output to disambiguate them. Should be a
  character vector of length 2.

- keep:

  Should the join keys from both `x` and `y` be preserved in the output?

  - If `NULL`, the default, joins on equality retain only the keys from
    `x`, while joins on inequality retain the keys from both inputs.

  - If `TRUE`, all keys from both inputs are retained.

  - If `FALSE`, only keys from `x` are retained. For right and full
    joins, the data in key columns corresponding to rows that only exist
    in `y` are merged into the key columns from `x`. Can't be used when
    joining on inequality conditions.

- na_matches:

  Should two `NA` or two `NaN` values match?

  - `"na"`, the default, treats two `NA` or two `NaN` values as equal,
    like `%in%`, [`match()`](https://rdrr.io/r/base/match.html), and
    [`merge()`](https://randrescastaneda.github.io/joyn/dev/reference/merge.md).

  - `"never"` treats two `NA` or two `NaN` values as different, and will
    never match them together or to any other values. This is similar to
    joins for database sources and to `base::merge(incomparables = NA)`.

- multiple:

  Handling of rows in `x` with multiple matches in `y`. For each row of
  `x`:

  - `"all"`, the default, returns every match detected in `y`. This is
    the same behavior as SQL.

  - `"any"` returns one match detected in `y`, with no guarantees on
    which match will be returned. It is often faster than `"first"` and
    `"last"` if you just need to detect if there is at least one match.

  - `"first"` returns the first match detected in `y`.

  - `"last"` returns the last match detected in `y`.

- unmatched:

  How should unmatched keys that would result in dropped rows be
  handled?

  - `"drop"` drops unmatched keys from the result.

  - `"error"` throws an error if unmatched keys are detected.

  `unmatched` is intended to protect you from accidentally dropping rows
  during a join. It only checks for unmatched keys in the input that
  could potentially drop rows.

  - For left joins, it checks `y`.

  - For right joins, it checks `x`.

  - For inner joins, it checks both `x` and `y`. In this case,
    `unmatched` is also allowed to be a character vector of length 2 to
    specify the behavior for `x` and `y` independently.

- relationship:

  Handling of the expected relationship between the keys of `x` and `y`.
  If the expectations chosen from the list below are invalidated, an
  error is thrown.

  - `NULL`, the default, doesn't expect there to be any relationship
    between `x` and `y`. However, for equality joins it will check for a
    many-to-many relationship (which is typically unexpected) and will
    warn if one occurs, encouraging you to either take a closer look at
    your inputs or make this relationship explicit by specifying
    `"many-to-many"`.

    See the *Many-to-many relationships* section for more details.

  - `"one-to-one"` expects:

    - Each row in `x` matches at most 1 row in `y`.

    - Each row in `y` matches at most 1 row in `x`.

  - `"one-to-many"` expects:

    - Each row in `y` matches at most 1 row in `x`.

  - `"many-to-one"` expects:

    - Each row in `x` matches at most 1 row in `y`.

  - `"many-to-many"` doesn't perform any relationship checks, but is
    provided to allow you to be explicit about this relationship if you
    know it exists.

  `relationship` doesn't handle cases where there are zero matches. For
  that, see `unmatched`.

- y_vars_to_keep:

  character: Vector of variable names in `y` that will be kept after the
  merge. If TRUE (the default), it keeps all the brings all the
  variables in y into x. If FALSE or NULL, it does not bring any
  variable into x, but a report will be generated.

- update_values:

  logical: If TRUE, it will update all values of variables in x with the
  actual of variables in y with the same name as the ones in x. **NAs
  from y won't be used to update actual values in x**. Yet, by default,
  NAs in x will be updated with values in y. To avoid this, make sure to
  set `update_NAs = FALSE`

- update_NAs:

  logical: If TRUE, it will update NA values of all variables in x with
  actual values of variables in y that have the same name as the ones
  in x. If FALSE, NA values won't be updated, even if `update_values` is
  `TRUE`

- reportvar:

  character: Name of reporting variable. Default is ".joyn". This is the
  same as variable "\_merge" in Stata after performing a merge. If FALSE
  or NULL, the reporting variable will be excluded from the final table,
  though a summary of the join will be display after concluding.

- reporttype:

  character: One of *"character"* or *"numeric"*. Default is
  *"character"*. If *"numeric"*, the reporting variable will contain
  numeric codes of the source and the contents of each observation in
  the joined table. See below for more information.

- roll:

  double: *to be implemented*

- keep_common_vars:

  logical: If TRUE, it will keep the original variable from y when both
  tables have common variable names. Thus, the prefix "y." will be added
  to the original name to distinguish from the resulting variable in the
  joined table.

- sort:

  logical: If TRUE, sort by key variables in `by`. Default is FALSE.

- verbose:

  logical: if FALSE, it won't display any message (programmer's option).
  Default is TRUE.

- ...:

  Arguments passed on to
  [`joyn`](https://randrescastaneda.github.io/joyn/dev/reference/joyn.md)

  `match_type`

  :   character: one of *"m:m"*, *"m:1"*, *"1:m"*, *"1:1"*. Default is
      *"1:1"* since this the most restrictive. However, following
      Stata's recommendation, it is better to be explicit and use any of
      the other three match types (See details in *match types
      sections*).

  `allow.cartesian`

  :   logical: Check documentation in official [web
      site](https://rdatatable.gitlab.io/data.table/reference/merge.html/).
      Default is `NULL`, which implies that if the join is "1:1" it will
      be `FALSE`, but if the join has any "m" on it, it will be
      converted to `TRUE`. By specifying `TRUE` of `FALSE` you force the
      behavior of the join.

  `suffixes`

  :   A character(2) specifying the suffixes to be used for making
      non-by column names unique. The suffix behaviour works in a
      similar fashion as the
      [base::merge](https://rdrr.io/r/base/merge.html) method does.

  `yvars`

  :   **\[superseded\]**: use now `y_vars_to_keep`

  `keep_y_in_x`

  :   **\[superseded\]**: use now `keep_common_vars`

  `msg_type`

  :   character: type of messages to display by default

  `na.last`

  :   `logical`. If `TRUE`, missing values in the data are placed last;
      if `FALSE`, they are placed first; if `NA` they are removed.
      `na.last=NA` is valid only for `x[order(., na.last)]` and its
      default is `TRUE`. `setorder` and `setorderv` only accept
      `TRUE`/`FALSE` with default `FALSE`.

## Value

An data frame of the same class as `x`. The properties of the output are
as close as possible to the ones returned by the dplyr alternative.

## See also

Other dplyr alternatives:
[`anti_join()`](https://randrescastaneda.github.io/joyn/dev/reference/anti_join.md),
[`full_join()`](https://randrescastaneda.github.io/joyn/dev/reference/full_join.md),
[`inner_join()`](https://randrescastaneda.github.io/joyn/dev/reference/inner_join.md),
[`right_join()`](https://randrescastaneda.github.io/joyn/dev/reference/right_join.md)

## Examples

``` r
# Simple left join
library(data.table)

x1 = data.table(id = c(1L, 1L, 2L, 3L, NA_integer_),
                t  = c(1L, 2L, 1L, 2L, NA_integer_),
                x  = 11:15)
y1 = data.table(id = c(1,2, 4),
                y  = c(11L, 15L, 16))
left_join(x1, y1, relationship = "many-to-one")
#> 
#> ── JOYn Report ──
#> 
#>   .joyn n percent
#> 1     x 2     40%
#> 2     y 1     20%
#> 3 x & y 2     40%
#> 4 total 5    100%
#> ────────────────────────────────────────────────────────── End of JOYn report ──
#> ℹ Note: Joyn's report available in variable .joyn
#> ℹ Note: Removing key variables id from id and y
#>       id     t     x     y  .joyn
#>    <num> <int> <int> <num> <fctr>
#> 1:     1     1    11    11  x & y
#> 2:     1     2    12    11  x & y
#> 3:     2     1    13    15  x & y
#> 4:     3     2    14    NA      x
#> 5:    NA    NA    15    NA      x
```
