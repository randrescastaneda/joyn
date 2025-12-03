# Merge two data frames

This is a joyn wrapper that works in a similar fashion to
[base::merge](https://rdrr.io/r/base/merge.html) and
[data.table::merge](https://rdatatable.gitlab.io/data.table/reference/merge.html),
which is why merge masks the other two.

## Usage

``` r
merge(
  x,
  y,
  by = NULL,
  by.x = NULL,
  by.y = NULL,
  all = FALSE,
  all.x = all,
  all.y = all,
  sort = TRUE,
  suffixes = c(".x", ".y"),
  no.dups = TRUE,
  allow.cartesian = getOption("datatable.allow.cartesian"),
  match_type = c("m:m", "m:1", "1:m", "1:1"),
  keep_common_vars = TRUE,
  ...
)
```

## Arguments

- x, y:

  `data table`s. `y` is coerced to a `data.table` if it isn't one
  already.

- by:

  A vector of shared column names in `x` and `y` to merge on. This
  defaults to the shared key columns between the two tables. If `y` has
  no key columns, this defaults to the key of `x`.

- by.x, by.y:

  Vectors of column names in `x` and `y` to merge on.

- all:

  logical; `all = TRUE` is shorthand to save setting both `all.x = TRUE`
  and `all.y = TRUE`.

- all.x:

  logical; if `TRUE`, rows from `x` which have no matching row in `y`
  are included. These rows will have 'NA's in the columns that are
  usually filled with values from `y`. The default is `FALSE` so that
  only rows with data from both `x` and `y` are included in the output.

- all.y:

  logical; analogous to `all.x` above.

- sort:

  logical. If `TRUE` (default), the rows of the merged `data.table` are
  sorted by setting the key to the `by / by.x` columns. If `FALSE`,
  unlike base R's `merge` for which row order is unspecified, the row
  order in `x` is retained (including retaining the position of missing
  entries when `all.x=TRUE`), followed by `y` rows that don't match `x`
  (when `all.y=TRUE`) retaining the order those appear in `y`.

- suffixes:

  A `character(2)` specifying the suffixes to be used for making
  non-`by` column names unique. The suffix behaviour works in a similar
  fashion as the [`merge.data.frame`](https://rdrr.io/r/base/merge.html)
  method does.

- no.dups:

  logical indicating that `suffixes` are also appended to non-`by.y`
  column names in `y` when they have the same column name as any `by.x`.

- allow.cartesian:

  See `allow.cartesian` in
  [`[.data.table`](https://rdatatable.gitlab.io/data.table/reference/data.table.html).

- match_type:

  character: one of *"m:m"*, *"m:1"*, *"1:m"*, *"1:1"*. Default is
  *"1:1"* since this the most restrictive. However, following Stata's
  recommendation, it is better to be explicit and use any of the other
  three match types (See details in *match types sections*).

- keep_common_vars:

  logical: If TRUE, it will keep the original variable from y when both
  tables have common variable names. Thus, the prefix "y." will be added
  to the original name to distinguish from the resulting variable in the
  joined table.

- ...:

  Arguments passed on to
  [`joyn`](https://randrescastaneda.github.io/joyn/reference/joyn.md)

  `y_vars_to_keep`

  :   character: Vector of variable names in `y` that will be kept after
      the merge. If TRUE (the default), it keeps all the brings all the
      variables in y into x. If FALSE or NULL, it does not bring any
      variable into x, but a report will be generated.

  `reportvar`

  :   character: Name of reporting variable. Default is ".joyn". This is
      the same as variable "\_merge" in Stata after performing a merge.
      If FALSE or NULL, the reporting variable will be excluded from the
      final table, though a summary of the join will be display after
      concluding.

  `update_NAs`

  :   logical: If TRUE, it will update NA values of all variables in x
      with actual values of variables in y that have the same name as
      the ones in x. If FALSE, NA values won't be updated, even if
      `update_values` is `TRUE`

  `update_values`

  :   logical: If TRUE, it will update all values of variables in x with
      the actual of variables in y with the same name as the ones in x.
      **NAs from y won't be used to update actual values in x**. Yet, by
      default, NAs in x will be updated with values in y. To avoid this,
      make sure to set `update_NAs = FALSE`

  `verbose`

  :   logical: if FALSE, it won't display any message (programmer's
      option). Default is TRUE.

## Value

data.table merging x and y

## Examples

``` r
x1 = data.frame(id = c(1L, 1L, 2L, 3L, NA_integer_),
                t  = c(1L, 2L, 1L, 2L, NA_integer_),
                x  = 11:15)
y1 = data.frame(id = c(1,2, 4),
                y  = c(11L, 15L, 16))
joyn::merge(x1, y1, by = "id")
#> 
#> ── JOYn Report ──
#> 
#>   .joyn n percent
#> 1     x 2   66.7%
#> 2     y 1   33.3%
#> 3 total 3    100%
#> ────────────────────────────────────────────────────────── End of JOYn report ──
#> ℹ Note: Joyn's report available in variable .joyn
#> ℹ Note: Removing key variables id from id and y
#> ⚠ Warning:  Supplied both by and by.x/by.y. by argument will be ignored.
#> ⚠ Warning: The keys supplied uniquely identify y, therefore a m:1 join is
#> executed
#>   id t  x  y .joyn
#> 1  1 1 11 11 x & y
#> 2  1 2 12 11 x & y
#> 3  2 1 13 15 x & y
# example of using by.x and by.y
x2 = data.frame(id1 = c(1, 1, 2, 3, 3),
                id2 = c(1, 1, 2, 3, 4),
                t   = c(1L, 2L, 1L, 2L, NA_integer_),
                x   = c(16, 12, NA, NA, 15))
y2 = data.frame(id  = c(1, 2, 5, 6, 3),
                id2 = c(1, 1, 2, 3, 4),
                y   = c(11L, 15L, 20L, 13L, 10L),
                x   = c(16:20))
jn <- joyn::merge(x2,
            y2,
            match_type = "m:m",
            all.x = TRUE,
            by.x = "id1",
            by.y = "id2")
#> 
#> ── JOYn Report ──
#> 
#>   .joyn n percent
#> 1     y 1   14.3%
#> 2 x & y 6   85.7%
#> 3 total 7    100%
#> ────────────────────────────────────────────────────────── End of JOYn report ──
#> ℹ Note: Joyn's report available in variable .joyn
#> ℹ Note: Removing key variables keyby1 from id, keyby1, y, and x
#> ⚠ Warning:  Supplied both by and by.x/by.y. by argument will be ignored.
# example with all = TRUE
jn <- joyn::merge(x2,
            y2,
            match_type = "m:m",
            by.x = "id1",
            by.y = "id2",
            all = TRUE)
#> 
#> ── JOYn Report ──
#> 
#>   .joyn n percent
#> 1     y 1   12.5%
#> 2 x & y 7   87.5%
#> 3 total 8    100%
#> ────────────────────────────────────────────────────────── End of JOYn report ──
#> ℹ Note: Joyn's report available in variable .joyn
#> ℹ Note: Removing key variables keyby1 from id, keyby1, y, and x
#> ⚠ Warning:  Supplied both by and by.x/by.y. by argument will be ignored.
```
