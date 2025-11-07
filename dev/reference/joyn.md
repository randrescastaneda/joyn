# Join two tables

This is the primary function in the `joyn` package. It executes a full
join, performs a number of checks, and filters to allow the
user-specified join.

## Usage

``` r
joyn(
  x,
  y,
  by = intersect(names(x), names(y)),
  match_type = c("1:1", "1:m", "m:1", "m:m"),
  keep = c("full", "left", "master", "right", "using", "inner", "anti"),
  y_vars_to_keep = ifelse(keep == "anti", FALSE, TRUE),
  update_values = FALSE,
  update_NAs = update_values,
  reportvar = getOption("joyn.reportvar"),
  reporttype = c("factor", "character", "numeric"),
  roll = NULL,
  keep_common_vars = FALSE,
  sort = FALSE,
  verbose = getOption("joyn.verbose"),
  suffixes = getOption("joyn.suffixes"),
  allow.cartesian = deprecated(),
  yvars = deprecated(),
  keep_y_in_x = deprecated(),
  na.last = getOption("joyn.na.last"),
  msg_type = getOption("joyn.msg_type")
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

- match_type:

  character: one of *"m:m"*, *"m:1"*, *"1:m"*, *"1:1"*. Default is
  *"1:1"* since this the most restrictive. However, following Stata's
  recommendation, it is better to be explicit and use any of the other
  three match types (See details in *match types sections*).

- keep:

  atomic character vector of length 1: One of *"full"*, *"left"*,
  *"master"*, *"right"*, *"using"*, *"inner"*. Default is *"full"*. Even
  though this is not the regular behavior of joins in R, the objective
  of `joyn` is to present a diagnosis of the join which requires a full
  join. That is why the default is a a full join. Yet, if *"left"* or
  *"master"*, it keeps the observations that matched in both tables and
  the ones that did not match in x. The ones in y will be discarded. If
  *"right"* or *"using"*, it keeps the observations that matched in both
  tables and the ones that did not match in y. The ones in x will be
  discarded. If *"inner"*, it only keeps the observations that matched
  both tables. Note that if, for example, a
  `keep = "left", the `joyn()`function still executes a full join under the hood and then filters so that only rows the output table is a left join. This behaviour, while inefficient, allows all the diagnostics and checks conducted by`joyn\`.

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

- suffixes:

  A character(2) specifying the suffixes to be used for making non-by
  column names unique. The suffix behaviour works in a similar fashion
  as the [base::merge](https://rdrr.io/r/base/merge.html) method does.

- allow.cartesian:

  logical: Check documentation in official [web
  site](https://rdatatable.gitlab.io/data.table/reference/merge.html/).
  Default is `NULL`, which implies that if the join is "1:1" it will be
  `FALSE`, but if the join has any "m" on it, it will be converted to
  `TRUE`. By specifying `TRUE` of `FALSE` you force the behavior of the
  join.

- yvars:

  **\[superseded\]**: use now `y_vars_to_keep`

- keep_y_in_x:

  **\[superseded\]**: use now `keep_common_vars`

- na.last:

  `logical`. If `TRUE`, missing values in the data are placed last; if
  `FALSE`, they are placed first; if `NA` they are removed. `na.last=NA`
  is valid only for `x[order(., na.last)]` and its default is `TRUE`.
  `setorder` and `setorderv` only accept `TRUE`/`FALSE` with default
  `FALSE`.

- msg_type:

  character: type of messages to display by default

## Value

a data.table joining x and y.

## match types

Using the same wording of the [Stata
manual](https://www.stata.com/manuals/dmerge.pdf)

**1:1**: specifies a one-to-one match merge. The variables specified in
`by` uniquely identify single observations in both table.

**1:m and m:1**: specify *one-to-many* and *many-to-one* match merges,
respectively. This means that in of the tables the observations are
uniquely identify by the variables in `by`, while in the other table
many (two or more) of the observations are identify by the variables in
`by`

**m:m** refers to *many-to-many merge*. variables in `by` does not
uniquely identify the observations in either table. Matching is
performed by combining observations with equal values in `by`; within
matching values, the first observation in the master (i.e. left or x)
table is matched with the first matching observation in the using (i.e.
right or y) table; the second, with the second; and so on. If there is
an unequal number of observations within a group, then the last
observation of the shorter group is used repeatedly to match with
subsequent observations of the longer group.

## reporttype

If `reporttype = "numeric"`, then the numeric values have the following
meaning:

1: row comes from `x`, i.e. "x" 2: row comes from `y`, i.e. "y" 3: row
from both `x` and `y`, i.e. "x & y" 4: row has NA in `x` that has been
updated with `y`, i.e. "NA updated" 5: row has valued in `x` that has
been updated with `y`, i.e. "value updated" 6: row from `x` that has not
been updated, i.e. "not updated"

## NAs order

`NA`s are placed either at first or at last in the resulting data.frame
depending on the value of `getOption("joyn.na.last")`. The Default is
`FALSE` as it is the default value of
[data.table::setorderv](https://rdatatable.gitlab.io/data.table/reference/setorder.html).

## Examples

``` r
# Simple join
library(data.table)
x1 = data.table(id = c(1L, 1L, 2L, 3L, NA_integer_),
t  = c(1L, 2L, 1L, 2L, NA_integer_),
x  = 11:15)

y1 = data.table(id = 1:2,
                y  = c(11L, 15L))

x2 = data.table(id = c(1, 1, 2, 3, NA),
                t  = c(1L, 2L, 1L, 2L, NA_integer_),
                x  = c(16, 12, NA, NA, 15))

y2 = data.table(id = c(1, 2, 5, 6, 3),
              yd = c(1, 2, 5, 6, 3),
              y  = c(11L, 15L, 20L, 13L, 10L),
              x  = c(16:20))
joyn(x1, y1, match_type = "m:1")
#> 
#> ── JOYn Report ──
#> 
#>   .joyn n percent
#> 1     x 2     40%
#> 2 x & y 3     60%
#> 3 total 5    100%
#> ────────────────────────────────────────────────────────── End of JOYn report ──
#> ℹ Note: Joyn's report available in variable .joyn
#> ℹ Note: Removing key variables id from id and y
#>       id     t     x     y  .joyn
#>    <int> <int> <int> <int> <fctr>
#> 1:     1     1    11    11  x & y
#> 2:     1     2    12    11  x & y
#> 3:     2     1    13    15  x & y
#> 4:     3     2    14    NA      x
#> 5:    NA    NA    15    NA      x

# Bad merge for not specifying by argument or match_type
joyn(x2, y2)
#> 
#> ── JOYn Report ──
#> 
#>   .joyn n percent
#> 1     x 4   44.4%
#> 2     y 4   44.4%
#> 3 x & y 1   11.1%
#> 4 total 9    100%
#> ────────────────────────────────────────────────────────── End of JOYn report ──
#> ℹ Note: Joyn's report available in variable .joyn
#> ℹ Note: Removing key variables id and x from id, yd, y, and x
#>       id     t     x    yd     y  .joyn
#>    <num> <int> <num> <num> <int> <fctr>
#> 1:     1     1    16     1    11  x & y
#> 2:     1     2    12    NA    NA      x
#> 3:     2     1    NA    NA    NA      x
#> 4:     3     2    NA    NA    NA      x
#> 5:    NA    NA    15    NA    NA      x
#> 6:     2    NA    17     2    15      y
#> 7:     5    NA    18     5    20      y
#> 8:     6    NA    19     6    13      y
#> 9:     3    NA    20     3    10      y

# good merge, ignoring variable x from y
joyn(x2, y2, by = "id", match_type = "m:1")
#> 
#> ── JOYn Report ──
#> 
#>   .joyn n percent
#> 1     x 1   14.3%
#> 2     y 2   28.6%
#> 3 x & y 4   57.1%
#> 4 total 7    100%
#> ────────────────────────────────────────────────────────── End of JOYn report ──
#> ℹ Note: Joyn's report available in variable .joyn
#> ℹ Note: Removing key variables id from id, yd, y, and x
#>       id     t     x    yd     y  .joyn
#>    <num> <int> <num> <num> <int> <fctr>
#> 1:     1     1    16     1    11  x & y
#> 2:     1     2    12     1    11  x & y
#> 3:     2     1    NA     2    15  x & y
#> 4:     3     2    NA     3    10  x & y
#> 5:    NA    NA    15    NA    NA      x
#> 6:     5    NA    NA     5    20      y
#> 7:     6    NA    NA     6    13      y

# update NAs in x variable form x
joyn(x2, y2, by = "id", update_NAs = TRUE, match_type = "m:1")
#> 
#> ── JOYn Report ──
#> 
#>         .joyn     n percent
#>        <char> <int>  <char>
#> 1:          x     1   14.3%
#> 2:      x & y     2   28.6%
#> 3: NA updated     4   57.1%
#> 4:      total     7    100%
#> ────────────────────────────────────────────────────────── End of JOYn report ──
#> ℹ Note: Joyn's report available in variable .joyn
#> ℹ Note: Removing key variables id from id, yd, y, and x
#>       id     t     x    yd     y      .joyn
#>    <num> <int> <num> <num> <int>     <fctr>
#> 1:     1     1    16     1    11      x & y
#> 2:     1     2    12     1    11      x & y
#> 3:     2     1    17     2    15 NA updated
#> 4:     3     2    20     3    10 NA updated
#> 5:    NA    NA    15    NA    NA          x
#> 6:     5    NA    18     5    20 NA updated
#> 7:     6    NA    19     6    13 NA updated

# Update values in x with variables from y
joyn(x2, y2, by = "id", update_values = TRUE, match_type = "m:1")
#> 
#> ── JOYn Report ──
#> 
#>            .joyn     n percent
#>           <char> <int>  <char>
#> 1:    NA updated     4   57.1%
#> 2: value updated     2   28.6%
#> 3:   not updated     1   14.3%
#> 4:         total     7    100%
#> ────────────────────────────────────────────────────────── End of JOYn report ──
#> ℹ Note: Joyn's report available in variable .joyn
#> ℹ Note: Removing key variables id from id, yd, y, and x
#>       id     t     x    yd     y         .joyn
#>    <num> <int> <num> <num> <int>        <fctr>
#> 1:     1     1    16     1    11 value updated
#> 2:     1     2    16     1    11 value updated
#> 3:     2     1    17     2    15    NA updated
#> 4:     3     2    20     3    10    NA updated
#> 5:    NA    NA    15    NA    NA   not updated
#> 6:     5    NA    18     5    20    NA updated
#> 7:     6    NA    19     6    13    NA updated
```
