# Perform necessary preliminary checks on arguments that are passed to joyn

Perform necessary preliminary checks on arguments that are passed to
joyn

## Usage

``` r
arguments_checks(
  x,
  y,
  by,
  copy,
  keep,
  suffix,
  na_matches,
  multiple,
  relationship,
  reportvar
)
```

## Arguments

- x:

  data frame: left table

- y:

  data frame: right table

- by:

  character vector or variables to join by

- copy:

  If `x` and `y` are not from the same data source, and `copy` is
  `TRUE`, then `y` will be copied into the same src as `x`. This allows
  you to join tables across srcs, but it is a potentially expensive
  operation so you must opt into it.

- keep:

  Should the join keys from both `x` and `y` be preserved in the output?

  - If `NULL`, the default, joins on equality retain only the keys from
    `x`, while joins on inequality retain the keys from both inputs.

  - If `TRUE`, all keys from both inputs are retained.

  - If `FALSE`, only keys from `x` are retained. For right and full
    joins, the data in key columns corresponding to rows that only exist
    in `y` are merged into the key columns from `x`. Can't be used when
    joining on inequality conditions.

- suffix:

  If there are non-joined duplicate variables in `x` and `y`, these
  suffixes will be added to the output to disambiguate them. Should be a
  character vector of length 2.

- na_matches:

  Should two `NA` or two `NaN` values match?

  - `"na"`, the default, treats two `NA` or two `NaN` values as equal,
    like `%in%`, [`match()`](https://rdrr.io/r/base/match.html), and
    [`merge()`](https://randrescastaneda.github.io/joyn/reference/merge.md).

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

- reportvar:

  character: Name of reporting variable. Default is ".joyn". This is the
  same as variable "\_merge" in Stata after performing a merge. If FALSE
  or NULL, the reporting variable will be excluded from the final table,
  though a summary of the join will be display after concluding.

## Value

list of checked arguments to pass on to the main joyn function
