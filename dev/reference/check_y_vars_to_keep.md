# Check variables in y that will be kept in returning table

check and return variable names in y to keep in returning table,
excluding those that are keys of the merge

## Usage

``` r
check_y_vars_to_keep(y_vars_to_keep, y, by)
```

## Arguments

- y_vars_to_keep:

  either TRUE, if keep all vars in `y`; FALSE or NULL, if keep no vars;
  or character vector specifying which variables in `y` to keep

- y:

  data frame

- by:

  A vector of shared column names in `x` and `y` to merge on. This
  defaults to the shared key columns between the two tables. If `y` has
  no key columns, this defaults to the key of `x`.

## Value

character vector with variable names from `y` table

## Examples

``` r
if (FALSE) { # \dontrun{
y1 = data.table(id = 1:2,
               y  = c(11L, 15L))
# With y_vars_to_keep TRUE
joyn:::check_y_vars_to_keep(TRUE, y1, by = "id")
# With y_vars_to_keep FALSE
joyn:::check_y_vars_to_keep(FALSE, y1, by = "id")
# Specifying which y vars to keep
joyn:::check_y_vars_to_keep("y", y1, by = "id")
} # }
```
