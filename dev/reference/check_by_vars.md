# Check `by` input

This function checks the variable name(s) to be used as key(s) of the
join

## Usage

``` r
check_by_vars(by, x, y)
```

## Arguments

- by:

  A vector of shared column names in `x` and `y` to merge on. This
  defaults to the shared key columns between the two tables. If `y` has
  no key columns, this defaults to the key of `x`.

- x, y:

  `data table`s. `y` is coerced to a `data.table` if it isn't one
  already.

## Value

list with information about by variables

## Examples

``` r
if (FALSE) { # \dontrun{
x1 = data.frame(
       id = c(1L, 1L, 2L, 3L, NA_integer_),
       t  = c(1L, 2L, 1L, 2L, NA_integer_),
       x  = 11:15)
y1 = data.frame(id = 1:2,
                y  = c(11L, 15L))
# With var "id" shared in x and y
joyn:::check_by_vars(by = "id", x = x1, y = y1)
} # }
```
