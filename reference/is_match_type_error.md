# Confirm if match type error

Confirm if match type error

## Usage

``` r
is_match_type_error(x, name, by, verbose, match_type_error)
```

## Arguments

- name:

  name of data frame

- by:

  A vector of shared column names in `x` and `y` to merge on. This
  defaults to the shared key columns between the two tables. If `y` has
  no key columns, this defaults to the key of `x`.

- match_type_error:

  logical: from existing code

## Value

logical

## Examples

``` r
if (FALSE) { # \dontrun{
# example with dt not uniquely identified by "id"
x1 = data.table(id = c(1L, 1L, 2L, 3L, NA_integer_),
                t  = c(1L, 2L, 1L, 2L, NA_integer_),
                x  = 11:15)
joyn:::is_match_type_error(x1, name = "x1", by = "id")
} # }
```
