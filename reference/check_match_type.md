# Check match type consistency

This function checks if the match type chosen by the user is consistent
with the data.  
(Match type must be one of the valid types: "1:1", "1:m", "m:1", "m:m")

## Usage

``` r
check_match_type(x, y, by, match_type, verbose = getOption("joyn.verbose"))
```

## Arguments

- x, y:

  `data table`s. `y` is coerced to a `data.table` if it isn't one
  already.

- by:

  A vector of shared column names in `x` and `y` to merge on. This
  defaults to the shared key columns between the two tables. If `y` has
  no key columns, this defaults to the key of `x`.

- match_type:

  character: one of *"m:m"*, *"m:1"*, *"1:m"*, *"1:1"*. Default is
  *"1:1"* since this the most restrictive. However, following Stata's
  recommendation, it is better to be explicit and use any of the other
  three match types (See details in *match types sections*).

## Value

character vector from
[split_match_type](https://randrescastaneda.github.io/joyn/reference/split_match_type.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Consistent match type
x1 = data.frame(
       id = c(1L, 1L, 2L, 3L, NA_integer_),
       t  = c(1L, 2L, 1L, 2L, NA_integer_),
       x  = 11:15)
y1 = data.frame(id = 1:2,
                y  = c(11L, 15L))
joyn:::check_match_type(x = x1, y=y1, by="id", match_type = "m:1")

# Inconsistent match type
joyn:::check_match_type(x = x1, y=y1, by="id", match_type = "1:1")
} # }
```
