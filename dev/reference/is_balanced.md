# Is data frame balanced by group?

Check if the data frame is balanced by group of columns, i.e., if it
contains every combination of the elements in the specified variables

## Usage

``` r
is_balanced(df, by, return = c("logic", "table"))
```

## Arguments

- df:

  data frame

- by:

  character: variables used to check if `df` is balanced

- return:

  character: either "logic" or "table". If "logic", returns `TRUE` or
  `FALSE` depending on whether data frame is balanced. If "table"
  returns the unbalanced observations - i.e. the combinations of
  elements in specified variables not found in input `df`

## Value

logical, if return == "logic", else returns data frame of unbalanced
observations

## Examples

``` r
x1 = data.frame(id = c(1L, 1L, 2L, 3L, NA_integer_),
                t  = c(1L, 2L, 1L, 2L, NA_integer_),
                x  = 11:15)
is_balanced(df = x1,
            by = c("id", "t"),
            return = "table") # returns combination of elements in "id" and "t" not present in df
#>   id t
#> 1  3 1
#> 2  2 2
is_balanced(df = x1,
            by = c("id", "t"),
            return = "logic") # FALSE
#> [1] FALSE
```
