# Check tables X and Y

This function performs checks inspired on merge.data.table: it detects
errors

- if x and/or y have no columns

- if x and/or y contain duplicate column names

## Usage

``` r
check_xy(x, y)
```

## Arguments

- x:

  data frame: referred to as *left* in R terminology, or *master* in
  Stata terminology.

- y:

  data frame: referred to as *right* in R terminology, or *using* in
  Stata terminology.

## Value

invisible TRUE

## Examples

``` r
if (FALSE) { # \dontrun{
# Check passing with no errors
library(data.table)
x1 = data.table(id = c(1L, 1L, 2L, 3L, NA_integer_),
                t  = c(1L, 2L, 1L, 2L, NA_integer_),
                x  = 11:15)
y1 = data.table(id = c(1,2, 4),
                y  = c(11L, 15L, 16))
joyn:::check_xy(x = x1, y=y1)
} # }
```
