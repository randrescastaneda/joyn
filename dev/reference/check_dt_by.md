# Check dt `by` vars

check variable(s) by which data frames are joined: either a single `by`
var, common to right and left dt, or

## Usage

``` r
check_dt_by(x, y, by, by.x, by.y)
```

## Arguments

- x:

  left table

- y:

  right table

- by:

  character: variable to join by (common variable to x and y)

- by.x:

  character: specified var in x to join by

- by.y:

  character: specified var in y to join by

## Value

character specifying checked variable(s) to join by

## Examples

``` r
if (FALSE) { # \dontrun{
x = data.table(id1 = c(1, 1, 2, 3, 3),
               id2 = c(1, 1, 2, 3, 4),
               t   = c(1L, 2L, 1L, 2L, NA_integer_),
               x   = c(16, 12, NA, NA, 15))
y = data.table(id  = c(1, 2, 5, 6, 3),
               id2 = c(1, 1, 2, 3, 4),
               y   = c(11L, 15L, 20L, 13L, 10L),
               x   = c(16:20))
# example specifying by.x and by.y
joyn:::check_dt_by(x, y, by.x = "id1", by.y = "id2")
} # }
```
