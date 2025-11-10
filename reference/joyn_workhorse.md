# Internal workhorse join function, used in the back-end of `joyn`

Always executes a full join.

## Usage

``` r
joyn_workhorse(
  x,
  y,
  by = intersect(names(x), names(y)),
  sort = FALSE,
  suffixes = getOption("joyn.suffixes"),
  reportvar = getOption("joyn.reportvar")
)
```

## Arguments

- x:

  data object, "left" or "master"

- y:

  data object, "right" or "using"

- by:

  atomic character vector: key specifying join

- sort:

  logical: sort the result by the columns in `by` `x` and `y`

- suffixes:

  atomic character vector: give suffixes to columns common to both

## Value

data object of same class as `x`

## Examples

``` r
if (FALSE) { # \dontrun{
# Full join
library(data.table)
x1 = data.table(id = c(1L, 1L, 2L, 3L, NA_integer_),
                t  = c(1L, 2L, 1L, 2L, NA_integer_),
                x  = 11:15)
y1 = data.table(id = c(1,2, 4),
                y  = c(11L, 15L, 16))
joyn:::joyn_workhorse(x = x1, y=y1)
} # }
```
