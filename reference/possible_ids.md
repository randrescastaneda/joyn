# Find possible unique identifies of data frame

Identify possible variables uniquely identifying x

## Usage

``` r
possible_ids(
  dt,
  exclude = NULL,
  include = NULL,
  verbose = getOption("possible_ids.verbose")
)
```

## Arguments

- dt:

  data frame

- exclude:

  character: Exclude variables to be selected as identifiers. It could
  be either the name of the variables of one type of the variable
  prefixed by "\_". For instance, "\_numeric" or "\_character".

- include:

  character: Name of variable to be included, that might belong to the
  group excluded in the `exclude`

- verbose:

  logical: If FALSE no message will be displayed. Default is TRUE

## Value

list with possible identifiers

## Examples

``` r
library(data.table)
x4 = data.table(id1 = c(1, 1, 2, 3, 3),
                id2 = c(1, 1, 2, 3, 4),
                t   = c(1L, 2L, 1L, 2L, NA_integer_),
                x   = c(16, 12, NA, NA, 15))
possible_ids(x4)
#> ✔ There are no duplicates in data frame
#> → we found 5 possible ids
#> $V1
#> [1] "id1" "t"  
#> 
#> $V2
#> [1] "id1" "x"  
#> 
#> $V3
#> [1] "id2" "t"  
#> 
#> $V4
#> [1] "id2" "x"  
#> 
#> $V5
#> [1] "t" "x"
#> 
```
