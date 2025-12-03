# Find possible unique identifies of data frame

Identify possible combinations of variables that uniquely identifying dt

## Usage

``` r
possible_ids(
  dt,
  vars = names(dt),
  exclude = NULL,
  include = NULL,
  exclude_classes = NULL,
  include_classes = NULL,
  verbose = getOption("possible_ids.verbose", default = FALSE),
  min_combination_size = 1,
  max_combination_size = 5,
  max_processing_time = 60,
  max_numb_possible_ids = 100,
  get_all = FALSE
)
```

## Arguments

- dt:

  data frame

- vars:

  character: A vector of variable names to consider for identifying
  unique combinations.

- exclude:

  character: Names of variables to exclude from analysis

- include:

  character: Name of variable to be included, that might belong to the
  group excluded in the `exclude`

- exclude_classes:

  character: classes to exclude from analysis (e.g., "numeric",
  "integer", "date")

- include_classes:

  character: classes to include in the analysis (e.g., "numeric",
  "integer", "date")

- verbose:

  logical: If FALSE no message will be displayed. Default is TRUE

- min_combination_size:

  numeric: Min number of combinations. Default is 1, so all
  combinations.

- max_combination_size:

  numeric. Max number of combinations. Default is 5. If there is a
  combinations of identifiers larger than `max_combination_size`, they
  won't be found

- max_processing_time:

  numeric: Max time to process in seconds. After that, it returns what
  it found.

- max_numb_possible_ids:

  numeric: Max number of possible IDs to find. See details.

- get_all:

  logical: get all possible combinations based on the parameters above.

## Value

list with possible identifiers

## Number of possible IDs

The number of possible IDs in a dataframe could be very large. This is
why, `possible_ids()` makes use of heuristics to return something useful
without wasting the time of the user. In addition, we provide multiple
parameter so that the user can fine tune their search for possible IDs
easily and quickly.

Say for instance that you have a dataframe with 10 variables. Testing
every possible pair of variables will give you 90 possible unique
identifiers for this dataframe. If you want to test all the possible
IDs, you will have to test more 5000 combinations. If the dataframe has
many rows, it may take a while.

## Examples

``` r
library(data.table)
x4 = data.table(id1 = c(1, 1, 2, 3, 3),
                id2 = c(1, 1, 2, 3, 4),
                t   = c(1L, 2L, 1L, 2L, NA_integer_),
                x   = c(16, 12, NA, NA, 15))
possible_ids(x4)
#> [[1]]
#> [1] "id1" "t"  
#> 
#> attr(,"checked_ids")
#> [1] "id1" "t"   "id2" "x"  
```
