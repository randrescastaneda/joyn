# Create variables that uniquely identify rows in a data table

This function generates unique identifier columns for a given number of
rows, based on the specified number of identifier variables.

## Usage

``` r
create_ids(n_rows, n_ids, prefix = "id")
```

## Arguments

- n_rows:

  An integer specifying the number of rows in the data table for which
  unique identifiers need to be generated.

- n_ids:

  An integer specifying the number of identifiers to be created. If
  `n_ids` is 1, a simple sequence of unique IDs is created. If greater
  than 1, a combination of IDs is generated.

- prefix:

  A character string specifying the prefix for the identifier variable
  names (default is `"id"`).

## Value

A named list where each element is a vector representing a unique
identifier column. The number of elements in the list corresponds to the
number of identifier variables (`n_ids`). The length of each element is
equal to `n_rows`.
