# Process the `by` vector

Gives as output a vector of names to be used for the specified table
that correspond to the `by` argument for that table

## Usage

``` r
process_by_vector(by, input = c("left", "right"))
```

## Arguments

- by:

  character vector: by argument for join

- input:

  character: either "left" or "right", indicating whether to give the
  left or right side of the equals ("=") if the equals is part of the
  `by` vector

## Value

character vector

## Examples

``` r
joyn:::process_by_vector(by = c("An = foo", "example"), input = "left")
#> [1] "An"      "example"
```
