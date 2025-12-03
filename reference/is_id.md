# Check if dt is uniquely identified by `by` variable

report if dt is uniquely identified by `by` var or, if report = TRUE,
the duplicates in `by` variable

## Usage

``` r
is_id(
  dt,
  by,
  verbose = getOption("joyn.verbose", default = FALSE),
  return_report = FALSE
)
```

## Arguments

- dt:

  either right of left table

- by:

  variable to merge by

- verbose:

  logical: if TRUE messages will be displayed

- return_report:

  logical: if TRUE, returns data with summary of duplicates. If FALSE,
  returns logical value depending on whether `dt` is uniquely identified
  by `by`

## Value

logical or data.frame, depending on the value of argument
`return_report`

## Examples

``` r
library(data.table)

# example with data frame not uniquely identified by `by` var

y <- data.table(id = c("c","b", "c", "a"),
                 y  = c(11L, 15L, 18L, 20L))
is_id(y, by = "id")
#> ! Duplicates found by: `id`
#> [1] FALSE
is_id(y, by = "id", return_report = TRUE)
#> ! Duplicates found by: `id`
#> 
#> ── Duplicates in terms of `id` 
#>        id copies percent
#>    <char>  <int>  <char>
#> 1:      c      2     50%
#> 2:  total      4    100%
#> ─────────────────────────────────────────────────────── End of is_id() report ──

# example with data frame uniquely identified by `by` var

y1 <- data.table(id = c("1","3", "2", "9"),
                 y  = c(11L, 15L, 18L, 20L))
is_id(y1, by = "id")
#> ✔ No duplicates found by `id`
#> [1] TRUE
```
