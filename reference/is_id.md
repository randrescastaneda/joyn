# Check if dt is uniquely identified by `by` variable

report if dt is uniquely identified by `by` var or, if report = TRUE,
the duplicates in `by` variable

## Usage

``` r
is_id(dt, by, verbose = getOption("joyn.verbose"), return_report = FALSE)
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
#> 
#> ── Duplicates in terms of `id` 
#>   copies n percent
#> 1      1 2   66.7%
#> 2      2 1   33.3%
#> 3  total 3    100%
#> ─────────────────────────────────────────────────────── End of is_id() report ──
#> [1] FALSE
is_id(y, by = "id", return_report = TRUE)
#> 
#> ── Duplicates in terms of `id` 
#>   copies n percent
#> 1      1 2   66.7%
#> 2      2 1   33.3%
#> 3  total 3    100%
#> ─────────────────────────────────────────────────────── End of is_id() report ──
#>        id copies
#>    <char>  <int>
#> 1:      c      2
#> 2:      b      1
#> 3:      a      1

# example with data frame uniquely identified by `by` var

y1 <- data.table(id = c("1","3", "2", "9"),
                 y  = c(11L, 15L, 18L, 20L))
is_id(y1, by = "id")
#> 
#> ── Duplicates in terms of `id` 
#>   copies n percent
#> 1      1 4    100%
#> 2  total 4    100%
#> ─────────────────────────────────────────────────────── End of is_id() report ──
#> [1] TRUE
```
