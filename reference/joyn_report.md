# Print JOYn report table

Print JOYn report table

## Usage

``` r
joyn_report(verbose = getOption("joyn.verbose"))
```

## Arguments

- verbose:

  logical: if FALSE, it won't display any message (programmer's option).
  Default is TRUE.

## Value

invisible table of frequencies

## See also

Messages functions
[`clear_joynenv()`](https://randrescastaneda.github.io/joyn/reference/clear_joynenv.md),
[`joyn_msg()`](https://randrescastaneda.github.io/joyn/reference/joyn_msg.md),
[`joyn_msgs_exist()`](https://randrescastaneda.github.io/joyn/reference/joyn_msgs_exist.md),
[`msg_type_dt()`](https://randrescastaneda.github.io/joyn/reference/msg_type_dt.md),
[`store_msg()`](https://randrescastaneda.github.io/joyn/reference/store_msg.md),
[`style()`](https://randrescastaneda.github.io/joyn/reference/style.md),
[`type_choices()`](https://randrescastaneda.github.io/joyn/reference/type_choices.md)

## Examples

``` r
library(data.table)
x1 = data.table(id = c(1L, 1L, 2L, 3L, NA_integer_),
t  = c(1L, 2L, 1L, 2L, NA_integer_),
x  = 11:15)

y1 = data.table(id = 1:2,
                y  = c(11L, 15L))

d <- joyn(x1, y1, match_type = "m:1")
#> 
#> ── JOYn Report ──
#> 
#>   .joyn n percent
#> 1     x 2     40%
#> 2 x & y 3     60%
#> 3 total 5    100%
#> ────────────────────────────────────────────────────────── End of JOYn report ──
#> ℹ Note: Joyn's report available in variable .joyn
#> ℹ Note: Removing key variables id from id and y
joyn_report(verbose = TRUE)
#> 
#> ── JOYn Report ──
#> 
#>   .joyn n percent
#> 1     x 2     40%
#> 2 x & y 3     60%
#> 3 total 5    100%
#> ────────────────────────────────────────────────────────── End of JOYn report ──
```
