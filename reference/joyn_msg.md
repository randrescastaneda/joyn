# display type of joyn message

display type of joyn message

## Usage

``` r
joyn_msg(msg_type = getOption("joyn.msg_type"), msg = NULL)
```

## Arguments

- msg_type:

  character: one or more of the following: all, basic, info, note, warn,
  timing, or err

- msg:

  character vector to be parsed to
  [`cli::cli_abort()`](https://cli.r-lib.org/reference/cli_abort.html).
  Default is NULL. It only works if `"err" %in% msg_type`. This is an
  internal argument.

## Value

returns data frame with message invisibly. print message in console

## See also

Messages functions
[`clear_joynenv()`](https://randrescastaneda.github.io/joyn/reference/clear_joynenv.md),
[`joyn_msgs_exist()`](https://randrescastaneda.github.io/joyn/reference/joyn_msgs_exist.md),
[`joyn_report()`](https://randrescastaneda.github.io/joyn/reference/joyn_report.md),
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
df <- joyn(x1, y1, match_type = "m:1")
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
joyn_msg("basic")
#> ℹ Note: Joyn's report available in variable .joyn
#> ℹ Note: Removing key variables id from id and y
joyn_msg("all")
#> ℹ Note: Joyn's report available in variable .joyn
#> ℹ Note: Removing key variables id from id and y
#> ● Timing:The full joyn is executed in 0.000285 seconds.
#> ● Timing: The entire joyn function, including checks, is executed in 0.022871
#> seconds.
```
