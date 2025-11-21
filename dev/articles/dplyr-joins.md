# dplyr joins wrappers

## Overview

Joining data tables with `joyn` is particularly convenient as it allows
you to analyze/be aware of the quality of the merging.

This vignette explores dplyr-like join functions available in `joyn`.
Their major objective is to let you employ a syntax you are supposedly
already familiar with - the `dplyr` one - while at the same time
benefiting of the additional tools that `joyn` offers. That is,
obtaining additional information and verification of the joining.

There are four types of dplyr-like join functions in `joyn`:

- Left joins:
  [`joyn::left_join()`](https://randrescastaneda.github.io/joyn/dev/reference/left_join.md)

- Right joins:
  [`joyn::right_join()`](https://randrescastaneda.github.io/joyn/dev/reference/right_join.md)

- Full joins:
  [`joyn::full_join()`](https://randrescastaneda.github.io/joyn/dev/reference/full_join.md)

- Inner joins:
  [`joyn::inner_join()`](https://randrescastaneda.github.io/joyn/dev/reference/inner_join.md)

Each of them is a wrapper that works in a similar way as the
corresponding `dplyr` function.

``` r

library(joyn)
#> 
#> Attaching package: 'joyn'
#> The following object is masked from 'package:base':
#> 
#>     merge
library(data.table)
```

## Rationale

``` r

x1 <- data.table(id  = c(1L, 1L, 2L, 3L, NA_integer_),
                 t   = c(1L, 2L, 1L, 2L, NA_integer_),
                 x   = 11:15)

y1 <- data.table(id  = c(1,2, 4),
                 y   = c(11L, 15L, 16))
```

Suppose you want to perform a **simple left join** between tables `x1`
and `y1`.

With `joyn` you have two possibilities:

- using the
  [`joyn()`](https://randrescastaneda.github.io/joyn/dev/reference/joyn.md)
  function, specifying `keep = "left"`

- using the
  **[`joyn::left_join()`](https://randrescastaneda.github.io/joyn/dev/reference/left_join.md)**
  function

In addition, you could use
[`dplyr::left_join()`](https://dplyr.tidyverse.org/reference/mutate-joins.html)
or base R merging functions.

Consider these three options:

``` r

# Option 1

joyn(x          = x1, 
     y          = y1, 
     keep       = "left",
     match_type = "m:1")
#> 
#> ── JOYn Report ──
#> 
#>   .joyn n percent
#> 1     x 2     40%
#> 2     y 1     20%
#> 3 x & y 2     40%
#> 4 total 5    100%
#> ────────────────────────────────────────────────────────── End of JOYn report ──
#> ℹ Note: Joyn's report available in variable .joyn
#> ℹ Note: Removing key variables id from id and y
#>       id     t     x     y  .joyn
#>    <num> <int> <int> <num> <fctr>
#> 1:     1     1    11    11  x & y
#> 2:     1     2    12    11  x & y
#> 3:     2     1    13    15  x & y
#> 4:     3     2    14    NA      x
#> 5:    NA    NA    15    NA      x

# Option 2 

joyn::left_join(x            = x1, 
                y            = y1, 
                relationship = "many-to-one") 
#> 
#> ── JOYn Report ──
#> 
#>   .joyn n percent
#> 1     x 2     40%
#> 2     y 1     20%
#> 3 x & y 2     40%
#> 4 total 5    100%
#> ────────────────────────────────────────────────────────── End of JOYn report ──
#> ℹ Note: Joyn's report available in variable .joyn
#> ℹ Note: Removing key variables id from id and y
#> ⚠ Warning: joyn does not currently allow inequality joins, so keep = NULL will
#> retain only keys in x
#>       id     t     x     y  .joyn
#>    <num> <int> <int> <num> <fctr>
#> 1:     1     1    11    11  x & y
#> 2:     1     2    12    11  x & y
#> 3:     2     1    13    15  x & y
#> 4:     3     2    14    NA      x
#> 5:    NA    NA    15    NA      x

# Option 3

dplyr::left_join(x            = x1, 
                 y            = y1, 
                 relationship = "many-to-one") 
#> Joining with `by = join_by(id)`
#>       id     t     x     y
#>    <num> <int> <int> <num>
#> 1:     1     1    11    11
#> 2:     1     2    12    11
#> 3:     2     1    13    15
#> 4:     3     2    14    NA
#> 5:    NA    NA    15    NA
```

Comparing the results, the same returning data table is produced.

However,
[`joyn::left_join()`](https://randrescastaneda.github.io/joyn/dev/reference/left_join.md)
allows you to enjoy both the intuitive syntax from `dplyr` and the
additional tools from `joyn`. These include additional options to
customize how the join is performed, the availability of the joyn
report, messages informing you on time of execution and the status of
the join as well as the execution of various checks during the merging.
(For additional information on each of these `joyn`’s features, please
take a look at all the other articles in this website.)

## Some examples

#### 1. Left join

ℹ️ Left joins return in the output table all rows from `x`, i.e., the
left table, and only matching rows from `y`, i.e., the right table.

``` r

# Data tables to be joined 

df1 <- data.frame(id = c(1L, 1L, 2L, 3L, NA_integer_, NA_integer_),
                  t  = c(1L, 2L, 1L, 2L, NA_integer_, 4L),
                  x  = 11:16)

df2 <- data.frame(id = c(1,2, 4, NA_integer_, 8),
                  y  = c(11L, 15L, 16, 17L, 18L),
                  t  = c(13:17))
```

*Example usage of some of the `joyn`’s additional options:*

***Updating NAs in left table***

Using the `update_NAs` argument from `joyn` you can update the values
that are NA in the *t* variable in the left table with the actual values
from the matching column *t* in the right one

``` r

left_join(x            = df1, 
          y            = df2,
          relationship = "many-to-one", 
          by           = "id",
          update_NAs   = TRUE)
#> 
#> ── JOYn Report ──
#> 
#>         .joyn     n percent
#>        <char> <int>  <char>
#> 1:          x     1   16.7%
#> 2:      x & y     4   66.7%
#> 3: NA updated     1   16.7%
#> 4:      total     6    100%
#> ────────────────────────────────────────────────────────── End of JOYn report ──
#> ℹ Note: Joyn's report available in variable .joyn
#> ℹ Note: Removing key variables id from id, y, and t
#>   id t.x  x  y t.y      .joyn
#> 1  1   1 11 11  13      x & y
#> 2  1   2 12 11  13      x & y
#> 3  2   1 13 15  14      x & y
#> 4  3   2 14 NA  NA          x
#> 5 NA  16 15 17  16 NA updated
#> 6 NA   4 16 17  16      x & y
```

***Specifying which variables to keep from the right table after the
join***

``` r

left_join(x              = df1, 
          y              = df2,
          relationship   = "many-to-one", 
          by             = "id", 
          y_vars_to_keep = "y")
#> 
#> ── JOYn Report ──
#> 
#>   .joyn n percent
#> 1     x 1   16.7%
#> 2     y 2   33.3%
#> 3 x & y 3     50%
#> 4 total 6    100%
#> ────────────────────────────────────────────────────────── End of JOYn report ──
#> ℹ Note: Joyn's report available in variable .joyn
#>   id  t  x  y .joyn
#> 1  1  1 11 11 x & y
#> 2  1  2 12 11 x & y
#> 3  2  1 13 15 x & y
#> 4  3  2 14 NA     x
#> 5 NA NA 15 17 x & y
#> 6 NA  4 16 17 x & y
```

#### 2. Right join

ℹ️ Right joins return in the output table matching rows from `x`, i.e.,
the left table, and all rows from `y`, i.e., the right table.

*Example usage of some of the `joyn`’s additional options:*

***Specifying a name for the reporting variable***

``` r

right_join(x            = df1, 
          y            = df2,
          relationship = "many-to-one", 
          by           = "id",
          reportvar    = "right.joyn")
#> 
#> ── JOYn Report ──
#> 
#>   right.joyn n percent
#> 1          x 1   14.3%
#> 2          y 2   28.6%
#> 3      x & y 4   57.1%
#> 4      total 7    100%
#> ────────────────────────────────────────────────────────── End of JOYn report ──
#> ℹ Note: Joyn's report available in variable right.joyn
#> ℹ Note: Removing key variables id from id, y, and t
#>   id t.x  x  y t.y right.joyn
#> 1  1   1 11 11  13      x & y
#> 2  1   2 12 11  13      x & y
#> 3  2   1 13 15  14      x & y
#> 4  4  NA NA 16  15          y
#> 5  8  NA NA 18  17          y
#> 6 NA  NA 15 17  16      x & y
#> 7 NA   4 16 17  16      x & y
```

***Updating values in common variables***

By setting `update_values = TRUE`, all values in x (both NAs and not)
will be updated with the actual values of variables in y with the same
name as the ones in x. You can then see the status of the update in the
reporting variable.

``` r

right_join(x            = df1, 
           y            = df2,
           relationship = "many-to-one", 
           by           = "id",
           reportvar    = "right.joyn")
#> 
#> ── JOYn Report ──
#> 
#>   right.joyn n percent
#> 1          x 1   14.3%
#> 2          y 2   28.6%
#> 3      x & y 4   57.1%
#> 4      total 7    100%
#> ────────────────────────────────────────────────────────── End of JOYn report ──
#> ℹ Note: Joyn's report available in variable right.joyn
#> ℹ Note: Removing key variables id from id, y, and t
#>   id t.x  x  y t.y right.joyn
#> 1  1   1 11 11  13      x & y
#> 2  1   2 12 11  13      x & y
#> 3  2   1 13 15  14      x & y
#> 4  4  NA NA 16  15          y
#> 5  8  NA NA 18  17          y
#> 6 NA  NA 15 17  16      x & y
#> 7 NA   4 16 17  16      x & y
```

#### 3. Full join

ℹ️ Full joins return in the output table all rows, both matching and non
matching rows from `x`, i.e., the left table, and `y`, i.e., the right
table.

``` r

full_join(x = x1, 
          y = y1, 
          relationship = "many-to-one", 
          keep = TRUE)
#> 
#> ── JOYn Report ──
#> 
#>   .joyn n percent
#> 1     x 2   33.3%
#> 2     y 1   16.7%
#> 3 x & y 3     50%
#> 4 total 6    100%
#> ────────────────────────────────────────────────────────── End of JOYn report ──
#> ℹ Note: Joyn's report available in variable .joyn
#> ℹ Note: Removing key variables id from id.y, id, and y
#>       id     t     x  id.y     y  .joyn
#>    <num> <int> <int> <num> <num> <fctr>
#> 1:     1     1    11     1    11  x & y
#> 2:     1     2    12     1    11  x & y
#> 3:     2     1    13     2    15  x & y
#> 4:     3     2    14    NA    NA      x
#> 5:     4    NA    NA     4    16      y
#> 6:    NA    NA    15    NA    NA      x
```

#### 4. Inner join

ℹ️ Inner joins return in the output table only rows that match between
`x`, i.e., the left table, and `y`, i.e., the right table.

***Simple inner join***

``` r

inner_join(x            = df1, 
           y             = df2,
           relationship  = "many-to-one", 
           by            = "id")
#> 
#> ── JOYn Report ──
#> 
#>   .joyn n percent
#> 1     x 1     20%
#> 2     y 2     40%
#> 3 x & y 2     40%
#> 4 total 5    100%
#> ────────────────────────────────────────────────────────── End of JOYn report ──
#> ℹ Note: Joyn's report available in variable .joyn
#> ℹ Note: Removing key variables id from id, y, and t
#>   id t.x  x  y t.y .joyn
#> 1  1   1 11 11  13 x & y
#> 2  1   2 12 11  13 x & y
#> 3  2   1 13 15  14 x & y
#> 4 NA  NA 15 17  16 x & y
#> 5 NA   4 16 17  16 x & y
```
