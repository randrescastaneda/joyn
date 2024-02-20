
# joyn

<!-- badges: start -->

[![R-CMD-check](https://github.com/randrescastaneda/joyn/workflows/R-CMD-check/badge.svg/)](https://github.com/randrescastaneda/joyn/actions/)
[![](https://www.r-pkg.org/badges/version/joyn?color=orange)](https://cran.r-project.org/package=joyn/)
[![](https://img.shields.io/badge/devel%20version-0.1.4-blue.svg)](https://github.com/randrescastaneda/joyn)
[![](https://img.shields.io/badge/lifecycle-maturing-green.svg)](https://lifecycle.r-lib.org/articles/stages.html#maturing)
[![codecov](https://codecov.io/gh/randrescastaneda/joyn/branch/master/graph/badge.svg)](https://app.codecov.io/gh/randrescastaneda/joyn?branch=master)
<!-- [![](https://www.r-pkg.org/badges/version/joyn?color=orange)](https://cran.r-project.org/package=joyn) -->
<!-- [![](https://img.shields.io/badge/devel%20version-0.1.5-blue.svg)](https://github.com/randrescastaneda/joyn) -->
<!-- [![](https://img.shields.io/badge/lifecycle-maturing-green.svg)](https://lifecycle.r-lib.org/articles/stages.html#maturing) -->
[![R-CMD-check](https://github.com/randrescastaneda/joyn/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/randrescastaneda/joyn/actions/workflows/R-CMD-check.yaml)

<!-- badges: end -->

`joyn` provides the user with a set of tools to analyze the quality of
merging (i.e., joining) data frames, so that it is a **JOY** to join
tables with `joyn`. This is inspired on the command `merge` of the
statistical software `Stata`.

## Motivation

The objective of `joyn` is to make your life easier when joining tables.
As a former Stata user (I still work with Stata but not that much as I
work with R now), I had missed, until now, the ability to assess the
accuracy of my join after mergin two tables in R. With one single
command, `merge`, Stata allows the user to perform any kind of
equi-join. The reason for this is that, by default, Stata merges fully
both tables into one and then it is up to the user to keep the
observation she needs. Most importantly, Stata forces the user to know
how the joining tables relate to each other. Most tables have a
one-to-one (1:1 ) relation, but it is common to find one-to-many (1:m),
many-to-one (m:1), and many-to-many (m:m) relations. Finally, Stata’s
`merge` command returns by default a variable with useful information
about the table’s join.

**The following features are the value added of `joyn`:**

1.  `joyn` performs a full join by default (i.e., resulting table has
    all the observations from both original, joining tables). Yet, the
    user can select “left”, “right” or “inner” joins using the argument
    `keep`.

2.  In the same vain, `joyn` keeps all the columns form both tables. Two
    caveats are worth mentioning:

    1.  If both tables have variables with the same name, by default
        `joyn` does not bring those variables from the right table into
        the left table. Yet, you can do one of two things. First, you
        can use arguments `update_values` or `update_NA` to update the
        value of those variable in the left table with the same name
        ones in the right table. As an alternative, you can also use the
        argument `keep_common_vars = TRUE` to keep both variables with a
        different name.

    2.  You can use the argument `y_vars_to_keep` to select which
        variables you want to bring from the right table into the left
        one.

3.  `joyn` allows the user to perform one-to-one (1:1) , one-to-many
    (1:m), many-to-one (m:1), and many-to-many (m:m) joins. !! FIX THIS:
    The default is m:m, following general R’s practice, but its is
    highly discouraged. Since you want to use `joyn`, I recommend you
    always specify the right relationship between your joining tables.

4.  `joyn` returns a reporting variable in the output table that informs
    about the status of the join.

## Regarding speed and flexibility

Notice the `joyn` is not intended to be a super fast joining tool. By
construction, it does a lot of things that will make it slower than
other tools out there. However, `joyn` is basically a wrapper around
`data.table`’s `merge.data.table()` function. So, the lost of speed of
`joyn` is mainly due to evaluating several conditions, creating the
reporting variable, and present a nice summary table at the end of the
process.

Also, keep in mind that `joyn` is intended to be informative, so it
displays messages here and there to inform you about your join (you can
silence any message in `joyn`, including the reporting table, by using
the argument `verbose = FALSE`). This, of course, makes `joyn` a little
slower than using regular `data.table` syntax. However, the loss of
speed is not much and you’re gaining a lot of information. The main
reason why `joyn` is a little slower is than pure `data.table` is that
it will always perform a full join of data in the same way `Stata` does.
Again, `joyn` is intended for information and verification of joins of
tables. If you are working on a project that executes many (say, 1000)
joins or you are constantly merging super big data sets, it is
recommended that you use `data.table`’s syntax directly.

As of now, the flexibility of `joyn` is limited to the basic joins, yet
the most used and useful ones. If you want to learn more about different
kinds of joins available in `data.table` and how they relate to `dplyr`,
I recommend you start with this [blog
post](https://atrebas.github.io/post/2019-03-03-datatable-dplyr/#joinbind-data-sets/).

## Installation

You can install the stable version of `joyn` from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("joyn")
```

The development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("randrescastaneda/joyn")
```

## Examples

``` r

library(joyn)
library(data.table)

x1 = data.table(id = c(1L, 1L, 2L, 3L, NA_integer_),
                t  = c(1L, 2L, 1L, 2L, NA_integer_),
                x  = 11:15)

y1 = data.table(id = c(1,2, 4),
                y  = c(11L, 15L, 16))


x2 = data.table(id = c(1, 4, 2, 3, NA),
                t  = c(1L, 2L, 1L, 2L, NA_integer_),
                x  = c(16, 12, NA, NA, 15))


y2 = data.table(id = c(1, 2, 5, 6, 3),
                yd = c(1, 2, 5, 6, 3),
                y  = c(11L, 15L, 20L, 13L, 10L),
                x  = c(16:20))

# using common variable `id` as key.
joyn(x = x1, 
     y = y1,
     match_type = "m:1")
#> 
#> ── JOYn Report ──
#> 
#>    .joyn n percent
#> 1:     x 2   33.3%
#> 2: x & y 3     50%
#> 3:     y 1   16.7%
#> 4: total 6    100%
#> ────────────────────────────────────────────────────────── End of JOYn report ──
#> ℹ ❯ Joyn's report available in variable .joyn
#> ℹ ❯ Removing key variables id from id and y
#> ● Timing: The full joyn is executed in 0.000778 seconds
#> ● Timing: The entire joyn function, including checks, is executed in 0.049526
#> seconds
#>    id  t  x  y .joyn
#> 1: NA NA 15 NA     x
#> 2:  1  1 11 11 x & y
#> 3:  1  2 12 11 x & y
#> 4:  2  1 13 15 x & y
#> 5:  3  2 14 NA     x
#> 6:  4 NA NA 16     y

# keep just those observations that match
joyn(x = x1, 
     y = y1, 
     match_type = "m:1",
     keep = "inner")
#> 
#> ── JOYn Report ──
#> 
#>    .joyn n percent
#> 1: x & y 3    100%
#> 2: total 3    100%
#> ────────────────────────────────────────────────────────── End of JOYn report ──
#> ℹ ❯ Joyn's report available in variable .joyn
#> ℹ ❯ Removing key variables id from id and y
#> ● Timing: The full joyn is executed in 7.3e-05 seconds
#> ● Timing: The entire joyn function, including checks, is executed in 0.042698
#> seconds
#>    id t  x  y .joyn
#> 1:  1 1 11 11 x & y
#> 2:  1 2 12 11 x & y
#> 3:  2 1 13 15 x & y

# Bad merge for not specifying by argument
joyn(x = x2, 
     y = y2,
     match_type = "1:1")
#> 
#> ── JOYn Report ──
#> 
#>    .joyn n percent
#> 1:     x 4   44.4%
#> 2: x & y 1   11.1%
#> 3:     y 4   44.4%
#> 4: total 9    100%
#> ────────────────────────────────────────────────────────── End of JOYn report ──
#> ℹ ❯ Joyn's report available in variable .joyn
#> ℹ ❯ Removing key variables id and x from id, yd, y, and x
#> ● Timing: The full joyn is executed in 8e-05 seconds
#> ● Timing: The entire joyn function, including checks, is executed in 0.035755
#> seconds
#>    id  t  x yd  y .joyn
#> 1: NA NA 15 NA NA     x
#> 2:  1  1 16  1 11 x & y
#> 3:  2  1 NA NA NA     x
#> 4:  2 NA 17  2 15     y
#> 5:  3  2 NA NA NA     x
#> 6:  3 NA 20  3 10     y
#> 7:  4  2 12 NA NA     x
#> 8:  5 NA 18  5 20     y
#> 9:  6 NA 19  6 13     y

# good merge, ignoring variable x from y
joyn(x = x2, 
     y = y2,
     by = "id",
     match_type = "1:1")
#> 
#> ── JOYn Report ──
#> 
#>    .joyn n percent
#> 1:     x 2   28.6%
#> 2: x & y 3   42.9%
#> 3:     y 2   28.6%
#> 4: total 7    100%
#> ────────────────────────────────────────────────────────── End of JOYn report ──
#> ℹ ❯ Joyn's report available in variable .joyn
#> ℹ ❯ Removing key variables id from id, yd, y, and x
#> ● Timing: The full joyn is executed in 8.2e-05 seconds
#> ● Timing: The entire joyn function, including checks, is executed in 0.03433
#> seconds
#>    id  t  x yd  y .joyn
#> 1: NA NA 15 NA NA     x
#> 2:  1  1 16  1 11 x & y
#> 3:  2  1 NA  2 15 x & y
#> 4:  3  2 NA  3 10 x & y
#> 5:  4  2 12 NA NA     x
#> 6:  5 NA NA  5 20     y
#> 7:  6 NA NA  6 13     y

# update NAs in var x in table x from var x in y
joyn(x = x2, 
     y = y2, 
     by = "id", 
     update_NAs = TRUE)
#> 
#> ── JOYn Report ──
#> 
#>         .joyn n percent
#> 1: NA updated 4   57.1%
#> 2:          x 2   28.6%
#> 3:      x & y 1   14.3%
#> 4:      total 7    100%
#> ────────────────────────────────────────────────────────── End of JOYn report ──
#> ℹ ❯ Joyn's report available in variable .joyn
#> ℹ ❯ Removing key variables id from id, yd, y, and x
#> ● Timing: The full joyn is executed in 8.5e-05 seconds
#> ● Timing: The entire joyn function, including checks, is executed in 0.037088
#> seconds
#>    id  t  x yd  y      .joyn
#> 1: NA NA 15 NA NA          x
#> 2:  1  1 16  1 11      x & y
#> 3:  2  1 17  2 15 NA updated
#> 4:  3  2 20  3 10 NA updated
#> 5:  4  2 12 NA NA          x
#> 6:  5 NA 18  5 20 NA updated
#> 7:  6 NA 19  6 13 NA updated

# update values in var x in table x from var x in y
joyn(x = x2, 
     y = y2, 
     by = "id", 
     update_values = TRUE)
#> 
#> ── JOYn Report ──
#> 
#>            .joyn n percent
#> 1:    NA updated 4   57.1%
#> 2:   not updated 2   28.6%
#> 3: value updated 1   14.3%
#> 4:         total 7    100%
#> ────────────────────────────────────────────────────────── End of JOYn report ──
#> ℹ ❯ Joyn's report available in variable .joyn
#> ℹ ❯ Removing key variables id from id, yd, y, and x
#> ● Timing: The full joyn is executed in 8e-05 seconds
#> ● Timing: The entire joyn function, including checks, is executed in 0.038001
#> seconds
#>    id  t  x yd  y         .joyn
#> 1: NA NA 15 NA NA   not updated
#> 2:  1  1 16  1 11 value updated
#> 3:  2  1 17  2 15    NA updated
#> 4:  3  2 20  3 10    NA updated
#> 5:  4  2 12 NA NA   not updated
#> 6:  5 NA 18  5 20    NA updated
#> 7:  6 NA 19  6 13    NA updated


# do not bring any variable from y into x, just the report
joyn(x = x2, 
     y = y2, 
     by = "id", 
     y_vars_to_keep = NULL)
#> 
#> ── JOYn Report ──
#> 
#>    .joyn n percent
#> 1:     x 2   28.6%
#> 2: x & y 3   42.9%
#> 3:     y 2   28.6%
#> 4: total 7    100%
#> ────────────────────────────────────────────────────────── End of JOYn report ──
#> ℹ ❯ Joyn's report available in variable .joyn
#> ● Timing: The full joyn is executed in 8.3e-05 seconds
#> ● Timing: The entire joyn function, including checks, is executed in 0.023695
#> seconds
#>    id  t  x .joyn
#> 1: NA NA 15     x
#> 2:  1  1 16 x & y
#> 3:  2  1 NA x & y
#> 4:  3  2 NA x & y
#> 5:  4  2 12     x
#> 6:  5 NA NA     y
#> 7:  6 NA NA     y
```
