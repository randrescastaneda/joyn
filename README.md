
# joyn

<!-- badges: start -->

[![Codecov test
coverage](https://codecov.io/gh/randrescastaneda/joyn/branch/master/graph/badge.svg)](https://codecov.io/gh/randrescastaneda/joyn?branch=master)
<!-- badges: end -->

The goal of `joyn` is to provide the user with a set of tools to analyze
the quality of merging (i.e., joining) data frames, so that it is a
**JOY** to join tables with `joyn`. This is inspired in the command
`merge` ofthe statistical software `Stata`.

## Motivation

The objective `joyn` is to make your life easier when joining tables. As
a former Stata user (I still work with Stata but not that much as I work
with R now), I have missed the ability to understand the results of
joining tables. With one single command, `merge`, Stata allows the user
to perform any kind of equi-join. The reason for this is that, by
default, Stata merges fully both tables into one and then it is up to
the user to keep the observation she needs. Most importantly, Stata
forces the user to know how her tables relate to each other. Some tables
have a one-to-one (1:1 ) relation, but it is common to find one-to-many
(1:m), many-to-one (m:1), and many-to-many (m:m) relations. Finally,
Stata’s `merge` command returns by default a variable with useful
information about the table’s join. So the following features are the
value added of `joyn`:

1.  Perform a full join by default (i.e., final table has all the
    observations of both original tables). Yet, the user can select
    “left”, “right” or “inner” joins using the argument `keep`.

2.  In the same vain, `joyn` merges all the columns in both tables. To
    caveats are worth mentioning.

    1.  If both tables have variables with the same name, by default
        `joyn` does not bring those variables from the second (i.e,
        right, using) table into the first (i.e., left, master) table.
        Yet, you can do one of two things. First, you can use arguments
        `update_values` or `updateNA` to update the value of those
        variable in the first table with the ones in the second table.
        Or, you can use argument `keep_y_in_x = TRUE` to keep both
        variables with a different name.

    2.  You can use the argument `yvars` to select which variables you
        want to bring from the second table into the first one.

3.  Allow the user to perform one-to-one (1:1) , one-to-many (1:m),
    many-to-one (m:1), and many-to-many (m:m) joins. The default is m:m,
    following general R’s practice, but its use is highly discouraged.

4.  Return a reporting variable with the status of the join.

## Regarding speed and flexibility

Notice the `joyn` is not intended to be a super fast joining tool. By
construction it does a lot of things that will make it slower that other
tools out there. However, `joyn` is basically a wrapper around
`data.table`’s indexed joining tools. So, the lost of speed of `joyn` is
mainly due to asses some conditions, create the reporting variable, and
(what really takes the most time) present a nice summary table when
`joyn` concludes. Also, keep in mind that `joyn` is intended to be
informative, so it displays messages here and there to inform you about
your join (you can silence any message in `joyn`, including the
reporting table, by using the argument `verbose = FALSE`).

As of now, the flexibility of `joyn` is limited to the basic joins, yet
the most used and useful ones. If you want to learn more about different
kinds of joins available in `data.table` and how they relate to `dplyr`,
I recommend you start with this [blog
post](https://atrebas.github.io/post/2019-03-03-datatable-dplyr/#joinbind-data-sets).

Thus, it is not intended to be extremely fast, even though it uses the
power of `data.table` indexing to perform the merges. The reason for
`joyn` to be a little slower is than pure `data.table` is that it will
always perform a full join of data in the same way the `Stata` does it.
Yet, the user can select what data to keep (e.g., that left \[master\],
right \[using\], inner join, or full join).

`joyn` presents several important features. The most important is the
additional variable, `report`, which summarizes the result of the join
(you can change the name if you want). In addition, it allows you to
specify the kind of join you want to perform (i.e, m:m, m:1, 1:m, or
1:1). This ensures that your join does not produce unexpected outcome.

## Installation

<!-- You can install the released version of joyn from [CRAN](https://CRAN.R-project.org) with: -->
<!-- ``` r -->
<!-- install.packages("joyn") -->
<!-- ``` -->

The development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("randrescastaneda/joyn")
```

## Example

``` r
library(joyn)
#> 
#> Attaching package: 'joyn'
#> The following object is masked from 'package:base':
#> 
#>     merge

x1
#>    id  t  x
#> 1:  1  1 11
#> 2:  1  2 12
#> 3:  2  1 13
#> 4:  3  2 14
#> 5: NA NA 15
y1
#>    id  y
#> 1:  1 11
#> 2:  2 15
#> 3:  4 16

# using commong variable `id` as key.
merge(x1, y1)[]
#> i joining by `id`
#> 
#> -- JOYn Report --
#> 
#>  report n percent
#>       x 2   33.3%
#>   x & y 3   50.0%
#>       y 1   16.7%
#>   Total 6  100.0%
#> ---------------------------------------------------------- End of JOYn report --
#>    id  t  x  y report
#> 1: NA NA 15 NA      x
#> 2:  1  1 11 11  x & y
#> 3:  1  2 12 11  x & y
#> 4:  2  1 13 15  x & y
#> 5:  3  2 14 NA      x
#> 6:  4 NA NA 16      y

# keep just those observations that match
merge(x1, y1, keep = "inner")[]
#> i joining by `id`
#> -- JOYn Report --
#> 
#>  report n percent
#>   x & y 3  100.0%
#>   Total 3  100.0%
#> ---------------------------------------------------------- End of JOYn report --
#>    id t  x  y report
#> 1:  1 1 11 11  x & y
#> 2:  1 2 12 11  x & y
#> 3:  2 1 13 15  x & y


x2 
#>    id  t  x
#> 1:  1  1 16
#> 2:  4  2 12
#> 3:  2  1 NA
#> 4:  3  2 NA
#> 5: NA NA 15

y2
#>    id yd  y  x
#> 1:  1  1 11 16
#> 2:  2  2 15 17
#> 3:  5  5 20 18
#> 4:  6  6 13 19
#> 5:  3  3 10 20


# Bad merge for not specifying by argument
merge(x2, y2)[]
#> i joining by `id` and `x`
#> -- JOYn Report --
#> 
#>  report n percent
#>       x 4   44.4%
#>   x & y 1   11.1%
#>       y 4   44.4%
#>   Total 9  100.0%
#> ---------------------------------------------------------- End of JOYn report --
#>    id  x  t yd  y report
#> 1: NA 15 NA NA NA      x
#> 2:  1 16  1  1 11  x & y
#> 3:  2 NA  1 NA NA      x
#> 4:  2 17 NA  2 15      y
#> 5:  3 NA  2 NA NA      x
#> 6:  3 20 NA  3 10      y
#> 7:  4 12  2 NA NA      x
#> 8:  5 18 NA  5 20      y
#> 9:  6 19 NA  6 13      y

# good merge, ignoring variable x from y
merge(x2, y2, by = "id")[]
#> i variable `x` in `y` is ignored in merge because `updateNA` and
#>   `update_values` are FALSE.
#> -- JOYn Report --
#> 
#>  report n percent
#>       x 2   28.6%
#>   x & y 3   42.9%
#>       y 2   28.6%
#>   Total 7  100.0%
#> ---------------------------------------------------------- End of JOYn report --
#>    id  t  x yd  y report
#> 1: NA NA 15 NA NA      x
#> 2:  1  1 16  1 11  x & y
#> 3:  2  1 NA  2 15  x & y
#> 4:  3  2 NA  3 10  x & y
#> 5:  4  2 12 NA NA      x
#> 6:  5 NA NA  5 20      y
#> 7:  6 NA NA  6 13      y

# update NAs in x variable form x
merge(x2, y2, by = "id", updateNA = TRUE)[]
#> 
#> -- JOYn Report --
#> 
#>      report n percent
#>  NA updated 2   28.6%
#>           x 2   28.6%
#>       x & y 1   14.3%
#>           y 2   28.6%
#>       Total 7  100.0%
#> ---------------------------------------------------------- End of JOYn report --
#>    id  t  x yd  y     report
#> 1: NA NA 15 NA NA          x
#> 2:  1  1 16  1 11      x & y
#> 3:  2  1 17  2 15 NA updated
#> 4:  3  2 20  3 10 NA updated
#> 5:  4  2 12 NA NA          x
#> 6:  5 NA 18  5 20          y
#> 7:  6 NA 19  6 13          y

# Update values in x with variables from y
merge(x2, y2, by = "id", update_values = TRUE)[]
#> 
#> -- JOYn Report --
#> 
#>       report n percent
#>   NA updated 2   28.6%
#>  not updated 2   28.6%
#>        x & y 1   14.3%
#>            y 2   28.6%
#>        Total 7  100.0%
#> ---------------------------------------------------------- End of JOYn report --
#>    id  t  x yd  y      report
#> 1: NA NA 15 NA NA not updated
#> 2:  1  1 16  1 11       x & y
#> 3:  2  1 17  2 15  NA updated
#> 4:  3  2 20  3 10  NA updated
#> 5:  4  2 12 NA NA not updated
#> 6:  5 NA 18  5 20           y
#> 7:  6 NA 19  6 13           y


# do not bring any variable from Y into x, just the report
merge(x2, y2, by = "id", yvars = NULL)[]
#> 
#> -- JOYn Report --
#> 
#>  report n percent
#>       x 2   28.6%
#>   x & y 3   42.9%
#>       y 2   28.6%
#>   Total 7  100.0%
#> ---------------------------------------------------------- End of JOYn report --
#>    id  t x.x yd  y x.y report
#> 1: NA NA  15 NA NA  NA      x
#> 2:  1  1  16  1 11  16  x & y
#> 3:  2  1  NA  2 15  17  x & y
#> 4:  3  2  NA  3 10  20  x & y
#> 5:  4  2  12 NA NA  NA      x
#> 6:  5 NA  NA  5 20  18      y
#> 7:  6 NA  NA  6 13  19      y
```
