
# joyn

<!-- badges: start -->
<!-- badges: end -->

The goal of `joyn` is to provide the user with a set of tools to analyze
the quality of merging (i.e., joining) data frames, so that it is a
**JOY** to join tables with `joyn`. This is inspired in the command
`merge` of the statistical software `Stata`. Thus, it is not intended to
be extremely fast, even though it uses the power of `data.table`
indexing to perform the merges. The reason for `joyn` to be a little
slower is than pure `data.table` is that it will always perform a full
join of data in the same way the `Stata` does it. Yet, the user can
select what data to keep (e.g., that left \[master\], right \[using\],
inner join, or full join).

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

# using commong variable `id` as key.
merge(x1, y1)[]
#> i joining by `id`
#> 
#> -- JOYn Report --
#> 
#>  report n percent
#>       x 2   40.0%
#>   x & y 3   60.0%
#>   Total 5  100.0%
#>    id  t  x  y report
#> 1:  1  1 11 11  x & y
#> 2:  1  2 12 11  x & y
#> 3:  2  1 13 15  x & y
#> 4:  3  2 14 NA      x
#> 5: NA NA 15 NA      x

# keep just those observations that match
merge(x1, y1, keep = "inner")[]
#> i joining by `id`
#> -- JOYn Report --
#> 
#>  report n percent
#>   x & y 3  100.0%
#>   Total 3  100.0%
#>    id t  x  y report
#> 1:  1 1 11 11  x & y
#> 2:  1 2 12 11  x & y
#> 3:  2 1 13 15  x & y


x2 
#>    id  t  x
#> 1:  1  1 16
#> 2:  1  2 12
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
#>    id  t  x yd  y report
#> 1:  1  1 16  1 11  x & y
#> 2:  1  2 12 NA NA      x
#> 3:  2  1 NA NA NA      x
#> 4:  3  2 NA NA NA      x
#> 5: NA NA 15 NA NA      x
#> 6:  2 NA 17  2 15      y
#> 7:  5 NA 18  5 20      y
#> 8:  6 NA 19  6 13      y
#> 9:  3 NA 20  3 10      y

# good merge, ignoring variable x from y
merge(x2, y2, by = "id")[]
#> i variable `x` in `y` is ignored in merge because `updateNA` and
#>   `update_values` are FALSE.
#> -- JOYn Report --
#> 
#>  report n percent
#>       x 1   14.3%
#>   x & y 4   57.1%
#>       y 2   28.6%
#>   Total 7  100.0%
#>    id  t  x yd  y report
#> 1:  1  1 16  1 11  x & y
#> 2:  1  2 12  1 11  x & y
#> 3:  2  1 NA  2 15  x & y
#> 4:  3  2 NA  3 10  x & y
#> 5: NA NA 15 NA NA      x
#> 6:  5 NA 18  5 20      y
#> 7:  6 NA 19  6 13      y

# update NAs in x variable form x
merge(x2, y2, by = "id", updateNA = TRUE)[]
#> -- JOYn Report --
#>      report n percent
#>  NA updated 2   28.6%
#>           x 1   14.3%
#>       x & y 2   28.6%
#>           y 2   28.6%
#>       Total 7  100.0%
#>    id  t  x yd  y     report
#> 1:  1  1 16  1 11      x & y
#> 2:  1  2 12  1 11      x & y
#> 3:  2  1 17  2 15 NA updated
#> 4:  3  2 20  3 10 NA updated
#> 5: NA NA 15 NA NA          x
#> 6:  5 NA 18  5 20          y
#> 7:  6 NA 19  6 13          y

# Update values in x with variables from y
merge(x2, y2, by = "id", update_values = TRUE)[]
#> -- JOYn Report --
#>         report n percent
#>    not updated 1   14.3%
#>  value updated 1   14.3%
#>          x & y 3   42.9%
#>              y 2   28.6%
#>          Total 7  100.0%
#>    id  t  x yd  y        report
#> 1:  1  1 16  1 11         x & y
#> 2:  1  2 16  1 11 value updated
#> 3:  2  1 NA  2 15         x & y
#> 4:  3  2 NA  3 10         x & y
#> 5: NA NA 15 NA NA   not updated
#> 6:  5 NA NA  5 20             y
#> 7:  6 NA NA  6 13             y
```
