
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
```
