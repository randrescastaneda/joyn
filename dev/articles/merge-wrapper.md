# \`data.table::merge()\` wrapper

``` r

library(joyn)
#> 
#> Attaching package: 'joyn'
#> The following object is masked from 'package:base':
#> 
#>     merge
library(data.table)

 x1 = data.table(id = c(1L, 1L, 2L, 3L, NA_integer_),
                 t  = c(1L, 2L, 1L, 2L, NA_integer_),
                 x  = 11:15)
 y1 = data.table(id = c(1,2, 4),
                 y  = c(11L, 15L, 16))
 
 x2 = data.table(id1 = c(1, 1, 2, 3, 3),
                 id2 = c(1, 1, 2, 3, 4),
                 t   = c(1L, 2L, 1L, 2L, NA_integer_),
                 x   = c(16, 12, NA, NA, 15))
 
 y2 = data.table(id  = c(1, 2, 5, 6, 3),
                 id2 = c(1, 1, 2, 3, 4),
                 y   = c(11L, 15L, 20L, 13L, 10L),
                 x   = c(16:20))
 
```

This vignette describes the use of the `joyn`
[`merge()`](https://randrescastaneda.github.io/joyn/dev/reference/merge.md)
function.

🔀
[`joyn::merge`](https://randrescastaneda.github.io/joyn/dev/reference/merge.md)
resembles the usability of
[`base::merge`](https://rdrr.io/r/base/merge.html) and
[`data.table::merge`](https://rdatatable.gitlab.io/data.table/reference/merge.html),
while also incorporating the additional features that characterize
`joyn`. In fact,
[`joyn::merge`](https://randrescastaneda.github.io/joyn/dev/reference/merge.md)
masks the other two.

### Examples

#### Simple merge

Suppose you want to merge `x1` and `y1`. First notice that while
[`base::merge`](https://rdrr.io/r/base/merge.html) is principally for
data frames,
[`joyn::merge`](https://randrescastaneda.github.io/joyn/dev/reference/merge.md)
coerces `x` and `y` to data tables if they are not already.

By default, `merge` will join by the shared column name(s) in `x` and
`y`.

``` r

# Example not specifying the key
merge(x = x1, 
      y = y1)
#> 
#> ── JOYn Report ──
#> 
#>   .joyn n percent
#> 1     x 2   66.7%
#> 2     y 1   33.3%
#> 3 total 3    100%
#> ────────────────────────────────────────────────────────── End of JOYn report ──
#> ℹ Note: Joyn's report available in variable .joyn
#> ℹ Note: Removing key variables id from id and y
#> ⚠ Warning: The keys supplied uniquely identify y, therefore a m:1 join is
#> executed
#>       id     t     x     y  .joyn
#>    <num> <int> <int> <num> <fctr>
#> 1:     1     1    11    11  x & y
#> 2:     1     2    12    11  x & y
#> 3:     2     1    13    15  x & y

# Example specifying the key
merge(x = x1, 
      y = y1,
      by = "id")
#> 
#> ── JOYn Report ──
#> 
#>   .joyn n percent
#> 1     x 2   66.7%
#> 2     y 1   33.3%
#> 3 total 3    100%
#> ────────────────────────────────────────────────────────── End of JOYn report ──
#> ℹ Note: Joyn's report available in variable .joyn
#> ℹ Note: Removing key variables id from id and y
#> ⚠ Warning: The keys supplied uniquely identify y, therefore a m:1 join is
#> executed
#>       id     t     x     y  .joyn
#>    <num> <int> <int> <num> <fctr>
#> 1:     1     1    11    11  x & y
#> 2:     1     2    12    11  x & y
#> 3:     2     1    13    15  x & y
```

As usual, if the columns you want to join by don’t have the same name,
you need to tell merge which columns you want to join by: `by.x` for the
x data frame column name, and `by.y` for the y one. For example,

``` r

df1 <- data.frame(id = c(1L, 1L, 2L, 3L, NA_integer_, NA_integer_),
                  t  = c(1L, 2L, 1L, 2L, NA_integer_, 4L),
                  x  = 11:16)

df2 <- data.frame(id = c(1,2, 4, NA_integer_, 8),
                  y  = c(11L, 15L, 16, 17L, 18L),
                  t  = c(13:17))

merge(x    = df1,
      y    = df2,
      by.x = "x",
      by.y = "y")
#> 
#> ── JOYn Report ──
#> 
#>   .joyn n percent
#> 1     x 3    100%
#> 2     y 2   66.7%
#> 3 total 3    100%
#> ────────────────────────────────────────────────────────── End of JOYn report ──
#> ℹ Note: Joyn's report available in variable .joyn
#> ℹ Note: Removing key variables keyby1 from id, keyby1, and t
#> ⚠ Warning: The keys supplied uniquely identify both x and y, therefore a 1:1
#> join is executed
#>   id.x t.x  x id.y t.y .joyn
#> 1    1   1 11    1  13 x & y
#> 2   NA  NA 15    2  14 x & y
#> 3   NA   4 16    4  15 x & y
```

By default, `sort` is `TRUE`, so that the merged table will be sorted by
the `by.x` column. Notice that the output table distinguishes non-by
column *t* coming from `x` from the one coming from `y` by adding the
*.x* and *.y* suffixes -which occurs because the `no.dups` argument is
set to `TRUE` by default.

#### Going further

In a similar fashion as the
[`joyn()`](https://randrescastaneda.github.io/joyn/dev/reference/joyn.md)
primary function does,
[`merge()`](https://randrescastaneda.github.io/joyn/dev/reference/merge.md)
offers a number of arguments to verify/control the merge[¹](#fn1).

For example,
[`joyn::joyn`](https://randrescastaneda.github.io/joyn/dev/reference/joyn.md)
allows to execute one-to-one, one-to-many, many-to-one and many-to-many
joins. Similarly, `merge` accepts the `match_type` argument:

``` r

# Example with many to many merge
joyn::merge(x          = x2,
            y          = y2,
            by.x       = "id1",
            by.y       = "id2",
            match_type = "m:m")
#> ⚠ Warning:  Supplied both by and by.x/by.y. by argument will be ignored.
#> 
#> ── JOYn Report ──
#> 
#>   .joyn n percent
#> 1     y 1   14.3%
#> 2 x & y 6   85.7%
#> 3 total 7    100%
#> ────────────────────────────────────────────────────────── End of JOYn report ──
#> ℹ Note: Joyn's report available in variable .joyn
#> ℹ Note: Removing key variables keyby1 from id, keyby1, y, and x
#> ⚠ Warning:  Supplied both by and by.x/by.y. by argument will be ignored.
#>      id1   id2     t   x.x    id     y   x.y  .joyn
#>    <num> <num> <int> <num> <num> <int> <int> <fctr>
#> 1:     1     1     1    16     1    11    16  x & y
#> 2:     1     1     1    16     2    15    17  x & y
#> 3:     1     1     2    12     1    11    16  x & y
#> 4:     1     1     2    12     2    15    17  x & y
#> 5:     2     2     1    NA     5    20    18  x & y
#> 6:     3     3     2    NA     6    13    19  x & y
#> 7:     3     4    NA    15     6    13    19  x & y

# Example with many to many merge
joyn::merge(x          = x1,
            y          = y1,
            by         = "id",
            match_type = "m:1")
#> ⚠ Warning:  Supplied both by and by.x/by.y. by argument will be ignored.
#> 
#> ── JOYn Report ──
#> 
#>   .joyn n percent
#> 1     x 2   66.7%
#> 2     y 1   33.3%
#> 3 total 3    100%
#> ────────────────────────────────────────────────────────── End of JOYn report ──
#> ℹ Note: Joyn's report available in variable .joyn
#> ℹ Note: Removing key variables id from id and y
#> ⚠ Warning:  Supplied both by and by.x/by.y. by argument will be ignored.
#>       id     t     x     y  .joyn
#>    <num> <int> <int> <num> <fctr>
#> 1:     1     1    11    11  x & y
#> 2:     1     2    12    11  x & y
#> 3:     2     1    13    15  x & y
```

In a similar way, you can exploit all the other additional options
available in
[`joyn()`](https://randrescastaneda.github.io/joyn/dev/reference/joyn.md),
e.g., for keeping common variables, updating NAs and values, displaying
messages etc…, which you can explore in the “Advanced functionalities”
article.

------------------------------------------------------------------------

1.  See the “Advanced functionalities” article for more details
