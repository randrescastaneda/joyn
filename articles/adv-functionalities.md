# Advanced functionalities

``` r

library(joyn)
#> 
#> Attaching package: 'joyn'
#> The following object is masked from 'package:base':
#> 
#>     merge
library(data.table)

x <- data.table(id = c(1, 4, 2, 3, NA),
                t  = c(1L, 2L, 1L, 2L, NA),
                country = c(16, 12, 3, NA, 15))
  
y <- data.table(id  = c(1, 2, 5, 6, 3),
                gdp = c(11L, 15L, 20L, 13L, 10L),
                country = 16:20)
```

## Advanced use

This vignette will let you explore some additional features available in
`joyn`, through an example use case.

Suppose you want to join tables `x` and `y`, where the variable
*country* is available in both. You could do one of five things:

### 1. Use variable *country* as one of the key variables

If you don’t use the argument `by`, `joyn` will consider *country* and
*id* as key variables by default given that they are common between `x`
and `y`.

``` r

# The variables with the same name, `id` and `country`, are used as key
# variables.

joyn(x = x, 
     y = y)
#> 
#> ── JOYn Report ──
#> 
#>   .joyn n percent
#> 1     x 4   44.4%
#> 2     y 4   44.4%
#> 3 x & y 1   11.1%
#> 4 total 9    100%
#> ────────────────────────────────────────────────────────── End of JOYn report ──
#> ℹ Note: Joyn's report available in variable .joyn
#> ℹ Note: Removing key variables id and country from id, gdp, and country
#>       id     t country   gdp  .joyn
#>    <num> <int>   <num> <int> <fctr>
#> 1:     1     1      16    11  x & y
#> 2:     4     2      12    NA      x
#> 3:     2     1       3    NA      x
#> 4:     3     2      NA    NA      x
#> 5:    NA    NA      15    NA      x
#> 6:     2    NA      17    15      y
#> 7:     5    NA      18    20      y
#> 8:     6    NA      19    13      y
#> 9:     3    NA      20    10      y
```

Alternatively, you can specify to join by *country*

``` r

# Joining by country

joyn(x = x, 
     y = y, 
     by = "country")
#> 
#> ── JOYn Report ──
#> 
#>   .joyn n percent
#> 1     x 4   44.4%
#> 2     y 4   44.4%
#> 3 x & y 1   11.1%
#> 4 total 9    100%
#> ────────────────────────────────────────────────────────── End of JOYn report ──
#> ℹ Note: Joyn's report available in variable .joyn
#> ℹ Note: Removing key variables country from id, gdp, and country
#>       id     t country   gdp  .joyn
#>    <num> <int>   <num> <int> <fctr>
#> 1:     1     1      16    11  x & y
#> 2:     4     2      12    NA      x
#> 3:     2     1       3    NA      x
#> 4:     3     2      NA    NA      x
#> 5:    NA    NA      15    NA      x
#> 6:    NA    NA      17    15      y
#> 7:    NA    NA      18    20      y
#> 8:    NA    NA      19    13      y
#> 9:    NA    NA      20    10      y
```

### 2. Ignore the values of *country* from `y` and don’t bring it into the resulting table

This the default if you did not include *country* as part of the key
variables in argument `by`.

``` r

joyn(x = x, 
     y = y, 
     by = "id")
#> 
#> ── JOYn Report ──
#> 
#>   .joyn n percent
#> 1     x 2   28.6%
#> 2     y 2   28.6%
#> 3 x & y 3   42.9%
#> 4 total 7    100%
#> ────────────────────────────────────────────────────────── End of JOYn report ──
#> ℹ Note: Joyn's report available in variable .joyn
#> ℹ Note: Removing key variables id from id, gdp, and country
#>       id     t country   gdp  .joyn
#>    <num> <int>   <num> <int> <fctr>
#> 1:     1     1      16    11  x & y
#> 2:     4     2      12    NA      x
#> 3:     2     1       3    15  x & y
#> 4:     3     2      NA    10  x & y
#> 5:    NA    NA      15    NA      x
#> 6:     5    NA      NA    20      y
#> 7:     6    NA      NA    13      y
```

### 3. Update only NAs in table x

Another possibility is to make use of the `update_NAs` argument of
[`joyn()`](https://randrescastaneda.github.io/joyn/reference/joyn.md).
This allows you to update the NAs values in variable *country* in table
`x` with the actual values of the matching observations in *country*
from table y. In this case, actual values in *country* from table x will
remain unchanged.

``` r

joyn(x = x,
     y = y, 
     by = "id", 
     update_NAs = TRUE)
#> 
#> ── JOYn Report ──
#> 
#>         .joyn     n percent
#>        <char> <int>  <char>
#> 1:          x     2   28.6%
#> 2:      x & y     2   28.6%
#> 3: NA updated     3   42.9%
#> 4:      total     7    100%
#> ────────────────────────────────────────────────────────── End of JOYn report ──
#> ℹ Note: Joyn's report available in variable .joyn
#> ℹ Note: Removing key variables id from id, gdp, and country
#>       id     t country   gdp      .joyn
#>    <num> <int>   <num> <int>     <fctr>
#> 1:     1     1      16    11      x & y
#> 2:     4     2      12    NA          x
#> 3:     2     1       3    15      x & y
#> 4:     3     2      20    10 NA updated
#> 5:    NA    NA      15    NA          x
#> 6:     5    NA      18    20 NA updated
#> 7:     6    NA      19    13 NA updated
```

### 4. Update actual values in table x

You can also update all the values - both NAs and actual - in variable
*country* of table `x` with the actual values of the matching
observations in *country* from `y`. This is done by setting
`update_values = TRUE`.

Notice that the `reportvar` allows you keep track of how the update
worked. In this case, *value update* means that only the values that are
different between *country* from `x` and *country* from `y` are updated.

However, let’s consider other possible cases:

- If, for the same matching observations, the values between the two
  *country* variables were the same, the reporting variable would report
  *x & y* instead (so you know that there is no update to make).

- if there are NAs in *country* from `y`, the actual values in `x` will
  be unchanged, and you would see a *not updated* status in the
  reporting variable. Nevertheless, notice there is another way for you
  to bring *country* from `y` to `x`. This is done through the argument
  `keep_y_in_x` (*see 5. below* ⬇️)

``` r

# Notice that only the value that are 

joyn(x = x, 
     y = y, 
     by = "id", 
     update_values = TRUE)
#> 
#> ── JOYn Report ──
#> 
#>            .joyn     n percent
#>           <char> <int>  <char>
#> 1:    NA updated     3   42.9%
#> 2: value updated     2   28.6%
#> 3:   not updated     2   28.6%
#> 4:         total     7    100%
#> ────────────────────────────────────────────────────────── End of JOYn report ──
#> ℹ Note: Joyn's report available in variable .joyn
#> ℹ Note: Removing key variables id from id, gdp, and country
#>       id     t country   gdp         .joyn
#>    <num> <int>   <num> <int>        <fctr>
#> 1:     1     1      16    11 value updated
#> 2:     4     2      12    NA   not updated
#> 3:     2     1      17    15 value updated
#> 4:     3     2      20    10    NA updated
#> 5:    NA    NA      15    NA   not updated
#> 6:     5    NA      18    20    NA updated
#> 7:     6    NA      19    13    NA updated
```

### 5. Keep original *country* variable from y into returning table

#### (Keep matching-names variable from y into x -not updating values in x)

Another available option is that of bringing the original variable
*country* from `y` into the resulting table, without using it to update
the values in `x`. In order to distinguish *country* from `x` and
*country* from `y`, `joyn` will assign a suffix to the variable’s name:
so that you will get *country.y* and *country.x*. All of this can be
done specifying `keep_common_vars = TRUE.`

``` r

joyn(x = x, 
     y = y, 
     by = "id", 
     keep_common_vars = TRUE)
#> 
#> ── JOYn Report ──
#> 
#>   .joyn n percent
#> 1     x 2   28.6%
#> 2     y 2   28.6%
#> 3 x & y 3   42.9%
#> 4 total 7    100%
#> ────────────────────────────────────────────────────────── End of JOYn report ──
#> ℹ Note: Joyn's report available in variable .joyn
#> ℹ Note: Removing key variables id from id, gdp, and country
#>       id     t country.x   gdp country.y  .joyn
#>    <num> <int>     <num> <int>     <int> <fctr>
#> 1:     1     1        16    11        16  x & y
#> 2:     4     2        12    NA        NA      x
#> 3:     2     1         3    15        17  x & y
#> 4:     3     2        NA    10        20  x & y
#> 5:    NA    NA        15    NA        NA      x
#> 6:     5    NA        NA    20        18      y
#> 7:     6    NA        NA    13        19      y
```

### Bring other variables from y into returning table

In `joyn` , you can also bring non common variables from `y` into the
resulting table. In fact you can specify them in `y_vars_to_keep`, as
shown in the example below:

``` r

# Keeping variable gdp 

joyn(x = x, 
     y = y, 
     by = "id", 
     y_vars_to_keep = "gdp")
#> 
#> ── JOYn Report ──
#> 
#>   .joyn n percent
#> 1     x 2   28.6%
#> 2     y 2   28.6%
#> 3 x & y 3   42.9%
#> 4 total 7    100%
#> ────────────────────────────────────────────────────────── End of JOYn report ──
#> ℹ Note: Joyn's report available in variable .joyn
#>       id     t country   gdp  .joyn
#>    <num> <int>   <num> <int> <fctr>
#> 1:     1     1      16    11  x & y
#> 2:     4     2      12    NA      x
#> 3:     2     1       3    15  x & y
#> 4:     3     2      NA    10  x & y
#> 5:    NA    NA      15    NA      x
#> 6:     5    NA      NA    20      y
#> 7:     6    NA      NA    13      y
```

Notice that if you set `y_vars_to_keep = FALSE` or
`y_vars_to_keep = NULL`, then `joyn` won’t bring any variable into the
returning table.
