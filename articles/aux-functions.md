# Additional functions

``` r

library(joyn)
#> 
#> Attaching package: 'joyn'
#> The following object is masked from 'package:base':
#> 
#>     merge
library(data.table)
```

This vignette will give you a brief overview of how you can use some
auxiliary functions that `joyn` makes available to the user.

## Verifying if dt is uniquely identified

One of the advantages of `joyn` is that you can perform one-to-one
(1:1), one-to-many (1:m), many-to-one (m:1), and many-to-many (m:m)
joins.
[`is_id()`](https://randrescastaneda.github.io/joyn/reference/is_id.md)
is a function that might come in handy when you want to check whether
your data table is uniquely identified by the variables you want to
merge by. In fact this is what
[`is_id()`](https://randrescastaneda.github.io/joyn/reference/is_id.md)
checks by default, returning either TRUE or FALSE depending on whether
the data table is uniquely identified or not. Alternatively, you can set
`return_report = FALSE` to get a summary of the duplicates.

``` r

x1 <- data.table(id = c(1L, 1L, 2L, 3L, NA_integer_),
                 t  = c(1L, 2L, 1L, 2L, NA_integer_),
                 x  = 11:15,
                 c  = c("a", "b", "a", "t", "d"),
                 c1 = c("h", "j", "k", "l", "y"))

y1 <- data.table(id = c(1,2, 4),
                 y  = c(11L, 15L, 16))

# Checking if x1 is uniquely identified by "id" with return_report = TRUE

is_id(dt = x1, 
      by = "id")
#> ! Duplicates found by: `id`
#> [1] FALSE

# Checking duplicates in x1 with return_report = FALSE

is_id(dt = x1, 
      by = "id", 
      return_report = FALSE)
#> ! Duplicates found by: `id`
#> [1] FALSE
```

## Possible unique identifiers

In `joyn`, you can also search for variables which possibly uniquely
identify your data table `x` using the
[`possible_ids()`](https://randrescastaneda.github.io/joyn/reference/possible_ids.md)
function. For example,

``` r

# Identify possible unique identifier excluding variable t
possible_ids(dt      = x1, 
             exclude = "t")
#> [[1]]
#> [1] "x"
#> 
#> [[2]]
#> [1] "c1"
#> 
#> attr(,"checked_ids")
#> [1] "id" "c"  "x"  "c1"

# Identify possible unique identifier excluding character variables
possible_ids(dt      = x1, 
             exclude = "_character")
#> [[1]]
#> [1] "x"
#> 
#> [[2]]
#> [1] "c1"
#> 
#> attr(,"checked_ids")
#> [1] "t"  "id" "c"  "x"  "c1"

# Identify possible unique identifiers, excluding character variables but considering variable c1
possible_ids(dt      = x1, 
             exclude_classes = "character",
             include = "c1")
#> [[1]]
#> [1] "x"
#> 
#> [[2]]
#> [1] "c1"
#> 
#> attr(,"checked_ids")
#> [1] "t"  "id" "x"  "c1"
```

## Verifying if data table is balanced

Additionally, `joyn` makes available to the user the
[`is_balanced()`](https://randrescastaneda.github.io/joyn/reference/is_balanced.md)
function. This is instrumental in assessing the completeness of the data
table within a specified group, i.e., if the table contains all the
combinations of observations in the group. By default,
[`is_balanced()`](https://randrescastaneda.github.io/joyn/reference/is_balanced.md)
will tell you if/if not the table is balanced. However, if you set
`return = "table"`, you will get a summary of the unbalanced
observations. In other words, those combinations of elements between the
specified variables that is not contained in the input table.

``` r

# Example with return = "logic", the default

is_balanced(df = x1,
            by = c("id", "t"))
#> [1] FALSE

# Example with return = "table"
is_balanced(df = x1,
            by = c("id", "t"), 
            return = "table")
#>   id t
#> 1  3 1
#> 2  2 2
```

## Tabulating simple frequencies

Furthermore, `joyn` provides a function that generates simple frequency
tables, so that you can easily have an overview of the distribution of
values within your data tables.

``` r

# Tabulating frequencies of var `id`

freq_table(x     = x1, 
           byvar = "id")[]
#>        id     n percent
#>    <char> <int>  <char>
#> 1:      1     2     40%
#> 2:      2     1     20%
#> 3:      3     1     20%
#> 4:   <NA>     1     20%
#> 5:  total     5    100%

# Removing NAs from the calculation

freq_table(x     = x1, 
           byvar = "id", 
           na.rm = TRUE)[]
#>        id     n percent
#>    <char> <int>  <char>
#> 1:      1     2     50%
#> 2:      2     1     25%
#> 3:      3     1     25%
#> 4:  total     4    100%
```
