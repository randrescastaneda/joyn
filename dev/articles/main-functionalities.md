# Main functionalities

``` r
library(joyn)
#> 
#> Attaching package: 'joyn'
#> The following object is masked from 'package:base':
#> 
#>     merge
```

## Overview

📌 In `joyn`, there are two major sets of tools to join data tables:

1.  The primary function
    [`joyn()`](https://randrescastaneda.github.io/joyn/dev/reference/joyn.md)

2.  Dplyr-like join functions:
    [`left_join()`](https://randrescastaneda.github.io/joyn/dev/reference/left_join.md),
    [`right_join()`](https://randrescastaneda.github.io/joyn/dev/reference/right_join.md),
    [`full_join()`](https://randrescastaneda.github.io/joyn/dev/reference/full_join.md),
    [`inner_join()`](https://randrescastaneda.github.io/joyn/dev/reference/inner_join.md)

This vignette will explore the main function
[`joyn()`](https://randrescastaneda.github.io/joyn/dev/reference/joyn.md).
You can read about dplyr-joins in the “**dplyr-joins”** article instead.

## General use

``` r

library(joyn)
library(data.table)

x1 <- data.table(id = c(1L, 1L, 2L, 3L, NA_integer_),
                 t  = c(1L, 2L, 1L, 2L, NA_integer_),
                 x  = 11:15)

y1 <- data.table(id = c(1,2, 4),
                 y  = c(11L, 15L, 16))


x2 <- data.table(id = c(1, 4, 2, 3, NA),
                 t  = c(1L, 2L, 1L, 2L, NA_integer_),
                 x  = c(16, 12, NA, NA, 15))


y2 <- data.table(id = c(1, 2, 5, 6, 3),
                 yd = c(1, 2, 5, 6, 3),
                 y  = c(11L, 15L, 20L, 13L, 10L),
                 x  = c(16:20))

x3 <- data.table(id  = c("c","b", "d", "d"),
                 v   = 8:11,
                 foo = c(4,2, 7, 3))

y3 <- data.table(id = c("c","b", "c", "a"),
                 y  = c(11L, 15L, 18L, 20L))


x4 <- data.table(id1 = c(1, 1, 2, 3, 3),
                 id2 = c(1, 1, 2, 3, 4),
                 t   = c(1L, 2L, 1L, 2L, NA_integer_),
                 x   = c(16, 12, NA, NA, 15))

y4 <- data.table(id  = c(1, 2, 5, 6, 3),
                 id2 = c(1, 1, 2, 3, 4),
                 y   = c(11L, 15L, 20L, 13L, 10L),
                 x   = c(16:20))


x5 <- data.table(id      = c(1, 4, 2, 3, NA),
                 t       = c(1L, 2L, 1L, 2L, NA),
                 country = c(16, 12, 3, NA, 15))
  
y5 <- data.table(id      = c(1, 2, 2, 6, 3),
                 gdp     = c(11L, 15L, 20L, 13L, 10L),
                 country = 16:20)
```

### The basics

Let’s suppose that you want to join the two tables `x1` and `y1`.

``` r

# Calling joyn() to join x1 and y1

joyn(x = x1,
     y = y1, 
     match_type = "m:1" ) #Note RT: remove this argument once fixing the default value
#>       id     t     x     y  .joyn
#>    <num> <int> <int> <num> <fctr>
#> 1:     1     1    11    11  x & y
#> 2:     1     2    12    11  x & y
#> 3:     2     1    13    15  x & y
#> 4:     3     2    14    NA      x
#> 5:    NA    NA    15    NA      x
#> 6:     4    NA    NA    16      y
```

The output table is the result of a *full join* -which is what `joyn`
always executes by the default. This means that the returning table will
retains both matching and non matching rows from both `x1` and `y1`.
Notice that the resulting table also contains an additional variable
called `.joyn`, which is the *reporting variable*. (Read below ⬇️)

#### Reporting variable

A particular feature of `joyn` is that it includes the **`reportvar`**
in the returning table, which informs you about the status of the join.
You can modify both the name and the format of the reporting variable as
follows:

- Name: by default `reportvar = ".joyn"`, but you can modify it with
  `reportvar = "myname"` specifying the name you want to assign

- Format: by default `reporttype = "character"` , but you can also set
  it to numeric using `reporttype = "numeric"`

You can see the difference between the two types in the table
below[¹](#fn1):

| numeric |   character   | meaning                                                                                 |
|:--------|:-------------:|:----------------------------------------------------------------------------------------|
| 1       |       x       | Obs only available in x table                                                           |
| 2       |       y       | Obs only available in y table                                                           |
| 3       |     x & y     | Matching obs available in both tables                                                   |
| 4       |  NA updated   | NAs in x updated with actual values in variables with same names in y                   |
| 5       | value updated | Actual values and NAs in x updated with actual values in variables with same names in y |
| 6       |  not updated  | Actual values and NAs in x are NOT updated with actual values in y                      |

#### Key variables

When performing a join, you might want to specify which variable(s)
`joyn` should join by.

While by default `joyn` will consider the variable(s) in common between
`x` and `y` as key(s) for the join, our suggestion is to make the keys
explicit - i.e., specifying it/them in the `by` argument

``` r

# Join with one variable in common

joyn(x = x1,
     y = y1, 
     by = "id", 
     match_type = "m:1")
#>       id     t     x     y  .joyn
#>    <num> <int> <int> <num> <fctr>
#> 1:     1     1    11    11  x & y
#> 2:     1     2    12    11  x & y
#> 3:     2     1    13    15  x & y
#> 4:     3     2    14    NA      x
#> 5:    NA    NA    15    NA      x
#> 6:     4    NA    NA    16      y
```

If you don’t want to join by all variables in common between `x` and
`y`, you can alternately use equivalency as an element of `by` vector.
This specification allows you to join on different variables between `x`
and `y.`

``` r

joyn(x = x4, 
     y = y4, 
     by = c("id1 = id", "id2"), 
     match_type = "m:m")
#>      id1   id2     t     x     y  .joyn
#>    <num> <num> <int> <num> <int> <fctr>
#> 1:     1     1     1    16    11  x & y
#> 2:     1     1     2    12    11  x & y
#> 3:     2     2     1    NA    NA      x
#> 4:     3     3     2    NA    NA      x
#> 5:     3     4    NA    15    10  x & y
#> 6:     2     1    NA    NA    15      y
#> 7:     5     2    NA    NA    20      y
#> 8:     6     3    NA    NA    13      y
```

Also, notice that `joyn` will `sort` the resulting table by key
variables in `by`. This is because `sort = TRUE` by default.

### Match type

💡Match type refers to the relationship that exists between the
observations of the joining tables. The possibility to perform joins
based on the match type is one of the value added of using `joyn`.

Following Stata’s convention, we can have four different match types:

1.  **1:1** (one to one): the ***default***[²](#fn2), the variables
    specified in `by` variables uniquely identify single observations in
    both table –\> each observation in left table has a unique match in
    the right table and viceversa

2.  **1:m** (one to many): only left table is uniquely identified by
    `by`variables –\> each observation in `by` var of the left table can
    have multiple matches in `by` var of the right table

3.  **m:1** (many to one): only right table is uniquely identified by
    `by`var -\> each observation in left table can have only one match
    in the right table but observations in the right table might have
    multiple matches in the left table

4.  **m:m** (many to many): variables in `by` does not uniquely identify
    the observations in either table –\> both tables can have multiple
    matches for each observation

We recommend you always specify the match type when joining tables to
ensure the output is correct.

``` r

# Many to one match type
joyn(x = x1,
     y = y1,
     by = "id",
     match_type = "m:1")
#>       id     t     x     y  .joyn
#>    <num> <int> <int> <num> <fctr>
#> 1:     1     1    11    11  x & y
#> 2:     1     2    12    11  x & y
#> 3:     2     1    13    15  x & y
#> 4:     3     2    14    NA      x
#> 5:    NA    NA    15    NA      x
#> 6:     4    NA    NA    16      y

# Many to many match type
joyn(x = x3,
     y = y3,
     by = "id",
     match_type = "m:m")
#>        id     v   foo     y  .joyn
#>    <char> <int> <num> <int> <fctr>
#> 1:      c     8     4    11  x & y
#> 2:      c     8     4    18  x & y
#> 3:      b     9     2    15  x & y
#> 4:      d    10     7    NA      x
#> 5:      d    11     3    NA      x
#> 6:      a    NA    NA    20      y

# One to one match type - the default
joyn(x = x2,
     y = y2,
     by = "id",
     match_type = "1:1")
#>       id     t     x    yd     y  .joyn
#>    <num> <int> <num> <num> <int> <fctr>
#> 1:     1     1    16     1    11  x & y
#> 2:     4     2    12    NA    NA      x
#> 3:     2     1    NA     2    15  x & y
#> 4:     3     2    NA     3    10  x & y
#> 5:    NA    NA    15    NA    NA      x
#> 6:     5    NA    NA     5    20      y
#> 7:     6    NA    NA     6    13      y

# Same join as:

joyn(x = x2,
     y = y2,
     by = "id")
#>       id     t     x    yd     y  .joyn
#>    <num> <int> <num> <num> <int> <fctr>
#> 1:     1     1    16     1    11  x & y
#> 2:     4     2    12    NA    NA      x
#> 3:     2     1    NA     2    15  x & y
#> 4:     3     2    NA     3    10  x & y
#> 5:    NA    NA    15    NA    NA      x
#> 6:     5    NA    NA     5    20      y
#> 7:     6    NA    NA     6    13      y


# One to many match type 
joyn(x = x5,
     y = y5,
     by = "id",
     match_type = "1:m")
#>       id     t country   gdp  .joyn
#>    <num> <int>   <num> <int> <fctr>
#> 1:     1     1      16    11  x & y
#> 2:     4     2      12    NA      x
#> 3:     2     1       3    15  x & y
#> 4:     2     1       3    20  x & y
#> 5:     3     2      NA    10  x & y
#> 6:    NA    NA      15    NA      x
#> 7:     6    NA      NA    13      y
```

However, if are unsure/wrong about the relationships between the
observations in your tables, `joyn` will let you know that something is
not right. Suppose you think your data is uniquely identified by
variable `id`, while it is not. By setting `match_type = "1:1"` you will
get and error, informing you that the match type is not as expected.

``` r

# Merging correctly but getting error because something is not right in the data
joyn(x3, y3, by = "id", match_type = "1:1")
#> ✖ Error: table x is not uniquely identified by id
#> ✖ Error: table y is not uniquely identified by id
#> Error in `check_match_type()`:
#> ! match type inconsistency
#> ℹ set verbose to TRUE to see where the issue is

# Merging wrongly but getting NO errors because you did not use match_type
joyn(x3, y3, by = "id")
#> ✖ Error: table x is not uniquely identified by id
#> ✖ Error: table y is not uniquely identified by id
#> Error in `check_match_type()`:
#> ! match type inconsistency
#> ℹ set verbose to TRUE to see where the issue is
```

If instead you don’t care about match types or you don’t think it is
necessary to use them for your particular needs, you might be fine
without `joyn`.

### Type of join

Join type determines which observations will be kept after the join.
[`joyn()`](https://randrescastaneda.github.io/joyn/dev/reference/joyn.md)
allows you to choose which type of join to execute via the `keep`
argument.

This argument is called `keep` rather than `join_type` to avoid
confusion with the argument `match_type`, and in order to reflect that
what you are specifying in the end is which observations you want to
keep. This argument plays the role of allowing
[`joyn()`](https://randrescastaneda.github.io/joyn/dev/reference/joyn.md)
to mimic the behavior of `dplyr`’s functions `left_join`, `right_join`,
`inner_join`, and `full_join`, the default.

`keep` can be of four types:

- **`keep = "full"`**: the default, which keeps *all* the observations
  in x and y, regardless of whether they match or not.

  ``` r

  # Full join 

  joyn(x          = x1, 
       y          = y1, 
       match_type = "m:m")
  #>       id     t     x     y  .joyn
  #>    <num> <int> <int> <num> <fctr>
  #> 1:     1     1    11    11  x & y
  #> 2:     1     2    12    11  x & y
  #> 3:     2     1    13    15  x & y
  #> 4:     3     2    14    NA      x
  #> 5:    NA    NA    15    NA      x
  #> 6:     4    NA    NA    16      y
  ```

- **`keep = "left"`** or **`keep = "master"`** : keeps all observations
  in `x`, both matching and non, and only those observations in `y` that
  match in `x`

  ``` r

  # keep obs in x

  joyn(x          = x1, 
       y          = y1, 
       keep       = "left", 
       match_type = "m:m")
  #>       id     t     x     y  .joyn
  #>    <num> <int> <int> <num> <fctr>
  #> 1:     1     1    11    11  x & y
  #> 2:     1     2    12    11  x & y
  #> 3:     2     1    13    15  x & y
  #> 4:     3     2    14    NA      x
  #> 5:    NA    NA    15    NA      x
  ```

- **`keep = "right"`** or **`keep = "using"`** keeps all observations in
  `y`, both matching and non, and only those observations in `x` that
  match in `y`

  ``` r

  # keep obs in y

  joyn(x          = x1, 
       y          = y1,
       keep       = "right", 
       match_type = "m:m")
  #>       id     t     x     y  .joyn
  #>    <num> <int> <int> <num> <fctr>
  #> 1:     1     1    11    11  x & y
  #> 2:     1     2    12    11  x & y
  #> 3:     2     1    13    15  x & y
  #> 4:     4    NA    NA    16      y
  ```

- **`keep = "inner"`** keeps only those observations that match in
  *both* tables.

  ``` r

  # keep matching obs in both tables

  joyn(x1, y1, keep = "inner", match_type = "m:m")
  #>       id     t     x     y  .joyn
  #>    <num> <int> <int> <num> <fctr>
  #> 1:     1     1    11    11  x & y
  #> 2:     1     2    12    11  x & y
  #> 3:     2     1    13    15  x & y
  ```

## An important feature: JOYn report and info display

Recall that `joyn` is intended to be informative about the status and
quality of the merging.

📊 **JOYn report**

By default, `joyn` returns the *JOYn report* , i.e., a summary table of
the merging. This includes the reporting variable, the number of rows
that come from `x`, the number of rows that come from `y` and those that
are common to both `x` and `y`. This info is also shown in percentage
form in the *percent* column.

``` r

joyn(x = x3, 
     y = y3, 
     by = "id", 
     match_type = "m:m",
     verbose = TRUE)
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
#> ℹ Note: Removing key variables id from id and y
#>        id     v   foo     y  .joyn
#>    <char> <int> <num> <int> <fctr>
#> 1:      c     8     4    11  x & y
#> 2:      c     8     4    18  x & y
#> 3:      b     9     2    15  x & y
#> 4:      d    10     7    NA      x
#> 5:      d    11     3    NA      x
#> 6:      a    NA    NA    20      y
```

📝 **Displaying messages**

One of the value added of `joyn` is that it produces a number of
messages that are intended to inform you about the status of the join.
The display of such messages is controlled by the argument `verbose`,
which allows you to show (`verbose = TRUE`) or silent
(`verbose = FALSE`) any messages.

To further explore messages in `joyn`, please refer to the
“**Messages”** article.

------------------------------------------------------------------------

1.  For a better understanding of the *meaning* column check the
    “Advanced functionalities” article

2.  Notice that in the previous version default match type was “m:m”
    instead.
