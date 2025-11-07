# Messages

✅ This vignette is dedicated to one specific feature of `joyn`:
displaying information through **messages**.

We’ll start with a rough overview of the different kinds of messages
that might be generated when merging two data tables, then discuss each
of them in detail with representative examples.

## Overview

`Joyn` messages can be of 4 different types:

1.  **Info**

2.  **Timing**

3.  **Warning**

4.  **Error**

``` r

# Setup 
library(joyn)
#> 
#> Attaching package: 'joyn'
#> The following object is masked from 'package:base':
#> 
#>     merge
library(data.table)
```

``` r

# Checking available types of messages
msgs_types = joyn:::type_choices()
print(msgs_types)
#> [1] "info"   "note"   "warn"   "timing" "err"
```

### Information messages ℹ

Info messages are intended to inform you about various aspects of the
join and the data tables involved, as you can see in the examples below.

Recall that one of the additional features of `joyn` is that it returns
a reporting variable with the status of the join. Examples in this
regard include info messages that tell you in which variable it is
available the `joyn` report, or if the reporting variable is not
returned instead.

Recall that one of the additional features of joyn is that it returns a
**reporting variable** with the status of the join. Examples in this
regard include info messages that tell you in which variable it is
available the `joyn` report, or if the reporting variable is not
returned instead. Also, an info message might let you know that the name
you want to assign to the reporting variable is already present in the
returning table, so that it will be changed to a another one.

``` r

# Example dataframes

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

x3 = data.table(id1 = c(1, 1, 2, 3, 3),
                id2 = c(1, 1, 2, 3, 4),
                t   = c(1L, 2L, 1L, 2L, NA_integer_),
                x   = c(16, 12, NA, NA, 15))


y3 = data.table(id3  = c(1, 2, 5, 6, 3),
                id4 = c(1, 1, 2, 3, 4),
                y   = c(11L, 15L, 20L, 13L, 10L),
                z   = c(16:20))



# ------------------- Showing which var contains joyn report -------------------

# Joining x2 and y2
joyn(x              = x2,
     y              = y2,
     by             = "id", 
     y_vars_to_keep = FALSE)
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
#>       id     t     x  .joyn
#>    <num> <int> <num> <fctr>
#> 1:     1     1    16  x & y
#> 2:     4     2    12      x
#> 3:     2     1    NA  x & y
#> 4:     3     2    NA  x & y
#> 5:    NA    NA    15      x
#> 6:     5    NA    NA      y
#> 7:     6    NA    NA      y

# Printing the info message
joyn_msg(msg_type = "info")
#> ℹ Note: Joyn's report available in variable .joyn

# ---------------- Info about change in reporting variable name ----------------
joyn(x              = x2,
     y              = y2,
     by             = "id", 
     reportvar      = "x",
     y_vars_to_keep = FALSE)
#> 
#> ── JOYn Report ──
#> 
#>     x.1 n percent
#> 1     x 2   28.6%
#> 2     y 2   28.6%
#> 3 x & y 3   42.9%
#> 4 total 7    100%
#> ────────────────────────────────────────────────────────── End of JOYn report ──
#> ℹ Note: Joyn's report available in variable x
#> ℹ Note: reportvar x is already part of the resulting table. It will be changed
#> to x.1
#>       id     t     x    x.1
#>    <num> <int> <num> <fctr>
#> 1:     1     1    16  x & y
#> 2:     4     2    12      x
#> 3:     2     1    NA  x & y
#> 4:     3     2    NA  x & y
#> 5:    NA    NA    15      x
#> 6:     5    NA    NA      y
#> 7:     6    NA    NA      y

joyn_msg(msg_type = "info")
#> ℹ Note: Joyn's report available in variable x
#> ℹ Note: reportvar x is already part of the resulting table. It will be changed
#> to x.1

# ------------- Informing that reporting variable is not returned -------------
joyn(x              = x2,
     y              = y2,
     by             = "id", 
     reportvar      = FALSE,
     y_vars_to_keep = FALSE)
#> 
#> ── JOYn Report ──
#> 
#>   .joyn n percent
#> 1     x 2   28.6%
#> 2     y 2   28.6%
#> 3 x & y 3   42.9%
#> 4 total 7    100%
#> ────────────────────────────────────────────────────────── End of JOYn report ──
#> ℹ Note:  Reporting variable is NOT returned
#>       id     t     x
#>    <num> <int> <num>
#> 1:     1     1    16
#> 2:     4     2    12
#> 3:     2     1    NA
#> 4:     3     2    NA
#> 5:    NA    NA    15
#> 6:     5    NA    NA
#> 7:     6    NA    NA

joyn_msg(msg_type = "info")
#> ℹ Note:  Reporting variable is NOT returned
```

Furthermore, info messages will help you keep track of which
**variables** **in `y` will be kept after the merging**, for example
notifying you if any of the y variables you have specified to keep will
be removed because they are part of the `by` variables.

``` r

joyn(x              = x2,
     y              = y2,
     by             = "id", 
     y_vars_to_keep = TRUE)
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
#> ℹ Note: Removing key variables id from id, yd, y, and x
#>       id     t     x    yd     y  .joyn
#>    <num> <int> <num> <num> <int> <fctr>
#> 1:     1     1    16     1    11  x & y
#> 2:     4     2    12    NA    NA      x
#> 3:     2     1    NA     2    15  x & y
#> 4:     3     2    NA     3    10  x & y
#> 5:    NA    NA    15    NA    NA      x
#> 6:     5    NA    NA     5    20      y
#> 7:     6    NA    NA     6    13      y

joyn_msg(msg_type = "info")
#> ℹ Note: Joyn's report available in variable .joyn
#> ℹ Note: Removing key variables id from id, yd, y, and x
```

### Timing messages 🔵

Timing messages report in how many seconds the join is executed,
including the time spent to perform all checks.

While performing the join, `joyn` keeps track of the **time spent for
the execution**. This is then displayed in timing messages, which report
the elapsed time measured in seconds.

Before visualizing some examples, it is important to remind a feature of
how `joyn` executes any join between two data tables.

Specifically, `joyn` always first executes a full join between the data
tables - which includes all matching and non matching rows in the
resulting table. Then, it filters the rows depending on the specific
type of join that user wants to execute. For example, if the user sets
`keep = "right"`, `joyn` will filter the table resulting from the full
join and return to the user the data table retaining *all* rows from the
right table and only *matching* rows from the left table. In addition,
note that since `joyn` performs a number of checks throughout the
execution (e.g., checking that the specified key for the merge is valid,
or the match type consistency), the time spent on checks will also be
included in reported time.

As a result, timing messages enable you to be aware of both:

1.  Time spent to execute the *full join*
2.  Time spent to execute the *entire joyn function, including checks*

``` r

# --------------------------- Example with full join ---------------------------

joyn(x          = x1, 
     y          = y1, 
     match_type = "m:1")
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
#>       id     t     x     y  .joyn
#>    <num> <int> <int> <num> <fctr>
#> 1:     1     1    11    11  x & y
#> 2:     1     2    12    11  x & y
#> 3:     2     1    13    15  x & y
#> 4:     3     2    14    NA      x
#> 5:    NA    NA    15    NA      x
#> 6:     4    NA    NA    16      y

joyn_msg("timing")
#> ● Timing:The full joyn is executed in 0.000311 seconds.
#> ● Timing: The entire joyn function, including checks, is executed in 0.021581
#> seconds.


# --------------------------- Example with left join ---------------------------
left_join(x            = x1, 
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
#>       id     t     x     y  .joyn
#>    <num> <int> <int> <num> <fctr>
#> 1:     1     1    11    11  x & y
#> 2:     1     2    12    11  x & y
#> 3:     2     1    13    15  x & y
#> 4:     3     2    14    NA      x
#> 5:    NA    NA    15    NA      x

joyn_msg("timing")
#> ● Timing:The full joyn is executed in 0.000589 seconds.
#> ● Timing: The entire joyn function, including checks, is executed in 0.023197
#> seconds.
```

### Warning messages ⚠️

`joyn` generates warning messages to alert you about possible
problematic situation which however do not warrant terminating execution
of the merge.

For example, if you provide a match type that is inconsistent with the
data, `joyn` will generate a warning to inform you about the actual
relationship and to alert that the join will be executed accordingly.

In the example below, both `x2` and `y2` are uniquely identified by the
key `id`, but the user is choosing a “one to many” relationship instead.
The user will be alerted and a “one to one” join will be executed
instead.

``` r

# Warning that "id" uniquely identifies y2 

joyn(x2, y2, by = "id", match_type = "1:m", verbose = TRUE)
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
#> ℹ Note: Removing key variables id from id, yd, y, and x
#> ⚠ Warning: The keys supplied uniquely identify y, therefore a 1:1 join is
#> executed
#>       id     t     x    yd     y  .joyn
#>    <num> <int> <num> <num> <int> <fctr>
#> 1:     1     1    16     1    11  x & y
#> 2:     4     2    12    NA    NA      x
#> 3:     2     1    NA     2    15  x & y
#> 4:     3     2    NA     3    10  x & y
#> 5:    NA    NA    15    NA    NA      x
#> 6:     5    NA    NA     5    20      y
#> 7:     6    NA    NA     6    13      y
joyn_msg("warn")
#> ⚠ Warning: The keys supplied uniquely identify y, therefore a 1:1 join is
#> executed
```

In a similar way, warning messages are generated when choosing
`match_type = "m:m" or "m:1"`

``` r

# ------------ Warning that "id" uniquely identifies both x2 and y2 ------------

joyn(x2, y2, by = "id", match_type = "m:m", verbose = TRUE)
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
#> ℹ Note: Removing key variables id from id, yd, y, and x
#> ⚠ Warning: The keys supplied uniquely identify both x and y, therefore a 1:1
#> join is executed
#>       id     t     x    yd     y  .joyn
#>    <num> <int> <num> <num> <int> <fctr>
#> 1:     1     1    16     1    11  x & y
#> 2:     4     2    12    NA    NA      x
#> 3:     2     1    NA     2    15  x & y
#> 4:     3     2    NA     3    10  x & y
#> 5:    NA    NA    15    NA    NA      x
#> 6:     5    NA    NA     5    20      y
#> 7:     6    NA    NA     6    13      y
joyn_msg("warn")
#> ⚠ Warning: The keys supplied uniquely identify both x and y, therefore a 1:1
#> join is executed

# ------------------ Warning that "id" uniquely identifies x2 ------------------

joyn(x2, y2, by = "id", match_type = "m:1", verbose = TRUE)
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
#> ℹ Note: Removing key variables id from id, yd, y, and x
#> ⚠ Warning: The keys supplied uniquely identify x, therefore a 1:1 join is
#> executed
#>       id     t     x    yd     y  .joyn
#>    <num> <int> <num> <num> <int> <fctr>
#> 1:     1     1    16     1    11  x & y
#> 2:     4     2    12    NA    NA      x
#> 3:     2     1    NA     2    15  x & y
#> 4:     3     2    NA     3    10  x & y
#> 5:    NA    NA    15    NA    NA      x
#> 6:     5    NA    NA     5    20      y
#> 7:     6    NA    NA     6    13      y
joyn_msg("warn")
#> ⚠ Warning: The keys supplied uniquely identify x, therefore a 1:1 join is
#> executed
```

Other examples of warnings are those that arise when you are trying to
supply certain arguments to the merging functions that are not yet
supported by the current version of `joyn`.

Suppose you are executing a left-join and you try to set the
`na_matches` argument to ‘never’. `joyn` will warn you that it currently
allows only `na_matches = 'na'`. A similar message is displayed when
`keep = NULL`. Given that the current version of `joyn` does not support
inequality joins, `joyn` will warn you that `keep = NULL` will make the
join retain only keys in `x`.

``` r

joyn::left_join(x            = x1, 
                y            = y1, 
                relationship = "many-to-one", 
                keep         = NULL,
                na_matches   = "never")
#> ⚠ Warning: joyn does not currently allow inequality joins, so keep = NULL will
#> retain only keys in x
#> ⚠ Warning: Currently, joyn allows only na_matches = 'na'
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
#> ⚠ Warning: Currently, joyn allows only na_matches = 'na'
#>       id     t     x     y  .joyn
#>    <num> <int> <int> <num> <fctr>
#> 1:     1     1    11    11  x & y
#> 2:     1     2    12    11  x & y
#> 3:     2     1    13    15  x & y
#> 4:     3     2    14    NA      x
#> 5:    NA    NA    15    NA      x

joyn_msg("warn")
#> ⚠ Warning: joyn does not currently allow inequality joins, so keep = NULL will
#> retain only keys in x
#> ⚠ Warning: Currently, joyn allows only na_matches = 'na'
```

### Error messages ❌

Error messages act as helpful notifications about the reasons why the
join you are trying to perform can’t be executed. Error messages
highlight where you went off course and provide clues to fix the issue
so that the merging can be successfully executed.

Sometimes error messages are due to a wrong/missing provision of the
inputs, for example if you do not supply variables to be used as key for
the merge, and `x` and `y` do not have any common variable names. Error
messages will also pop up if you provide an input data table that has no
variables, or that has duplicate variable names.

Representative messages in this regard can be visualized below:

``` r

# ----------------- Error due to input table x with no columns -----------------

x_empty = data.table()
  
joyn(x = x_empty,
     y = y1)
#> ✖ Error:  Input table x has no columns.
#> ✖ Error:  Input table x has no rows.
#> Error in `check_xy()`:
#> ! wrong input specification

joyn_msg("err")
#> ✖ Error:  Input table x has no columns.
#> ✖ Error:  Input table x has no rows.

# ----------------------- Error due to duplicate names  ------------------------

x_duplicates = data.table(id          = c(1L, 1L, 2L, 3L, NA_integer_),
                          x           = c(1L, 2L, 1L, 2L, NA_integer_),
                          x           = 11:15,
                          check.names = FALSE)
joyn(x = x_duplicates,
     y = y1)
#> ✖ Error:  Table x has the following column duplicated: x.  Please rename or
#> remove and try again.
#> Error in `check_xy()`:
#> ! wrong input specification

joyn_msg("err")
#> ✖ Error:  Table x has the following column duplicated: x.  Please rename or
#> remove and try again.
```

Furthermore, errors messages are generated when choosing the wrong
`match_type`, that is not consistent with the actual relationship
between the variables being used for merging. `joyn` will therefore
display the following message:

``` r

joyn(x = x1, y=y1, by="id", match_type = "1:1")
#> ✖ Error: table x is not uniquely identified by id
#> Duplicate counts in x:
#>        id copies percent
#>    <char>  <int>  <char>
#> 1:      1      2     40%
#> 2:  total      5    100%
#> Error in `check_match_type()`:
#> ! match type inconsistency
#> ℹ refer to the duplicate counts in the table(s) above to identify where the
#>   issue occurred
joyn_msg("err")
#> ✖ Error: table x is not uniquely identified by id
```

## Additional: How to visualize `joyn` messages?

`joyn` stores the messages in the `joyn` environment.

In order to print them, you can use the
[`joyn_msg()`](https://randrescastaneda.github.io/joyn/dev/reference/joyn_msg.md)
function. The `msg_type` argument allows you to specify a certain type
of message you would like to visualize, or, if you want all of them to
be displayed, you can just set `type = 'all'`

``` r

# Execute a join 

joyn(x = x1, 
     y = y1, 
     match_type = "m:1")
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
#>       id     t     x     y  .joyn
#>    <num> <int> <int> <num> <fctr>
#> 1:     1     1    11    11  x & y
#> 2:     1     2    12    11  x & y
#> 3:     2     1    13    15  x & y
#> 4:     3     2    14    NA      x
#> 5:    NA    NA    15    NA      x
#> 6:     4    NA    NA    16      y

# Print all messages stored
joyn_msg(msg_type = "all")
#> ℹ Note: Joyn's report available in variable .joyn
#> ℹ Note: Removing key variables id from id and y
#> ● Timing:The full joyn is executed in 0.000299 seconds.
#> ● Timing: The entire joyn function, including checks, is executed in 0.02154
#> seconds.

# Print info messages only 
joyn_msg(msg_type = "info")
#> ℹ Note: Joyn's report available in variable .joyn
#> ℹ Note: Removing key variables id from id and y
```
