
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

`joyn` empowers you to assess the results of joining data frames, making
it easier and more efficient to combine your tables. Similar in
philosophy to the `merge` command in `Stata`, `joyn` offers matching key
variables and detailed join reports to ensure accurate and insightful
results.

## Motivation

Merging tables in R can be tricky. Ensuring accuracy and understanding
the joined data fully can be tedious tasks. That’s where `joyn` comes
in. Inspired by Stata’s informative approach to merging, `joyn` makes
the process smoother and more insightful.

While standard R merge functions are powerful, they often lack features
like assessing join accuracy, detecting potential issues, and providing
detailed reports. `joyn` fills this gap by offering:

- **Intuitive join handling:** Whether you’re dealing with one-to-one,
  one-to-many, or many-to-many relationships, `joyn` helps you navigate
  them confidently.
- **Informative reports:** Get clear insights into the join process with
  helpful reports that identify duplicate observations, missing values,
  and potential inconsistencies.

## What makes `joyn` special?

While standard R merge functions offer basic functionality, `joyn` goes
above and beyond by providing comprehensive tools and features tailored
to your data joining needs:

**1. Flexibility in join types:** Choose your ideal join type (“left”,
“right”, or “inner”) with the `keep` argument. Unlike R’s default,
`joyn` performs a full join by default, ensuring all observations are
included, but you have full control to tailor the results.

**2. Seamless variable handling:** No more wrestling with duplicate
variable names! `joyn` offers multiple options:

- **Update values:** Use `update_values` or `update_NA` to automatically
  update conflicting variables in the left table with values from the
  right table.

- **Keep both (with different names):** Enable `keep_common_vars = TRUE`
  to retain both variables, each with a unique suffix.

- **Selective inclusion:** Choose specific variables from the right
  table with `y_vars_to_keep`, ensuring you get only the data you need.

**3. Relationship awareness:** `joyn` recognizes one-to-one,
one-to-many, many-to-one, and many-to-many relationships between tables.
While it defaults to many-to-many for compatibility, **remember this is
often not ideal**. **Always specify the correct relationship using `by`
arguments** for accurate and meaningful results.

**4. Join success at a glance:** Get instant feedback on your join with
the automatically generated reporting variable. Identify potential
issues like unmatched observations or missing values to ensure data
integrity and informed decision-making.

By addressing these common pain points and offering enhanced
flexibility, `joyn` empowers you to confidently and effectively join
your data frames, paving the way for deeper insights and data-driven
success.

## Performance and flexibility

### The cost of Reliability

While raw speed is essential, understanding your joins every step of the
way is equally crucial. `joyn` prioritizes providing **insightful
information** and preventing errors over solely focusing on speed.
Unlike other functions, it adds:

- **Meticulous checks:** `joyn` performs comprehensive checks to ensure
  your join is accurate and avoids potential missteps, like unmatched
  observations or missing values.
- **Detailed reporting:** Get a clear picture of your join with a
  dedicated report, highlighting any issues you should be aware of.
- **User-friendly summary:** Quickly grasp the join’s outcome with a
  concise overview presented in a clear table.

These valuable features contribute to a slightly slower performance
compared to functions like `data.table::merge.data.table()` or
`collapse::join()`. However, the benefits of **preventing errors and
gaining invaluable insights** far outweigh the minor speed difference.

### Know your needs, choose your tool

- **Speed is your top priority for massive datasets?** Consider using
  `data.table` or `collapse` directly.
- **Seek clear understanding and error prevention for your joins?**
  `joyn` is your trusted guide.

### Protective by design

`joyn` intentionally restricts certain actions and provides clear
messages when encountering unexpected data configurations. This might
seem **opinionated**, but it’s designed to **protect you from
accidentally creating inaccurate or misleading joins**. This “safety
net” empowers you to confidently merge your data, knowing `joyn` has
your back.

### Flexibility

Currently, `joyn` focuses on the most common and valuable join types.
Future development might explore expanding its flexibility based on user
needs and feedback.

## `joyn` as wrapper: Familiar Syntax, Familiar Power

While `joyn::join()` offers the core functionality and Stata-inspired
arguments, you might prefer a syntax more aligned with your existing
workflow. `joyn` has you covered!

**Embrace base R and `data.table`:**

- `joyn::merge()`: Leverage familiar base R and `data.table` syntax for
  seamless integration with your existing code.

**Join with flair using `dplyr`:**

- `joyn::{dplyr verbs}()`: Enjoy the intuitive
  [verb-based](https://dplyr.tidyverse.org/reference/mutate-joins.html)
  syntax of `dplyr` for a powerful and expressive way to perform joins.

**Dive deeper:** Explore the corresponding vignettes to unlock the full
potential of these alternative interfaces and find the perfect fit for
your data manipulation style.

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
#>     .joyn     n percent
#>    <char> <int>  <char>
#> 1:      x     2   33.3%
#> 2:  x & y     3     50%
#> 3:      y     1   16.7%
#> 4:  total     6    100%
#> ────────────────────────────────────────────────────────── End of JOYn report ──
#> ℹ ❯ Joyn's report available in variable .joyn
#> ℹ ❯ Removing key variables id from id and y
#> ● Timing: The full joyn is executed in 0.000718 seconds
#> ● Timing: The entire joyn function, including checks, is executed in 0.048331
#> seconds
#> Key: <id>
#>       id     t     x     y  .joyn
#>    <num> <int> <int> <num> <char>
#> 1:    NA    NA    15    NA      x
#> 2:     1     1    11    11  x & y
#> 3:     1     2    12    11  x & y
#> 4:     2     1    13    15  x & y
#> 5:     3     2    14    NA      x
#> 6:     4    NA    NA    16      y

# keep just those observations that match
joyn(x = x1, 
     y = y1, 
     match_type = "m:1",
     keep = "inner")
#> 
#> ── JOYn Report ──
#> 
#>     .joyn     n percent
#>    <char> <int>  <char>
#> 1:  x & y     3    100%
#> 2:  total     3    100%
#> ────────────────────────────────────────────────────────── End of JOYn report ──
#> ℹ ❯ Joyn's report available in variable .joyn
#> ℹ ❯ Removing key variables id from id and y
#> ● Timing: The full joyn is executed in 7.4e-05 seconds
#> ● Timing: The entire joyn function, including checks, is executed in 0.041525
#> seconds
#> Key: <id>
#>       id     t     x     y  .joyn
#>    <num> <int> <int> <num> <char>
#> 1:     1     1    11    11  x & y
#> 2:     1     2    12    11  x & y
#> 3:     2     1    13    15  x & y

# Bad merge for not specifying by argument
joyn(x = x2, 
     y = y2,
     match_type = "1:1")
#> 
#> ── JOYn Report ──
#> 
#>     .joyn     n percent
#>    <char> <int>  <char>
#> 1:      x     4   44.4%
#> 2:  x & y     1   11.1%
#> 3:      y     4   44.4%
#> 4:  total     9    100%
#> ────────────────────────────────────────────────────────── End of JOYn report ──
#> ℹ ❯ Joyn's report available in variable .joyn
#> ℹ ❯ Removing key variables id and x from id, yd, y, and x
#> ● Timing: The full joyn is executed in 8.1e-05 seconds
#> ● Timing: The entire joyn function, including checks, is executed in 0.033808
#> seconds
#> Key: <id, x>
#>       id     t     x    yd     y  .joyn
#>    <num> <int> <num> <num> <int> <char>
#> 1:    NA    NA    15    NA    NA      x
#> 2:     1     1    16     1    11  x & y
#> 3:     2     1    NA    NA    NA      x
#> 4:     2    NA    17     2    15      y
#> 5:     3     2    NA    NA    NA      x
#> 6:     3    NA    20     3    10      y
#> 7:     4     2    12    NA    NA      x
#> 8:     5    NA    18     5    20      y
#> 9:     6    NA    19     6    13      y

# good merge, ignoring variable x from y
joyn(x = x2, 
     y = y2,
     by = "id",
     match_type = "1:1")
#> 
#> ── JOYn Report ──
#> 
#>     .joyn     n percent
#>    <char> <int>  <char>
#> 1:      x     2   28.6%
#> 2:  x & y     3   42.9%
#> 3:      y     2   28.6%
#> 4:  total     7    100%
#> ────────────────────────────────────────────────────────── End of JOYn report ──
#> ℹ ❯ Joyn's report available in variable .joyn
#> ℹ ❯ Removing key variables id from id, yd, y, and x
#> ● Timing: The full joyn is executed in 8.5e-05 seconds
#> ● Timing: The entire joyn function, including checks, is executed in 0.035442
#> seconds
#> Key: <id>
#>       id     t     x    yd     y  .joyn
#>    <num> <int> <num> <num> <int> <char>
#> 1:    NA    NA    15    NA    NA      x
#> 2:     1     1    16     1    11  x & y
#> 3:     2     1    NA     2    15  x & y
#> 4:     3     2    NA     3    10  x & y
#> 5:     4     2    12    NA    NA      x
#> 6:     5    NA    NA     5    20      y
#> 7:     6    NA    NA     6    13      y

# update NAs in var x in table x from var x in y
joyn(x = x2, 
     y = y2, 
     by = "id", 
     update_NAs = TRUE)
#> 
#> ── JOYn Report ──
#> 
#>         .joyn     n percent
#>        <char> <int>  <char>
#> 1: NA updated     4   57.1%
#> 2:          x     2   28.6%
#> 3:      x & y     1   14.3%
#> 4:      total     7    100%
#> ────────────────────────────────────────────────────────── End of JOYn report ──
#> ℹ ❯ Joyn's report available in variable .joyn
#> ℹ ❯ Removing key variables id from id, yd, y, and x
#> ● Timing: The full joyn is executed in 8.2e-05 seconds
#> ● Timing: The entire joyn function, including checks, is executed in 0.038231
#> seconds
#> Key: <id>
#>       id     t     x    yd     y      .joyn
#>    <num> <int> <num> <num> <int>     <char>
#> 1:    NA    NA    15    NA    NA          x
#> 2:     1     1    16     1    11      x & y
#> 3:     2     1    17     2    15 NA updated
#> 4:     3     2    20     3    10 NA updated
#> 5:     4     2    12    NA    NA          x
#> 6:     5    NA    18     5    20 NA updated
#> 7:     6    NA    19     6    13 NA updated

# update values in var x in table x from var x in y
joyn(x = x2, 
     y = y2, 
     by = "id", 
     update_values = TRUE)
#> 
#> ── JOYn Report ──
#> 
#>            .joyn     n percent
#>           <char> <int>  <char>
#> 1:    NA updated     4   57.1%
#> 2:   not updated     2   28.6%
#> 3: value updated     1   14.3%
#> 4:         total     7    100%
#> ────────────────────────────────────────────────────────── End of JOYn report ──
#> ℹ ❯ Joyn's report available in variable .joyn
#> ℹ ❯ Removing key variables id from id, yd, y, and x
#> ● Timing: The full joyn is executed in 8.1e-05 seconds
#> ● Timing: The entire joyn function, including checks, is executed in 0.033889
#> seconds
#> Key: <id>
#>       id     t     x    yd     y         .joyn
#>    <num> <int> <num> <num> <int>        <char>
#> 1:    NA    NA    15    NA    NA   not updated
#> 2:     1     1    16     1    11 value updated
#> 3:     2     1    17     2    15    NA updated
#> 4:     3     2    20     3    10    NA updated
#> 5:     4     2    12    NA    NA   not updated
#> 6:     5    NA    18     5    20    NA updated
#> 7:     6    NA    19     6    13    NA updated


# do not bring any variable from y into x, just the report
joyn(x = x2, 
     y = y2, 
     by = "id", 
     y_vars_to_keep = NULL)
#> 
#> ── JOYn Report ──
#> 
#>     .joyn     n percent
#>    <char> <int>  <char>
#> 1:      x     2   28.6%
#> 2:  x & y     3   42.9%
#> 3:      y     2   28.6%
#> 4:  total     7    100%
#> ────────────────────────────────────────────────────────── End of JOYn report ──
#> ℹ ❯ Joyn's report available in variable .joyn
#> ● Timing: The full joyn is executed in 7.7e-05 seconds
#> ● Timing: The entire joyn function, including checks, is executed in 0.025583
#> seconds
#> Key: <id>
#>       id     t     x  .joyn
#>    <num> <int> <num> <char>
#> 1:    NA    NA    15      x
#> 2:     1     1    16  x & y
#> 3:     2     1    NA  x & y
#> 4:     3     2    NA  x & y
#> 5:     4     2    12      x
#> 6:     5    NA    NA      y
#> 7:     6    NA    NA      y
```
