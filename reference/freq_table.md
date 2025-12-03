# Tabulate simple frequencies

tabulate one variable frequencies

## Usage

``` r
freq_table(x, byvar, digits = 1, na.rm = FALSE, freq_var_name = "n")
```

## Arguments

- x:

  data frame

- byvar:

  character: name of variable to tabulate. Use Standard evaluation.

- digits:

  numeric: number of decimal places to display. Default is 1.

- na.rm:

  logical: report NA values in frequencies. Default is FALSE.

- freq_var_name:

  character: name for frequency variable. Default is "n"

## Value

data.table with frequencies.

## Examples

``` r
library(data.table)
x4 = data.table(id1 = c(1, 1, 2, 3, 3),
                id2 = c(1, 1, 2, 3, 4),
                t   = c(1L, 2L, 1L, 2L, NA_integer_),
                x   = c(16, 12, NA, NA, 15))
freq_table(x4, "id1")
```
