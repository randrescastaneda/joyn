# Check join variable class

Checks if a variable in a data.table is of a supported class for
joining. Stores a warning via
[`store_joyn_msg()`](https://randrescastaneda.github.io/joyn/dev/reference/store_joyn_msg.md)
if unsupported.

## Usage

``` r
check_var_class(dt, var)
```

## Arguments

- dt:

  data.table containing the variable

- var:

  Character vector of variable names to check

## Value

Variable name invisibly if unsupported, otherwise NULL
