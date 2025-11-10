# Rename vars in y so they are different to x's when joined

Check vars in y with same names as vars in x, and return new variables
names for those y vars for the joined data frame

## Usage

``` r
check_new_y_vars(x, by, y_vars_to_keep)
```

## Arguments

- x:

  master table

- by:

  character: by vars

- y_vars_to_keep:

  character vector of y variables to keep

## Value

vector with new variable names for y

## Examples

``` r
if (FALSE) { # \dontrun{
y2 = data.frame(id = c(1, 2, 5, 6, 3),
                yd = c(1, 2, 5, 6, 3),
                y  = c(11L, 15L, 20L, 13L, 10L),
                x  = c(16:20))
joyn:::y_vars_to_keep <- check_y_vars_to_keep(TRUE, y2, by = "id")
x2 = data.frame(id = c(1, 1, 2, 3, NA),
                t  = c(1L, 2L, 1L, 2L, NA_integer_),
                x  = c(16, 12, NA, NA, 15))
joyn:::check_new_y_vars(x = x2, by="id", y_vars_to_keep)
} # }
```
