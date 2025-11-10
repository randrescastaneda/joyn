# Check if vars in dt have duplicate names

Check if vars in dt have duplicate names

## Usage

``` r
check_duplicate_names(dt, name)
```

## Arguments

- dt:

  data.frame to check

- name:

  var name to check if has duplicates in dt

## Value

logical either TRUE, if any duplicates are found, or FALSE otherwise

## Examples

``` r
if (FALSE) { # \dontrun{
# When no duplicates
x1 = data.table(id = c(1L, 1L, 2L, 3L, NA_integer_),
                t  = c(1L, 2L, 1L, 2L, NA_integer_),
                x  = 11:15)
joyn:::check_duplicate_names(x1, "x")

# When duplicates
x1_duplicates = data.frame(id = c(1L, 1L, 2L, 3L, NA_integer_),
                           x  = c(1L, 2L, 1L, 2L, NA_integer_),
                           x  = 11:15,
                           check.names = FALSE)
joyn:::check_duplicate_names(x1_duplicates, "x")
} # }
```
