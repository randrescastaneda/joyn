# Check whether specified "many" relationship is valid

When "many" relationship is specified, check if it is valid.  
(Specified many relationship not valid if the dt is instead uniquely
identified by specified keys)

## Usage

``` r
is_valid_m_key(dt, by)
```

## Arguments

- dt:

  data object

- by:

  character vector: specified keys, already fixed

## Value

logical: `TRUE` if valid, `FALSE` if uniquely identified

## Examples

``` r
if (FALSE) { # \dontrun{
# example with data frame uniquely identified by specified `by` vars
x1 = data.frame(id  = c(1L, 1L, 2L, 3L, NA_integer_),
                 t  = c(1L, 2L, 1L, 2L, NA_integer_),
                 x  = 11:15)

joyn:::is_valid_m_key(x1, by = c("id", "t"))
# example with valid specified "many" relationship
x2 = data.frame(id  = c(1L, 1L, 1L, 3L, NA_integer_),
                 t  = c(1L, 2L, 1L, 2L, NA_integer_),
                 x  = 11:15)
joyn:::is_valid_m_key(x2, by = c("id", "t"))
} # }
```
