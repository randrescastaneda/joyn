# Add x key var and y key var (with suffixes) to x and y -when joining by different variables and keep is true

Add x key var and y key var (with suffixes) to x and y -when joining by
different variables and keep is true

## Usage

``` r
set_col_names(x, y, by, suffix, jn_type)
```

## Arguments

- x:

  data table: left table

- y:

  data table: right table

- by:

  character vector of variables to join by

- suffix:

  character(2) specifying the suffixes to be used for making non-by
  column names unique

- jn_type:

  character specifying type of join

## Value

list containing x and y
