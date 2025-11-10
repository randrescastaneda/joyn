# Update NA and/or values

The function updates NAs and/or values in the following way:

- If only update_NAs is TRUE: update NAs of var in x with values of var
  y of the same name

- If only update_values = TRUE: update all values, but NOT NAs, of var
  in x with values of var y of the same name. NAs from y are not used to
  update values in x . (e.g., if x.var = 10 and y.var = NA, x.var
  remains 10)

- If both update_NAs and update_values are TRUE, both NAs and values in
  x are updated as described above

- If both update_NAs and update_values are FALSE, no update

## Usage

``` r
update_na_values(
  dt,
  var,
  reportvar = getOption("joyn.reportvar"),
  suffixes = getOption("joyn.suffixes"),
  rep_NAs = FALSE,
  rep_values = FALSE
)
```

## Arguments

- dt:

  joined data.table

- var:

  variable(s) to be updated

- reportvar:

  character: Name of reporting variable. Default is ".joyn". This is the
  same as variable "\_merge" in Stata after performing a merge. If FALSE
  or NULL, the reporting variable will be excluded from the final table,
  though a summary of the join will be display after concluding.

- suffixes:

  A character(2) specifying the suffixes to be used for making non-by
  column names unique. The suffix behaviour works in a similar fashion
  as the [base::merge](https://rdrr.io/r/base/merge.html) method does.

- rep_NAs:

  inherited from joyn update_NAs

- rep_values:

  inherited from joyn update_values

## Value

data.table
