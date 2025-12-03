# Auxiliary function to select vars of data table

Auxiliary function to select vars of data table

## Usage

``` r
filter_vars(
  dt,
  vars = names(dt),
  include = NULL,
  exclude = NULL,
  include_classes = NULL,
  exclude_classes = NULL,
  verbose = TRUE
)
```

## Arguments

- dt:

  data frame

- vars:

  character: A vector of variable names to consider for identifying
  unique combinations.

- include:

  character: Name of variable to be included, that might belong to the
  group excluded in the `exclude`

- exclude:

  character: Names of variables to exclude

- include_classes:

  character: classes to include in the analysis (e.g., "numeric",
  "integer", "date")

- exclude_classes:

  character: classes to exclude from analysis (e.g., "numeric",
  "integer", "date")

- verbose:

  logical: If FALSE no message will be displayed. Default is TRUE

## Value

character vector of selected vars
