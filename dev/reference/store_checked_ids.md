# store checked variables as possible ids

This function processes a list of possible IDs by removing any `NULL`
entries, storing a set of checked variables as an attribute and in the
specified environment, and then returning the updated list of possible
IDs.

## Usage

``` r
store_checked_ids(checked_ids, possible_ids, env = .joynenv)
```

## Arguments

- checked_ids:

  A vector of variable names that have been checked as possible IDs.

- possible_ids:

  A list containing potential identifiers. This list may contain `NULL`
  values, which will be removed by the function.

- env:

  An environment where the `checked_ids` will be stored. The default is
  `.joynenv`.

## Value

A list of possible IDs with `NULL` values removed, and the `checked_ids`
stored as an attribute.
