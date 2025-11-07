# Split matching type

Split matching type (one of `"1:1", "m:1", "1:m", "m:m"`) into its two
components

## Usage

``` r
split_match_type(match_type)
```

## Arguments

- match_type:

  character: one of *"m:m"*, *"m:1"*, *"1:m"*, *"1:1"*. Default is
  *"1:1"* since this the most restrictive. However, following Stata's
  recommendation, it is better to be explicit and use any of the other
  three match types (See details in *match types sections*).

## Value

character vector
