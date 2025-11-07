# Rename to syntactically valid names

Rename to syntactically valid names

## Usage

``` r
rename_to_valid(name, verbose = getOption("joyn.verbose"))
```

## Arguments

- name:

  character: name to be coerced to syntactically valid name

- verbose:

  logical: if FALSE, it won't display any message (programmer's option).
  Default is TRUE.

## Value

valid character name

## Examples

``` r
joyn:::rename_to_valid("x y")
#> ℹ name x y is an invalid variable name. It will be changed to x.y
#> [1] "x.y"
```
