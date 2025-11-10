# Set joyn options

This function is used to change the value of one or more joyn options

## Usage

``` r
set_joyn_options(..., env = .joynenv)
```

## Arguments

- ...:

  pairs of option = value

- env:

  environment, which is joyn environment by default

## Value

joyn new options and values invisibly as a list

## See also

JOYn options functions
[`get_joyn_options()`](https://randrescastaneda.github.io/joyn/reference/get_joyn_options.md)

## Examples

``` r
joyn:::set_joyn_options(joyn.verbose = FALSE, joyn.reportvar = "joyn_status")
joyn:::set_joyn_options() # return to default options
```
