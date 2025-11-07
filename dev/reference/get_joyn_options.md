# Get joyn options

This function aims to display and store info on joyn options

## Usage

``` r
get_joyn_options(env = .joynenv, display = TRUE, option = NULL)
```

## Arguments

- env:

  environment, which is joyn environment by default

- display:

  logical, if TRUE displays (i.e., print) info on joyn options and
  corresponding default and current values

- option:

  character or NULL. If character, name of a specific joyn option. If
  NULL, all joyn options

## Value

joyn options and values invisibly as a list

## See also

JOYn options functions
[`set_joyn_options()`](https://randrescastaneda.github.io/joyn/dev/reference/set_joyn_options.md)

## Examples

``` r
if (FALSE) { # \dontrun{

# display all joyn options, their default and current values
joyn:::get_joyn_options()

# store list of option = value pairs AND do not display info
joyn_options <- joyn:::get_joyn_options(display = FALSE)

# get info on one specific option and store it
joyn.verbose <- joyn:::get_joyn_options(option = "joyn.verbose")

# get info on two specific option
joyn:::get_joyn_options(option = c("joyn.verbose", "joyn.reportvar"))

} # }
```
