# Check reporting variable

check reportvar input  
If resulting data frame has a reporting variable (storing joyn's
report), check and return a valid name.

## Usage

``` r
check_reportvar(reportvar, verbose = getOption("joyn.verbose"))
```

## Value

if input reportvar is character, return valid name for the report var.
If NULL or FALSE, return NULL.

## Examples

``` r
if (FALSE) { # \dontrun{
# When null - reporting variable not returned in merged dt
joyn:::check_reportvar(reportvar = NULL)
# When FALSE - reporting variable not returned in merged dt
joyn:::check_reportvar(reportvar = FALSE)
# When character
joyn:::check_reportvar(reportvar = ".joyn")
} # }
```
