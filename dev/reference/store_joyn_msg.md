# Wrapper for store_msg function This function serves as a wrapper for the store_msg function, which is used to store various types of messages within the .joyn environment. :errors, warnings, timing information, or info

Wrapper for store_msg function This function serves as a wrapper for the
store_msg function, which is used to store various types of messages
within the .joyn environment. :errors, warnings, timing information, or
info

## Usage

``` r
store_joyn_msg(err = NULL, warn = NULL, timing = NULL, info = NULL)
```

## Arguments

- err:

  A character string representing an error message to be stored. Default
  value is NULL

- warn:

  A character string representing a warning message to be stored.
  Default value is NULL

- timing:

  A character string representing a timing message to be stored. Default
  value is NULL

- info:

  A character string representing an info message to be stored. Default
  value is NULL

## Value

invisible TRUE

## Hot to pass the message string

The function allows for the customization of the message string using
cli classes to emphasize specific components of the message Here's how
to format the message string: \*For variables: .strongVar \*For function
arguments: .strongArg \*For dt/df: .strongTable \*For text/anything
else: .strong \*NOTE: By default, the number of seconds specified in
timing messages is automatically emphasized using a custom formatting
approach. You do not need to apply cli classes nor to specify that the
number is in seconds.

## Examples

``` r
# Timing msg
joyn:::store_joyn_msg(timing = paste("  The entire joyn function, including checks,
                                       is executed in  ", round(1.8423467, 6)))

# Error msg
joyn:::store_joyn_msg(err = " Input table {.strongTable x} has no columns.")

# Info msg
joyn:::store_joyn_msg(info = "Joyn's report available in variable {.strongVar .joyn}")

```
