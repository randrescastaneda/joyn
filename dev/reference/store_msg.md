# Store joyn message to .joynenv environment

Store joyn message to .joynenv environment

## Usage

``` r
store_msg(type, ...)
```

## Arguments

- ...:

  combination of type and text in the form
  `style1 = text1, style2 = text2`, etc.

## Value

current message data frame invisibly

## See also

Messages functions
[`clear_joynenv()`](https://randrescastaneda.github.io/joyn/dev/reference/clear_joynenv.md),
[`joyn_msg()`](https://randrescastaneda.github.io/joyn/dev/reference/joyn_msg.md),
[`joyn_msgs_exist()`](https://randrescastaneda.github.io/joyn/dev/reference/joyn_msgs_exist.md),
[`joyn_report()`](https://randrescastaneda.github.io/joyn/dev/reference/joyn_report.md),
[`msg_type_dt()`](https://randrescastaneda.github.io/joyn/dev/reference/msg_type_dt.md),
[`style()`](https://randrescastaneda.github.io/joyn/dev/reference/style.md),
[`type_choices()`](https://randrescastaneda.github.io/joyn/dev/reference/type_choices.md)

## Examples

``` r
# Storing msg with msg_type "info"
joyn:::store_msg("info",
  ok = cli::symbol$tick, "  ",
  pale = "This is an info message")

# Storing msg with msg_type "warn"
joyn:::store_msg("warn",
  err = cli::symbol$cross, "  ",
  note = "This is a warning message")
```
