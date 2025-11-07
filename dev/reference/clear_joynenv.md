# Clearing joyn environment

Clearing joyn environment

## Usage

``` r
clear_joynenv()
```

## See also

Messages functions
[`joyn_msg()`](https://randrescastaneda.github.io/joyn/dev/reference/joyn_msg.md),
[`joyn_msgs_exist()`](https://randrescastaneda.github.io/joyn/dev/reference/joyn_msgs_exist.md),
[`joyn_report()`](https://randrescastaneda.github.io/joyn/dev/reference/joyn_report.md),
[`msg_type_dt()`](https://randrescastaneda.github.io/joyn/dev/reference/msg_type_dt.md),
[`store_msg()`](https://randrescastaneda.github.io/joyn/dev/reference/store_msg.md),
[`style()`](https://randrescastaneda.github.io/joyn/dev/reference/style.md),
[`type_choices()`](https://randrescastaneda.github.io/joyn/dev/reference/type_choices.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Storing a message
joyn:::store_msg("info", "simple message")

# Clearing the environment
joyn:::clear_joynenv()

# Checking it does not exist in the environment
print(joyn:::joyn_msgs_exist())
} # }
```
