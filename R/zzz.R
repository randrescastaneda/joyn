.onLoad <- function(libname, pkgname) {
  op <- options()
  op.joyn <- list(
    joyn.verbose         = TRUE,
    possible_ids.verbose = FALSE,
    joyn.reportvar       = ".joyn",
    joyn.suffixes        = c(".x", ".y"),
    joyn.match_type      = c("1:1", "1:m", "m:1", "m:m"),
    joyn.na.last         = FALSE,
    joyn.msg_type        = "basic",
    joyn.output_method   = cli::ansi_has_hyperlink_support()

  )
  toset <- !(names(op.joyn) %in% names(op))

  #store them in .joynenv
  rlang::env_bind(.joynenv, op.joyn = op.joyn)

  if(any(toset)) {
    options(op.joyn[toset])
  }

  #get_joyn_options()

  invisible()
}


#' Get joyn options
#'
#' This function aims to display and store info on joyn options
#'
#' @param env environment, which is joyn environment by default
#' @param display logical, if TRUE displays (i.e., print) info on joyn options and
#'    corresponding default and current values
#' @param option character or NULL. If character, name of a specific joyn option.
#'    If NULL, all joyn options
#' @return joyn options and values invisibly as a list
#' @family options
#'
#' @export
#' @examples
#' \dontrun{
#'
#' # display all joyn options, their default and current values
#' joyn:::get_joyn_options()
#'
#' # store list of option = value pairs AND do not display info
#' joyn_options <- joyn:::get_joyn_options(display = FALSE)
#'
#' # get info on one specific option and store it
#' joyn.verbose <- joyn:::get_joyn_options(option = "joyn.verbose")
#'
#' # get info on two specific option
#' joyn:::get_joyn_options(option = c("joyn.verbose", "joyn.reportvar"))
#'
#' }
get_joyn_options <- function(env     = .joynenv,
                             display = TRUE,
                             option  = NULL) {

  # joyn options
  op.joyn <- rlang::env_get(env, "op.joyn")

  if (is.null(option)) {
    # get all possible options
    op.joyn <- rlang::env_get(env, "op.joyn")

  } else {
    # get specified option
    op.joyn <- rlang::env_get(env, "op.joyn")[option]
  }

  if (display == TRUE) {

    names_ops     <- names(op.joyn)
    default_value <- sapply(op.joyn, toString, USE.NAMES = FALSE, simplify = TRUE)
    current_value <- sapply(names(op.joyn),
                            \(x) {
                              x |>
                                getOption() |>
                                toString()
                            },
                            USE.NAMES = FALSE)

    ops_info <-
      paste0(
        cli::col_green(cli::symbol$bullet), " ", cli::col_blue(format(names_ops)),
        " ", format("default:"), " ", cli::col_cyan(format(default_value)),
        " ", format("current:"), " ", cli::col_cyan(format(current_value))
      ) |>
      cli::ansi_columns(width = 80)


    # print and display options
    cli::boxx(ops_info,
              border_style = "single",
              padding = 1,
              header = cli::col_cyan("Joyn options: "),
              border_col = "white") |>
      print()
  }

  invisible(op.joyn)
}

#' Set joyn options
#'
#' This function is used to change the value of one or more joyn options
#'
#' @param env environment, which is joyn environment by default
#' @param ... pairs of option = value
#' @return joyn new options and values invisibly as a list
#' @family options
#'
#' @export
#' @examples
#' joyn:::set_joyn_options(joyn.verbose = FALSE, joyn.reportvar = "joyn_status")
#' joyn:::set_joyn_options() # return to default options
set_joyn_options <- function(...,
                             env = .joynenv
                             ) {

  op.joyn <- rlang::env_get(env, "op.joyn")

  new_options <- list(...)
  if (any(!(names(new_options) %in% names(op.joyn)))) {
    cli::cli_abort("invalid option")
  }

  # TO DO: Add checks on input names and values

  # Return new options invisibly as a list
  if (length(new_options) == 0) {
    new_options <- op.joyn
  }
  # Set new options
  options(new_options)

  invisible(new_options)

}

# ------------------------------------
# -- Define global variables --
# ------------------------------------

utils::globalVariables(c("..byvar",
                         "..vars"))

# ------------------------------------------------------------------------------------------
# Define custom .strong {cli} classes to emphasize messages subcomponents
# --- to be used when creating/storing {joyn} messages
# -------------------------------------------------------------------------------------------

# Class 'strong' - for text/general subcomponents of the msg that we want to highlight
cli::cli_div(theme = list(
  span.strong = list(color = "#555555"),
  "span.strong" = list("font-weight" = "bold")),
  .auto_close = FALSE)

# Class `strongVar` - to highlight variables/column names
cli::cli_div(theme = list(
  span.strongVar = list(color = "#0a9396"),
  "span.strongVar" = list(before = "`"),
  "span.strongVar" = list("font-weight" = "bold"),
  "span.strongVar" = list(after = "`")),
  .auto_close = FALSE)

# Class `strongTable` - to highlight data tables/frames (e.g., x or y)
cli::cli_div(theme = list(
  span.strongTable = list(color = "#BF00FF"),
  "span.strongTable" = list(before = "`"),
  "span.strongTable" = list("font-weight" = "bold"),
  "span.strongTable" = list(after = "`")),
  .auto_close = FALSE)

# Class `strongArg` - to highlight function arguments (e.g., suffixes, match type)
cli::cli_div(theme = list(
  span.strongArg = list(color = "#0077b6"),
  "span.strongArg" = list("font-weight" = "bold")),
  .auto_close = FALSE)



