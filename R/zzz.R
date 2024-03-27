.onLoad <- function(libname, pkgname) {
  op <- options()
  op.joyn <- list(
    joyn.verbose         = TRUE,
    possible_ids.verbose = TRUE,
    joyn.reportvar       = ".joyn",
    joyn.suffixes        = c(".x", ".y"),
    joyn.match_type      = c("1:1", "1:m", "m:1", "m:m"),
    joyn.na.last         = FALSE
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
#'
#' @keywords internal
#' @examples
#' \donotrun{
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
    options_info <- sapply(names(op.joyn), function(opt) {

      default_value <- op.joyn[[opt]]
      current_value <- getOption(opt)
      sprintf("%-20s default = %-20s > current = %s",
              opt,
              toString(default_value),
              toString(current_value))
    },
    USE.NAMES = FALSE)


    # print and display options
    cat("\nJoyn Options:\n")
    cat("---------------------------------------------------------------------\n")
    cat(paste(options_info, collapse = "\n"), "\n")
    cat("---------------------------------------------------------------------\n")

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
#'
#' @keywords internal
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

