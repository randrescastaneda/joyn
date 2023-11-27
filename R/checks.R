#' check reportvar input
#'
#' @inheritParams merge
#' @keywords internal
#' @return if character, it returns valid name. If NULL or FALSE, returns NULL.
check_reportvar <- function(reportvar, verbose = getOption("joyn.verbose")) {
  if (is.character(reportvar)) {

    reportvar <- rename_to_valid(reportvar, verbose)
    if (verbose) {
      cli::cli_alert_info("Joyn's report available in variable {.var {reportvar}}", wrap = TRUE)
    }
    return(reportvar)

  } else if (is.null(reportvar) || isFALSE(reportvar)) {
    if (verbose) {
      cli::cli_alert_info("No reporting variable will be available", wrap = TRUE)
    }
    return(NULL)
  } else  {
    cli::cli_abort("reportvar should be character, NULL or FALSE")
  }
}



#' check by input
#'
#' @inheritParams merge
#'
#' @return list with information about by variables
#' @keywords internal
check_by_vars <- function(by, x, y) {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # computations   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  fixby  <- fix_by_vars(by, x, y)

  if (length(fixby$by) == 0) {
    msg     <- "no common variable names in `x` and `y`"
    hint    <- "Make sure all variables are spelled correctly.
      Check for upper and lower cases"
    problem <- "When `by = NULL`, joyn search for common variable
      names to be used as keys"
    cli::cli_abort(c(
      msg,
      i = hint,
      x = problem
    ),
    class = "joyn_error"
    )
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Return   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  return(fixby)

}
