#' check reportvar input
#'
#' @inheritParams merge
#' @keywords internal
#' @return if character, it returns valid name. If NULL or FALSE, returns NULL.
check_reportvar <-
  function(reportvar, verbose = getOption("joyn.verbose")) {
    if (is.character(reportvar)) {
      reportvar <- rename_to_valid(reportvar, verbose)
      store_msg("info",
                timing = cli::symbol$star,
                "   ",
                pale = "Joyn's report available in variable {.var {reportvar}}")

      return(reportvar)

    } else if (is.null(reportvar) || isFALSE(reportvar)) {

      store_msg("info",
                timing = cli::symbol$star,
                "   ",
                "Reporting variable is not returned")

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


#' check match type consistency
#'
#' @inheritParams merge
#'
#' @return character vector from [split_match_type]
#' @keywords internal
check_match_type <- function(x, y, by, match_type, verbose) {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # computations   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  mts <- split_match_type(match_type)
  tx  <- mts[1]
  ty  <- mts[2]

  match_type_error <- FALSE

  if (tx == "1") {
    match_type_error <- is_match_type_error(x, "x", by, verbose, match_type_error)
  }

  if (ty == "1") {
    match_type_error <- is_match_type_error(y, "y", by, verbose, match_type_error)
  }

  if (match_type_error) {

    msg     <- "match type inconsistency"
    hint    <- "you could use `return_report = TRUE` in `joyn::is_id()`
    to see where the problem is"
    cli::cli_abort(c(
      msg,
      i = hint
    ),
    class = "joyn_error"
    )

  }


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Return   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  return(mts)

}

#' confirm if match_type_error
#'
#' @inheritParams merge
#' @param name name of variable
#' @param match_type_error  logical: from existing code
#'
#' @return logical
#' @keywords internal
is_match_type_error <- function(x, name, by, verbose, match_type_error) {

  isidx <- is_id(x, by = by, verbose = FALSE)

  if (isFALSE(isidx)) {

    match_type_error <- TRUE
    store_msg("warn",
              err = cli::symbol$cross,
              "   ",
              warn = "table {.field name} is not uniquely identified by {.code {by}}")
  }
  match_type_error
}
