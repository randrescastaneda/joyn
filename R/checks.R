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




#' check variables in Y that will be kept in returning table
#'
#' @inheritParams merge
#'
#' @return charactere vector with variable names from Y table
#' @keywords internal
check_y_vars_to_keep <- function(y_vars_to_keep, y, by) {

  if (length(y_vars_to_keep) > 1 && !is.character(y_vars_to_keep)) {
    cli::cli_abort("argumet {.arg y_vars_to_keep} must be of length 1
                   when it is not class character")
  }

  if (isTRUE(y_vars_to_keep)) {
    y_vars_to_keep <- names(y)
  }

  if (isFALSE(y_vars_to_keep) || is.null(y_vars_to_keep)) {

    y_vars_to_keep <- NULL
    # temp_yvar <- paste0("temp_var", floor(stats::runif(1)*1000))
    # y_vars_to_keep     <-  temp_yvar
    #
    # y[, (temp_yvar) := 1]

  } else if (is.character(y_vars_to_keep)) {

    yvars    <- names(y)
    is_avail <- !(y_vars_to_keep %in% yvars)

    if (any(is_avail)) {
      no_avail <- y_vars_to_keep[is_avail]
      cli::cli_abort(
        c(
          "{.val {no_avail}} {?is/are} not {?a/} variable name{?s} available
                       in table {.field y}",
          "i" = "name{?s} available {?is/are} {.val {yvars}}"
        )
      )
    }

    # remove id variables
    y_in_by <- y_vars_to_keep %in% by

    if (any(y_in_by)) {
      store_msg("info",
                note = cli::symbol$circle_filled, "  ",
                pale = "removing key variables {.val {y_vars_to_keep[y_in_by]}}
                   from {.arg y_vars_to_keep}")
    }

    y_vars_to_keep <- y_vars_to_keep[!y_in_by]

  } else {
    valid <- c("character", "FALSE", "NULL")
    cli::cli_abort(c("{.val {y_vars_to_keep}} is not valid for argument
                   {.arg y_vars_to_keep}",
                   "i" = "Only {.or {.field {valid}}}"))
  }

  return(y_vars_to_keep)

}
