#' check tables X and Y
#'
#' This function performs checks inspired on merge.data.table:
#'  * Checks if x and/or y have no columns
#'  * Checks if x and/or y contain duplicate column names
#'
#' @inheritParams joyn
#'
#' @return invisible TRUE
#' @keywords internal
#' 
#' @example 
#' # Check passing with no erros
#' library(data.table)
#' x1 = data.table(id = c(1L, 1L, 2L, 3L, NA_integer_),
#'                 t  = c(1L, 2L, 1L, 2L, NA_integer_),
#'                 x  = 11:15)
#' y1 = data.table(id = c(1,2, 4),
#'                 y  = c(11L, 15L, 16))
#' check_xy(x = x1, y=y1)

check_xy  <- function(x,y) {

  error_exists <- FALSE

  # check no columns --------------
  x0 = length(x) == 0L
  y0 = length(y) == 0L
  if (x0 || y0) {
    error_exists <- TRUE
    if (x0 && y0) {
      xy <- c("x", "y")
      store_msg("err",
                err = paste(cli::symbol$cross, "Error:"),
                "   Neither {.or {.field {xy}}} table has columns.")
    } else if (x0) {
      store_msg("err",
                err = paste(cli::symbol$cross, "Error:"),
                "   Input table {.field x} has no columns.")
    } else {
      store_msg("err",
                err = paste(cli::symbol$cross, "Error:"),
                "   Input table {.field y} has no columns.")
    }

  }

  # check names -----------
  error_exists <- check_duplicate_names(x, "x")
  error_exists <- check_duplicate_names(y, "y")

  if (error_exists) {
    joyn_msg("err")
    cli::cli_abort("wrong input specification")
  }
  return(invisible(TRUE))
}

check_duplicate_names <- \(dt, name) {
  nm_x = names(dt)
  if (anyDuplicated(nm_x)) {
    dups <- nm_x[duplicated(nm_x)] |>
      unique()
    store_msg("err",
              err = paste(cli::symbol$cross, "Error:"),
              "  table {.field {name}} has the folowing
              {cli::qty(length(dups))} column{?s} duplicated:
              {.var {dups}}.
              Please remove or rename {?it/them} and try again.")
    return(TRUE)
  }
  return(FALSE)
}

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



#' check `by` input
#' 
#' This function checks the variable name(s) to be used as key(s) of the join
#'
#' @inheritParams merge
#'
#' @return list with information about by variables
#' @keywords internal
#' 
#' @example
#' # Good by input
#' x1 = data.frame(
#'        id = c(1L, 1L, 2L, 3L, NA_integer_),
#'        t  = c(1L, 2L, 1L, 2L, NA_integer_),
#'        x  = 11:15)
#' y1 = data.frame(id = 1:2,
#'                 y  = c(11L, 15L))
#' check_by_vars(by = "id", x = x1, y = y1)

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
#' This function checks if the match type chosen by the user is consistent with the data 
#'
#' @inheritParams merge
#'
#' @return character vector from [split_match_type]
#' @keywords internal
#' 
#' @examples 
#' # Consistent match type
#' x1 = data.frame(
#'        id = c(1L, 1L, 2L, 3L, NA_integer_),
#'        t  = c(1L, 2L, 1L, 2L, NA_integer_),
#'        x  = 11:15)
#' y1 = data.frame(id = 1:2,
#'                 y  = c(11L, 15L))
#' check_match_type(x = x1, y=y1, by="id", match_type = "m:1")
#' 
#' # Inconsistent match type 
#' check_match_type(x = x1, y=y1, by="id", match_type = "1:1")
#' 
check_match_type <- function(x, y, by, match_type, verbose) {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # computations   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  mts <- split_match_type(match_type)
  tx  <- mts[1]
  ty  <- mts[2]

  # Check which messages to return
  match_type_error <- FALSE
  x_m              <- TRUE
  y_m              <- TRUE

  if (tx == "1") {
    match_type_error <-
      is_match_type_error(x, "x", by, verbose, match_type_error)
  } else {
    x_m <- is_valid_m_key(x, by)
  }

  if (ty == "1") {
    match_type_error <-
      is_match_type_error(y, "y", by, verbose, match_type_error)
  } else {
      y_m <- is_valid_m_key(y, by)
    }

  # Error if user choses "1" but actually "m" ----
  if (match_type_error) {
    msg     <- "match type inconsistency"
    hint    <-
      "you could use `return_report = TRUE` in `joyn::is_id()`
    to see where the problem is"
    joyn_msg("err")
    cli::cli_abort(c(msg,
                     i = hint),
                   class = "joyn_error")

  }

  # Warning if user choses "m" but actually "1" ----
  m_m <- data.table::fcase(
    isTRUE(x_m)  & isTRUE(y_m),  "none",
    isTRUE(x_m)  & isFALSE(y_m), "warn_y",
    isFALSE(x_m) & isTRUE(y_m),  "warn_x",
    isFALSE(x_m) & isFALSE(y_m), "warn_both"
  )

  if (!m_m == "none") {

    switch(
      m_m,
      "warn_y" = {
        store_msg(
          type   = "warn",
          warn   = 'The keys supplied uniquely identify y therefore a `{tx}:1` join is executed.'
        )
      },
      "warn_x" = {
        store_msg(
          type   = "warn",
          warn   = 'The keys supplied uniquely identify x therefore a `1:{ty}` join is executed'
          )
      },
      "warn_both" = {
        store_msg(
          type   = "warn",
          warn   =  cli::symbol$record, "  ",
          warn   = 'The key/s supplied uniquely identifies both x and y therefore a `1:1` join executed.'
        )
      }
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
    by2 <- by
    store_msg("err",
              err = paste(cli::symbol$cross, "Error:"),
              "   table {.field {name}} is not uniquely identified
              by {.val {by2}}")

  }
  match_type_error
}



#' check variables in Y that will be kept in returning table
#'
#' @inheritParams merge
#'
#' @return character vector with variable names from Y table
#' @keywords internal
#' 
#' @example 
#' y1 = data.frame(id = 1:2,
#'                y  = c(11L, 15L))
#' # Keep y in y1
#' check_y_vars_to_keep(TRUE, y1, by = "id")

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


#' rename vars in y so they are different to x's when joined
#'
#' @param x master table
#' @param by character: by vars
#' @param y_vars_to_keep  character vector of variables to keep
#'
#' @return vector with new variable names for y
#' @keywords internal
#' 
#' @example
#' y_vars_to_keep <- check_y_vars_to_keep(TRUE, y2, by = "id")
#' check_new_y_vars(x = x2, by="id", y_vars_to_keep)

check_new_y_vars <- \(x, by, y_vars_to_keep) {
  xvars <- names(x)
  xvars <- xvars[!(xvars %in% by)]

  upvars <- intersect(xvars, y_vars_to_keep)

  if (length(upvars) != 0) {
    y.upvars <- paste0(upvars, ".y")
    y_vars_to_keep[y_vars_to_keep %in% upvars] <- y.upvars

    if (isFALSE(update_NAs) && isFALSE(update_values)) {
      store_msg(
        "note",
        note = cli::symbol$square_small_filled,
        "variable{?s} {.code {upvars}} in table {.field y} {?is/are}
                            ignored because arguments {.arg update_NAs} and
                            {.arg update_values} are FALSE.")
    }

  } # end of update vars


  return(y_vars_to_keep)
}



#' Check whether specified "many" relationship is valid
#'
#' @param dt data object
#' @param by character vector: specified keys, already fixed
#'
#' @return logical: `TRUE` if valid, `FALSE` if uniquely identified
#' @keywords internal
is_valid_m_key <- function(dt, by){

  # Argument checks
  if ( !is.character(by))
    stop("`by` argument must be character")

  # by <- unname(by)
  duplicates <-
    dt |>
    get_vars(by) |>
    any_duplicated()

  if (duplicates)
    TRUE
  else
    FALSE
}



check_suffixes <- function(suffixes) {

  if (length(suffixes) != 2) {
    cli::cli_abort("argumet {.arg suffixes} must be a character vector of length 2")
  }

}




