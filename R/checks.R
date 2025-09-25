#' Check tables X and Y
#'
#' This function performs checks inspired on merge.data.table: it detects errors
#'  * if x and/or y have no columns
#'  * if x and/or y contain duplicate column names
#'
#' @inheritParams joyn
#'
#' @return invisible TRUE
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' # Check passing with no errors
#' library(data.table)
#' x1 = data.table(id = c(1L, 1L, 2L, 3L, NA_integer_),
#'                 t  = c(1L, 2L, 1L, 2L, NA_integer_),
#'                 x  = 11:15)
#' y1 = data.table(id = c(1,2, 4),
#'                 y  = c(11L, 15L, 16))
#' joyn:::check_xy(x = x1, y=y1)
#' }
check_xy  <- function(x,y) {

  error_exists <- FALSE

  # check no columns --------------

  x0 = length(x) == 0L
  y0 = length(y) == 0L

  if (x0 || y0) {
    error_exists <- TRUE
    if (x0 && y0) {
      xy <- c("x", "y")
      store_joyn_msg(err = "   Neither {.or {.strongTable {xy}}} table has columns.")
    } else if (x0) {
      store_joyn_msg(err = "   Input table {.strongTable x} has no columns.")
    } else {
      store_joyn_msg(err = "   Input table {.strongTable y} has no columns.")
    }

  }

  # -----------------------
  # Check no rows
  # -----------------------

  x_rows0 <- nrow(x) == 0L
  y_rows0 <- nrow(y) == 0L

  if (x_rows0 || y_rows0) {
    error_exists <- TRUE
    if (x_rows0 && y_rows0) {
      xy <- c("x", "y")
      store_joyn_msg(err = "   Neither {.or {.strongTable {xy}}} table has rows.")
    } else if (x_rows0) {
      store_joyn_msg(err = "   Input table {.strongTable x} has no rows.")
    } else {
      store_joyn_msg(err = "   Input table {.strongTable y} has no rows.")
    }
  }

  # check names -----------

  error_exists <- error_exists || check_duplicate_names(x, "x")
  error_exists <- error_exists || check_duplicate_names(y, "y")

  if (error_exists) {
    joyn_msg("err")
    cli::cli_abort("wrong input specification")
  }
  return(invisible(TRUE))
}


#' Check if vars in dt have duplicate names
#'
#' @param dt data.frame to check
#' @param name var name to check if has duplicates in dt
#' @return logical either TRUE, if any duplicates are found, or FALSE otherwise
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' # When no duplicates
#' x1 = data.table(id = c(1L, 1L, 2L, 3L, NA_integer_),
#'                 t  = c(1L, 2L, 1L, 2L, NA_integer_),
#'                 x  = 11:15)
#' joyn:::check_duplicate_names(x1, "x")
#'
#' # When duplicates
#' x1_duplicates = data.frame(id = c(1L, 1L, 2L, 3L, NA_integer_),
#'                            x  = c(1L, 2L, 1L, 2L, NA_integer_),
#'                            x  = 11:15,
#'                            check.names = FALSE)
#' joyn:::check_duplicate_names(x1_duplicates, "x")
#' }
check_duplicate_names <- \(dt, name) {
  nm_x = names(dt)
  if (anyDuplicated(nm_x)) {
    dups <- nm_x[duplicated(nm_x)] |>
      unique()
    store_joyn_msg(err    = " Table {.strongTable {name}} has the following {cli::qty(length(dups))} column{?s} duplicated:
                   {.strongVar {dups}}. \nPlease rename or remove and try again.")
    return(TRUE)
  }
  return(FALSE)
}

#' Check reporting variable
#'
#' check reportvar input <br> If resulting data frame has a reporting variable (storing joyn's report), check and return a valid name.
#'
#' @inheritParams merge
#' @keywords internal
#' @return if input reportvar is character, return valid name for the report var. If NULL or FALSE, return NULL.
#' @examples
#' \dontrun{
#' # When null - reporting variable not returned in merged dt
#' joyn:::check_reportvar(reportvar = NULL)
#' # When FALSE - reporting variable not returned in merged dt
#' joyn:::check_reportvar(reportvar = FALSE)
#' # When character
#' joyn:::check_reportvar(reportvar = ".joyn")
#' }

check_reportvar <-
  function(reportvar, verbose = getOption("joyn.verbose")) {

    if (is.character(reportvar)) {

      reportvar <- rename_to_valid(reportvar, verbose)

      store_joyn_msg(info = "Joyn's report available in variable {.strongVar {reportvar}}")

      return(reportvar)

    } else if (is.null(reportvar) || isFALSE(reportvar)) {

     store_joyn_msg(info = "  Reporting variable is {.strong NOT} returned")

      return(NULL)
    } else  {
      cli::cli_abort("{.strongArg reportvar} should be character, NULL or FALSE")
    }
  }



#' Check `by` input
#'
#' This function checks the variable name(s) to be used as key(s) of the join
#'
#' @inheritParams merge
#'
#' @return list with information about by variables
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' x1 = data.frame(
#'        id = c(1L, 1L, 2L, 3L, NA_integer_),
#'        t  = c(1L, 2L, 1L, 2L, NA_integer_),
#'        x  = 11:15)
#' y1 = data.frame(id = 1:2,
#'                 y  = c(11L, 15L))
#' # With var "id" shared in x and y
#' joyn:::check_by_vars(by = "id", x = x1, y = y1)
#'}
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

  # ~~~~~~~~~~~~~~~~ #
  # Check class  ####

  check_x_by <- check_var_class(dt = x,
                                if (length(fixby$xby)) fixby$tempkey else fixby$by)

  check_y_by <- check_var_class(dt = y,
                                if (length(fixby$yby)) fixby$tempkey else fixby$by)

  if (!is.null(check_x_by) || !is.null(check_y_by)) {
    joyn_msg()  # show stored messages first
    # cli::cli_abort(
    #   "Aborting join due to unsupported class for join variables"
    # )
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Return   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  return(fixby)

}

#' Check join variable class
#'
#' Checks if a variable in a data.table is of a supported class for joining.
#' Stores a warning via `store_joyn_msg()` if unsupported.
#'
#' @param dt data.table containing the variable
#' @param var Name of the variable to check
#' @return Variable name invisibly if unsupported, otherwise NULL
#' @keywords internal
check_var_class <- function(dt, var) {

  allowed_classes <- c("character", "integer", "numeric",
                       "factor", "logical", "Date", "POSIXct")

  bad_vars <- lapply(var, function(v) {
    primary_class <- class(dt[[v]])[1]

    if (!(primary_class %in% allowed_classes)) {
      store_joyn_msg(
        warn = glue::glue(
          "Join `by` var of class {primary_class}
          may cause issues. Consider coercing it to a standard type (e.g. character)."
        )
      )
      return(v)
    }
    NULL
  })

  bad_vars <- unlist(bad_vars, use.names = FALSE)

  if (length(bad_vars) > 0) invisible(bad_vars) else NULL
}





#' Check match type consistency
#'
#' This function checks if the match type chosen by the user is consistent with the data.
#' <br>(Match type must be one of the valid types: "1:1", "1:m", "m:1", "m:m")
#'
#' @inheritParams merge
#'
#' @return character vector from [split_match_type]
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' # Consistent match type
#' x1 = data.frame(
#'        id = c(1L, 1L, 2L, 3L, NA_integer_),
#'        t  = c(1L, 2L, 1L, 2L, NA_integer_),
#'        x  = 11:15)
#' y1 = data.frame(id = 1:2,
#'                 y  = c(11L, 15L))
#' joyn:::check_match_type(x = x1, y=y1, by="id", match_type = "m:1")
#'
#' # Inconsistent match type
#' joyn:::check_match_type(x = x1, y=y1, by="id", match_type = "1:1")
#' }
check_match_type <- function(x, y, by,
                             match_type,
                             verbose = getOption("joyn.verbose")) {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # computations   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  mts <- split_match_type(match_type)
  tx  <- mts[1]
  ty  <- mts[2]

  # Check which messages to return
  match_type_error <- FALSE
  x_m    <-  y_m   <- TRUE
  mte_x  <- mte_y  <- FALSE

  if (tx == "1") {
      mte_x <- is_match_type_error(x, "x", by, verbose, match_type_error)
  } else {
    x_m <- is_valid_m_key(x, by)
  }

  if (ty == "1") {
      mte_y <- is_match_type_error(y, "y", by, verbose, match_type_error)
  } else {
      y_m <- is_valid_m_key(y, by)
    }

  if (TRUE %in% c(mte_x, mte_y)) {
    match_type_error <-TRUE
    }

  # Error if user chooses "1" but actually "m" ----
  if (match_type_error) {

    msg     <- "match type inconsistency"
    hint    <-
      "set verbose to TRUE to see where the issue is"
    joyn_msg("err")

    if (verbose == TRUE) {

      msg     <- "match type inconsistency"
      hint    <-
        "refer to the duplicate counts in the table(s) above
       to identify where the issue occurred"

      if (mte_x == TRUE) {
        display_id_x <- is_id(x, by, return_report = TRUE, verbose = FALSE) |>
          fsubset(copies > 1)

        cli::cli_inform("Duplicate counts in {.field x}:")
        print(display_id_x)
        # I would like to show the table with the duplicated values.
        # Something like this:
        #      dt <- collapse::join(x, display_id_x,
        #      on = by,
        #      how = "inner",
        #      verbose = FALSE)
        #      dt[]
      }

      if (mte_y == TRUE) {
        display_id_y <- is_id(y, by, return_report = TRUE, verbose = FALSE) |>
          fsubset(copies > 1)

        cli::cli_inform("Duplicate counts in {.field y}:")
        print(display_id_y)
      }

    }

    cli::cli_abort(c(msg,
                     i = hint),
                     class = "joyn_error")

  }

  # Warning if user chooses "m" but actually "1" ----
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
        store_joyn_msg(warn = "The keys supplied uniquely identify {.strongTable y},
                               therefore a {.strong {tx}:1} join is executed")
      },

      "warn_x" = {
        store_joyn_msg(warn = "The keys supplied uniquely identify {.strongTable x},
                               therefore a {.strong 1:{ty}} join is executed")
      },

      #},
      "warn_both" = {
        store_joyn_msg(warn = "The keys supplied uniquely identify both {.strongTable x and y},
                               therefore a {.strong 1:1} join is executed")
      }
    )

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Return   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  return(mts)

}

#' Confirm if match type error
#'
#'
#' @inheritParams merge
#' @param name name of data frame
#' @param match_type_error  logical: from existing code
#'
#' @return logical
#' @keywords internal
#' @examples
#' \dontrun{
#' # example with dt not uniquely identified by "id"
#' x1 = data.table(id = c(1L, 1L, 2L, 3L, NA_integer_),
#'                 t  = c(1L, 2L, 1L, 2L, NA_integer_),
#'                 x  = 11:15)
#' joyn:::is_match_type_error(x1, name = "x1", by = "id")
#' }

is_match_type_error <- function(x, name, by, verbose, match_type_error) {

  isidx <- is_id(x, by = by, verbose = FALSE)

  if (isFALSE(isidx)) {

    match_type_error <- TRUE
    by2 <- by
    store_joyn_msg(err = "table {.strongTable {name}} is not uniquely identified by {.strongVar {by2}}")

  }
  match_type_error
  }



#' Check variables in y that will be kept in returning table
#'
#' check and return variable names in y to keep in returning table, excluding those that are keys of the merge
#'
#' @inheritParams merge
#' @param y_vars_to_keep either TRUE, if keep all vars in `y`;
#'        FALSE or NULL, if keep no vars; or character vector specifying which variables in `y` to keep
#' @param y data frame
#' @return character vector with variable names from `y` table
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' y1 = data.table(id = 1:2,
#'                y  = c(11L, 15L))
#' # With y_vars_to_keep TRUE
#' joyn:::check_y_vars_to_keep(TRUE, y1, by = "id")
#' # With y_vars_to_keep FALSE
#' joyn:::check_y_vars_to_keep(FALSE, y1, by = "id")
#' # Specifying which y vars to keep
#' joyn:::check_y_vars_to_keep("y", y1, by = "id")
#' }

check_y_vars_to_keep <- function(y_vars_to_keep, y, by) {

  if (length(y_vars_to_keep) > 1 && !is.character(y_vars_to_keep)) {
    cli::cli_abort("argumet {.arg {y_vars_to_keep}} must be of length 1
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
      store_joyn_msg(info = "Removing key variables {.strongVar {y_vars_to_keep[y_in_by]}} from {.strongVar {y_vars_to_keep}}")
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


#' Rename vars in y so they are different to x's when joined
#'
#' Check vars in y with same names as vars in x, and return new variables names for those y vars for the joined data frame
#'
#' @param x master table
#' @param by character: by vars
#' @param y_vars_to_keep  character vector of y variables to keep
#'
#' @return vector with new variable names for y
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' y2 = data.frame(id = c(1, 2, 5, 6, 3),
#'                 yd = c(1, 2, 5, 6, 3),
#'                 y  = c(11L, 15L, 20L, 13L, 10L),
#'                 x  = c(16:20))
#' joyn:::y_vars_to_keep <- check_y_vars_to_keep(TRUE, y2, by = "id")
#' x2 = data.frame(id = c(1, 1, 2, 3, NA),
#'                 t  = c(1L, 2L, 1L, 2L, NA_integer_),
#'                 x  = c(16, 12, NA, NA, 15))
#' joyn:::check_new_y_vars(x = x2, by="id", y_vars_to_keep)
#' }

check_new_y_vars <- \(x, by, y_vars_to_keep) {
  xvars <- names(x)
  xvars <- xvars[!(xvars %in% by)]

  upvars <- intersect(xvars, y_vars_to_keep)

  if (length(upvars) != 0) {
    y.upvars <- paste0(upvars, ".y")
    y_vars_to_keep[y_vars_to_keep %in% upvars] <- y.upvars

 #   if (isFALSE(update_NAs) && isFALSE(update_values)) {
#      store_msg(
#        "note",
#        ok          = paste(cli::symbol$info, "  ", cli::symbol$pointer, "  "),
#        pale        = "variable{?s} ",
#        bolded_pale = "{upvars}",
#        pale        = "  in table",
#        bolded_pale = "  {y}",
#        pale        = "  {?is/are} ignored because arguments",
#        bolded_pale = "  update_NAs and update_values",
#        pale        = "  are FALSE.")
#    }

  } # end of update vars


  return(y_vars_to_keep)
}



#' Check whether specified "many" relationship is valid
#'
#' When "many" relationship is specified, check if it is valid. <br> (Specified many relationship not valid if the dt is instead uniquely identified by specified keys)
#'
#' @param dt data object
#' @param by character vector: specified keys, already fixed
#'
#' @return logical: `TRUE` if valid, `FALSE` if uniquely identified
#' @keywords internal
#' @examples
#' \dontrun{
#' # example with data frame uniquely identified by specified `by` vars
#' x1 = data.frame(id  = c(1L, 1L, 2L, 3L, NA_integer_),
#'                  t  = c(1L, 2L, 1L, 2L, NA_integer_),
#'                  x  = 11:15)
#'
#' joyn:::is_valid_m_key(x1, by = c("id", "t"))

#' # example with valid specified "many" relationship
#' x2 = data.frame(id  = c(1L, 1L, 1L, 3L, NA_integer_),
#'                  t  = c(1L, 2L, 1L, 2L, NA_integer_),
#'                  x  = 11:15)
#' joyn:::is_valid_m_key(x2, by = c("id", "t"))
#' }
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
