#-------------------------------------------------------------------------------
# LEFT JOIN --------------------------------------------------------------------
#-------------------------------------------------------------------------------


#' Left join two data frames
#'
#' This is a `joyn` wrapper that works in a similar
#' fashion to [dplyr::left_join]
#'
#' @param x data frame: referred to as *left* in R terminology, or *master* in
#'   Stata terminology.
#' @param y data frame: referred to as *right* in R terminology, or *using* in
#'   Stata terminology.
#' @param by a character vector of variables to join by. If NULL, the default,
#'   joyn will do a natural join, using all variables with common names across
#'   the two tables. A message lists the variables so that you can check they're
#'   correct (to suppress the message, simply explicitly list the variables that
#'   you want to join). To join by different variables on x and y use a vector
#'   of expressions. For example, `by = c("a = b", "z")` will use "a" in `x`, "b"
#'   in `y`, and "z" in both tables.
#' @inheritParams dplyr::left_join
#' @inheritParams joyn
#' @inheritDotParams joyn y_vars_to_keep update_values update_NAs reportvar
#'   reporttype keep_common_vars verbose
#'
#' @return data frame
#' @export
#'
#' @examples
#' # Simple left join
#' library(data.table)
#'
#' x1 = data.table(id = c(1L, 1L, 2L, 3L, NA_integer_),
#'                 t  = c(1L, 2L, 1L, 2L, NA_integer_),
#'                 x  = 11:15)
#' y1 = data.table(id = c(1,2, 4),
#'                 y  = c(11L, 15L, 16))
#' left_join(x1, y1, relationship = "many-to-one")
left_join <- function(
    x,
    y,
    by               = intersect(names(x), names(y)),
    copy             = FALSE,
    suffix           = c(".x", ".y"),
    keep             = NULL,
    na_matches       = c("na", "never"),
    multiple         = "all",
    unmatched        = "drop",
    relationship     = NULL,
    y_vars_to_keep   = TRUE,
    update_values    = FALSE,
    update_NAs       = update_values,
    reportvar        = getOption("joyn.reportvar"),
    reporttype       = c("character", "numeric"),
    roll             = NULL,
    keep_common_vars = FALSE,
    sort             = TRUE,
    verbose          = getOption("joyn.verbose"),
    ...
) {
  clear_joynenv()

  # Argument checks ---------------------------------
  # get args
  na_matches <- match.arg(
    na_matches,
    choices = c(
      "na",
      "never"
    )
  )

  multiple   <- match.arg(
    multiple,
    choices = c(
      "all",
      "any",
      "first",
      "last"
    )
  )

  unmatched  <- match.arg(
    unmatched,
    choices = c(
      "drop",
      "error"
    )
  )

  #check args
  args_check <- arguments_checks(x             = x,
                                 y             = y,
                                 by            = by,
                                 copy          = copy,
                                 keep          = keep,
                                 suffix        = suffix,
                                 na_matches    = na_matches,
                                 multiple      = multiple,
                                 relationship  = relationship,
                                 reportvar     = reportvar)



  # Column names -----------------------------------


  if (args_check$keep == TRUE) {

    jn_type = "left"
    modified_cols <- set_col_names(x      = x,
                                   y      = y,
                                   by     = args_check$by,
                                   jn_type= jn_type,
                                   suffix = args_check$suffix)
    x = modified_cols$x
    y = modified_cols$y
  }


  # left join checks --------------------------------


  # Do left join ------------------------------------
  lj <- joyn(
    x                = x,
    y                = y,
    by               = args_check$by,
    match_type       = args_check$relationship,
    keep             = "left",
    y_vars_to_keep   = y_vars_to_keep,
    suffixes         = args_check$suffix,
    update_values    = update_values,
    update_NAs       = update_NAs,
    reportvar        = args_check$reportvar,
    reporttype       = reporttype,
    keep_common_vars = T,
    sort             = sort,
    verbose          = verbose,
    ...
  )


  # Do filter ---------------------------------------

  ### unmatched == "error"
  if (unmatched == "error") {

    # If joining by same var in x and y
    if (length(grep(pattern = "==?", x = by, value = TRUE)) == 0) {

    # Input key
    y_keys      <- qDT(y[args_check$by])

    # Output key
    jn_key      <- qDT(lj[args_check$by])

    # Unmatched keys
    unmatched_keys <- fsetdiff(y_keys, jn_key)

    }

    # If joining by different vars in x and y

    else {

      # Input key
      x_1 = copy(x)
      y_1 = copy(y)

      # Output key
      y_by <- fix_by_vars(by = by, x_1, y_1)$yby
      y_keys <-  qDT(y[y_by])

      jn_key  <- qDT(lj[y_by])

      # Unmatched keys
      unmatched_keys <- fsetdiff(y_keys, jn_key)

    }

    # If there are unmatched keys that would result in dropped rows in output -> stop
    if(nrow(unmatched_keys) >0) {

      cli::cli_abort(
        paste0(
          cli::symbol$cross,
          " Error: some rows in `y` are not matched - this check is due to
           argument `unmatched = 'error'` "))
    }
  } # close if unmatched == "error" condition

  ### if dropreport = T
  if (args_check$dropreport == T) {
    get_vars(lj, args_check$reportvar) <- NULL
  }

  # return
  lj
}



#' Right join two data frames
#'
#' This is a `joyn` wrapper that works in a similar
#' fashion to [dplyr::right_join]
#'
#' @param x data frame: referred to as *left* in R terminology, or *master* in
#'   Stata terminology.
#' @param y data frame: referred to as *right* in R terminology, or *using* in
#'   Stata terminology.
#' @param by a character vector of variables to join by. If NULL, the default,
#'   joyn will do a natural join, using all variables with common names across
#'   the two tables. A message lists the variables so that you can check they're
#'   correct (to suppress the message, simply explicitly list the variables that
#'   you want to join). To join by different variables on x and y use a vector
#'   of expressions. For example, `by = c("a = b", "z")` will use "a" in `x`, "b"
#'   in `y`, and "z" in both tables.
#' @inheritParams dplyr::right_join
#' @inheritParams joyn
#' @inheritDotParams joyn y_vars_to_keep update_values update_NAs reportvar
#'   reporttype keep_common_vars verbose
#'
#' @return data frame
#' @export
#'
#' @examples
#' # Simple right join
#' library(data.table)
#'
#' x1 = data.table(id = c(1L, 1L, 2L, 3L, NA_integer_),
#'                 t  = c(1L, 2L, 1L, 2L, NA_integer_),
#'                 x  = 11:15)
#' y1 = data.table(id = c(1,2, 4),
#'                 y  = c(11L, 15L, 16))
#' right_join(x1, y1, relationship = "many-to-one")
right_join <- function(
    x,
    y,
    by               = intersect(names(x), names(y)),
    copy             = FALSE,
    suffix           = c(".x", ".y"),
    keep             = NULL,
    na_matches       = c("na", "never"),
    multiple         = "all",
    unmatched        = "drop",
    relationship     = "one-to-one",
    y_vars_to_keep   = TRUE,
    update_values    = FALSE,
    update_NAs       = update_values,
    reportvar        = getOption("joyn.reportvar"),
    reporttype       = c("character", "numeric"),
    roll             = NULL,
    keep_common_vars = FALSE,
    sort             = TRUE,
    verbose          = getOption("joyn.verbose"),
    ...
) {

  clear_joynenv()

  # Argument checks ---------------------------------
  na_matches <- match.arg(
    na_matches,
    choices = c(
      "na",
      "never"
    )
  )

  multiple   <- match.arg(
    multiple,
    choices = c(
      "all",
      "any",
      "first",
      "last"
    )
  )

  unmatched  <- match.arg(
    unmatched,
    choices = c(
      "drop",
      "error"
    )
  )

  #check args
  args_check <- arguments_checks(x             = x,
                                 y             = y,
                                 by            = by,
                                 copy          = copy,
                                 keep          = keep,
                                 suffix        = suffix,
                                 na_matches    = na_matches,
                                 multiple      = multiple,
                                 relationship  = relationship,
                                 reportvar     = reportvar)


  # Column names -----------------------------------
  if (args_check$keep == TRUE) {

    jn_type = "right"
    modified_cols <- set_col_names(x      = x,
                                   y      = y,
                                   by     = args_check$by,
                                   jn_type= jn_type,
                                   suffix = args_check$suffix)
    x = modified_cols$x
    y = modified_cols$y
  }


  # right join checks --------------------------------


  # Do right join ------------------------------------
  rj <- joyn(
    x                = x,
    y                = y,
    by               = args_check$by,
    match_type       = args_check$relationship,
    keep             = "right",
    y_vars_to_keep   = y_vars_to_keep,
    suffixes         = args_check$suffix,
    update_values    = update_values,
    update_NAs       = update_NAs,
    reportvar        = args_check$reportvar,
    reporttype       = reporttype,
    keep_common_vars = T,
    sort             = sort,
    verbose          = verbose,
    ...
  )


  # Do filter ---------------------------------------

  ### unmatched == "error"
  if (unmatched == "error") {

    # If joining by same var in x and y
    if (length(grep(pattern = "==?", x = by, value = TRUE)) == 0) {

      # Input key
      x_keys      <- qDT(x[args_check$by])

      # Output key
      jn_key      <- qDT(rj[args_check$by])

      # Unmatched keys
      unmatched_keys <- fsetdiff(x_keys, jn_key)

    }

    # If joining by different vars in x and y

    else {

      # Input key
      x_1 = copy(x)
      y_1 = copy(y)

      # Output key
      x_by <- fix_by_vars(by = by, x_1, y_1)$xby
      x_keys <-  qDT(x[x_by])

      jn_key  <- qDT(rj[x_by])

      # Unmatched keys
      unmatched_keys <- fsetdiff(x_keys, jn_key)

    }

    # If there are unmatched keys that would result in dropped rows in output -> stop
    if(nrow(unmatched_keys) >0) {

      cli::cli_abort(
        paste0(
          cli::symbol$cross,
          " Error: some rows in `x` are not matched - this check is due to
           argument `unmatched = 'error'` "))
    }
  } # close if unmatched == "error" condition


  ### if dropreport = T
  if (args_check$dropreport == T) {
    get_vars(rj, args_check$reportvar) <- NULL
  }

  # Return
  rj

}


#-------------------------------------------------------------------------------
# FULL JOIN --------------------------------------------------------------------
#-------------------------------------------------------------------------------


#' Full join two data frames
#'
#' This is a `joyn` wrapper that works in a similar
#' fashion to [dplyr::full_join]
#'
#' @param x data frame: referred to as *left* in R terminology, or *master* in
#'   Stata terminology.
#' @param y data frame: referred to as *right* in R terminology, or *using* in
#'   Stata terminology.
#' @param by a character vector of variables to join by. If NULL, the default,
#'   joyn will do a natural join, using all variables with common names across
#'   the two tables. A message lists the variables so that you can check they're
#'   correct (to suppress the message, simply explicitly list the variables that
#'   you want to join). To join by different variables on x and y use a vector
#'   of expressions. For example, `by = c("a = b", "z")` will use "a" in `x`, "b"
#'   in `y`, and "z" in both tables.
#' @inheritParams dplyr::full_join
#' @inheritParams joyn
#' @inheritDotParams joyn y_vars_to_keep update_values update_NAs reportvar
#'   reporttype keep_common_vars verbose
#'
#' @return data frame
#' @export
#'
#' @examples
#' # Simple full join
#' library(data.table)
#'
#' x1 = data.table(id = c(1L, 1L, 2L, 3L, NA_integer_),
#'                 t  = c(1L, 2L, 1L, 2L, NA_integer_),
#'                 x  = 11:15)
#' y1 = data.table(id = c(1,2, 4),
#'                 y  = c(11L, 15L, 16))
#' full_join(x1, y1, relationship = "many-to-one")
full_join <- function(
    x,
    y,
    by               = intersect(names(x), names(y)),
    copy             = FALSE,
    suffix           = c(".x", ".y"),
    keep             = NULL,
    na_matches       = c("na", "never"),
    multiple         = "all",
    unmatched        = "drop",
    relationship     = "one-to-one",
    y_vars_to_keep   = TRUE,
    update_values    = FALSE,
    update_NAs       = update_values,
    reportvar        = getOption("joyn.reportvar"),
    reporttype       = c("character", "numeric"),
    roll             = NULL,
    keep_common_vars = FALSE,
    sort             = TRUE,
    verbose          = getOption("joyn.verbose"),
    ...
) {
  clear_joynenv()

  # Argument checks ---------------------------------
  # get args
  na_matches <- match.arg(
    na_matches,
    choices = c(
      "na",
      "never"
    )
  )

  multiple   <- match.arg(
    multiple,
    choices = c(
      "all",
      "any",
      "first",
      "last"
    )
  )

  unmatched  <- match.arg(
    unmatched,
    choices = c(
      "drop",
      "error"
    )
  )

  #check args
  args_check <- arguments_checks(x             = x,
                                 y             = y,
                                 by            = by,
                                 copy          = copy,
                                 keep          = keep,
                                 suffix        = suffix,
                                 na_matches    = na_matches,
                                 multiple      = multiple,
                                 relationship  = relationship,
                                 reportvar     = reportvar)


  # Column names -----------------------------------
  if (args_check$keep == TRUE) {

    jn_type = "full"
    modified_cols <- set_col_names(x      = x,
                                   y      = y,
                                   by     = args_check$by,
                                   jn_type= jn_type,
                                   suffix = args_check$suffix)
    x = modified_cols$x
    y = modified_cols$y
  }


  # Do full join ------------------------------------
  fj <- joyn(
    x                = x,
    y                = y,
    by               = args_check$by,
    match_type       = args_check$relationship,
    keep             = "full",
    y_vars_to_keep   = y_vars_to_keep,
    suffixes         = args_check$suffix,
    update_values    = update_values,
    update_NAs       = update_NAs,
    reportvar        = args_check$reportvar,
    reporttype       = reporttype,
    keep_common_vars = T,
    sort             = sort,
    verbose          = verbose,
    ...
  )


  # Do filter ---------------------------------------
  ### unmatched == "error"
  if (unmatched == "error") {

    if (unmatched_keys(join    = fj,
                       jn_type = "full"))
    {cli::cli_abort(
      paste0(
        cli::symbol$cross,
        " Error: some rows in `x` and `y` are not matched - this check is due to
           argument `unmatched = 'error'` "
      )
    )
    }

  }

  ### if dropreport = T
  if (args_check$dropreport == T) {
    get_vars(fj, args_check$reportvar) <- NULL
  }

  # Return
  fj

}



#-------------------------------------------------------------------------------
# INNER JOIN --------------------------------------------------------------------
#-------------------------------------------------------------------------------


#' Inner join two data frames
#'
#' This is a `joyn` wrapper that works in a similar
#' fashion to [dplyr::inner_join]
#'
#' @param x data frame: referred to as *left* in R terminology, or *master* in
#'   Stata terminology.
#' @param y data frame: referred to as *right* in R terminology, or *using* in
#'   Stata terminology.
#' @param by a character vector of variables to join by. If NULL, the default,
#'   joyn will do a natural join, using all variables with common names across
#'   the two tables. A message lists the variables so that you can check they're
#'   correct (to suppress the message, simply explicitly list the variables that
#'   you want to join). To join by different variables on x and y use a vector
#'   of expressions. For example, `by = c("a = b", "z")` will use "a" in `x`, "b"
#'   in `y`, and "z" in both tables.
#' @inheritParams dplyr::inner_join
#' @inheritParams joyn
#' @inheritDotParams joyn y_vars_to_keep update_values update_NAs reportvar
#'   reporttype keep_common_vars verbose
#'
#' @return data frame
#' @export
#'
#' @examples
#' # Simple full join
#' library(data.table)
#'
#' x1 = data.table(id = c(1L, 1L, 2L, 3L, NA_integer_),
#'                 t  = c(1L, 2L, 1L, 2L, NA_integer_),
#'                 x  = 11:15)
#' y1 = data.table(id = c(1,2, 4),
#'                 y  = c(11L, 15L, 16))
#' inner_join(x1, y1, relationship = "many-to-one")
inner_join <- function(
    x,
    y,
    by               = intersect(names(x), names(y)),
    copy             = FALSE,
    suffix           = c(".x", ".y"),
    keep             = NULL,
    na_matches       = c("na", "never"),
    multiple         = "all",
    unmatched        = "drop",
    relationship     = "one-to-one",
    y_vars_to_keep   = TRUE,
    update_values    = FALSE,
    update_NAs       = update_values,
    reportvar        = getOption("joyn.reportvar"),
    reporttype       = c("character", "numeric"),
    roll             = NULL,
    keep_common_vars = FALSE,
    sort             = TRUE,
    verbose          = getOption("joyn.verbose"),
    ...
) {
  clear_joynenv()

  # Argument checks ---------------------------------
  # get args
  na_matches <- match.arg(
    na_matches,
    choices = c(
      "na",
      "never"
    )
  )

  multiple   <- match.arg(
    multiple,
    choices = c(
      "all",
      "any",
      "first",
      "last"
    )
  )

  unmatched  <- match.arg(
    unmatched,
    choices = c(
      "drop",
      "error"
    )
  )

  #check args
  args_check <- arguments_checks(x             = x,
                                 y             = y,
                                 by            = by,
                                 copy          = copy,
                                 keep          = keep,
                                 suffix        = suffix,
                                 na_matches    = na_matches,
                                 multiple      = multiple,
                                 relationship  = relationship,
                                 reportvar     = reportvar)

  # Column names -----------------------------------
  if (args_check$keep == TRUE) {

    jn_type = "inner"
    modified_cols <- set_col_names(x      = x,
                                   y      = y,
                                   by     = args_check$by,
                                   jn_type= jn_type,
                                   suffix = args_check$suffix)
    x = modified_cols$x
    y = modified_cols$y
  }


  # Do inner join ------------------------------------
  ij <- joyn(
    x                = x,
    y                = y,
    by               = args_check$by,
    match_type       = args_check$relationship,
    keep             = "inner",
    y_vars_to_keep   = y_vars_to_keep,
    suffixes         = args_check$suffix,
    update_values    = update_values,
    update_NAs       = update_NAs,
    reportvar        = args_check$reportvar,
    reporttype       = reporttype,
    keep_common_vars = T,
    sort             = sort,
    verbose          = verbose,
    ...
  )


  # Do filter ---------------------------------------

  ### unmatched == "error"
  if (unmatched == "error") {

    ##TOCHECK - unmatched keys from both data frames

  }


  ### if dropreport = T
  if (args_check$dropreport == T) {
    get_vars(ij, args_check$reportvar) <- NULL
  }

  # Return
  ij

}



# HELPER FUNCTIONS -------------------------------------------------------------
## Arguments checks ####

arguments_checks <- function(x, y, by, copy, keep, suffix, na_matches, multiple,
                             relationship, reportvar) {
  # Check by
  if (is.null(by)) {
    by <- intersect(
      names(x),
      names(y)
    )
  }

  # Check copy
  if (copy == TRUE) {
    store_msg(
      type        = "warn",
      warn        = paste(cli::symbol$warn, "\nWarning:"),
      pale        = "\nargument",
      bolded_pale = "  copy = TRUE",
      pale        = "\nis not active in this version of",
      bolded_pale = "  joyn"
    )
  }

  # Check suffix
  if (is.null(suffix) || !length(suffix) == 2 || !is.character(suffix)) {
    cli::cli_abort(
      paste0(
        cli::symbol$cross,
        " Error: argument `suffix` must be character vector of length 2"
      )
    )

  }

  # Check keep
  if (!is.null(keep) & !is.logical(keep)) {
    cli::cli_abort(
      paste0(
        cli::symbol$cross,
        " Error: argument `keep` should be one of NULL, TRUE, or FALSE"
      )
    )
  }
  if (is.null(keep)) {
    store_msg(
      type        = "warn",
      warn        = paste(cli::symbol$warn,"\nWarning:"),
      pale        = "  joyn does not currently allow inequality joins, so",
      bolded_pale = "  keep = NULL",
      pale        = "  will retain only keys in x"
    )
    keep <- FALSE
  }

  # Check multiple
  if (multiple == "any") {
    multiple <- "first"
  }

  # Check relationship
  if (is.null(relationship)) {relationship <- "one-to-one"}

  relationship <- switch(
    relationship,
    "one-to-one"   = "1:1",
    "one-to-many"  = "1:m",
    "many-to-one"  = "m:1",
    "many-to-many" = "m:m"
  )
  if (
    relationship %in% c("1:m", "m:m") &
    !multiple == "all"
  ) {
    cli::cli_abort(
      paste0(
        cli::symbol$cross,
        " Error: if `relationship` is 1:m or m:m then `multiple` should be 'all' "
      )
    )
  }

  # Check na_matches
  if (na_matches == "never") {
    store_msg(
      type        = "warn",
      warn        = paste(cli::symbol$warn, "\nWarning:"),
      pale        = "  Currently, joyn allows only",
      bolded_pale = "  na_matches = 'na'"
    )
  }

  # Check reportvar
  if (is.null(reportvar) || isFALSE(reportvar)) {
    dropreport <- TRUE
    reportvar <- getOption("joyn.reportvar")
  } else{
    dropreport <- FALSE
  }

  out <- list(by           = by,
              copy         = copy,
              suffix       = suffix,
              keep         = keep,
              na_matches   = na_matches,
              multiple     = multiple,
              relationship = relationship,
              reportvar    = reportvar,
              dropreport   = dropreport)

  return(out)


} # close function

## Column names ####

set_col_names <- function(x, y, by, suffix, jn_type) {

  x_1 <- copy(x)
  y_1 <- copy(y)

  if (length(grep(pattern = "==?", x = by, value = TRUE)) != 0) {

    if (jn_type == "right") {
      by_x_names <- fix_by_vars(by = by, x_1, y_1)$xby
    }

    else if (jn_type == "left" | jn_type == "full" | jn_type == "inner") {
      by_y_names <- fix_by_vars(by = by, x_1, y_1)$yby
    }

  } else {
    by_y_names <- by_x_names <- fix_by_vars(by = by, x_1, y_1)$by
    }


  if (jn_type == "right") {
    xkeys <- x |>
      fselect(by_x_names)
    names(xkeys) <- paste0(names(xkeys), suffix[1])
    x <- cbind(
      xkeys,
      x
    )
  } else if (jn_type == "left" | jn_type == "full" | jn_type == "inner")  {

    ykeys <- y |>
      fselect(by_y_names)
    names(ykeys) <- paste0(names(ykeys), suffix[2])
    y <- cbind(
      ykeys,
      y
    )

  } #close else

  return(list(x = x, y = y))

} #close function


## Unmatched error ####
















