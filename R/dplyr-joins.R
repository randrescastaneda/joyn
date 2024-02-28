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
  #update args
  by           <- args_check$by
  keep         <- args_check$keep
  na_matches   <- args_check$na_matches
  multiple     <- args_check$multiple
  relationship <- args_check$relationship
  reportvar    <- args_check$reportvar
  dropreport   <- args_check$dropreport

  # Column names -----------------------------------


  if (keep == TRUE) {

    jn_type = "left"
    modified_cols <- set_col_names(x      = x,
                                   y      = y,
                                   by     = by,
                                   jn_type= jn_type,
                                   suffix = suffix)
    x = modified_cols$x
    y = modified_cols$y
  }


  # left join checks --------------------------------


  # Do left join ------------------------------------
  lj <- joyn(
    x                = x,
    y                = y,
    by               = by,
    match_type       = relationship,
    keep             = "left",
    y_vars_to_keep   = y_vars_to_keep,
    suffixes         = suffix,
    update_values    = update_values,
    update_NAs       = update_NAs,
    reportvar        = reportvar,
    reporttype       = reporttype,
    keep_common_vars = T,
    sort             = sort,
    verbose          = verbose,
    ...
  )


  # Do filter ---------------------------------------

  ### unmatched == "error"

  if (unmatched == "error") {

    if (unmatched_keys(join    = lj,
                       jn_type = "left")) {
      cli::cli_abort(
      paste0(
        cli::symbol$cross,
        " Error: some rows in `y` are not matched - this check is due to
           argument `unmatched = 'error'` ")
      )
    }
    }

  ### if dropreport = T
  if (dropreport == T) {
    get_vars(lj, reportvar) <- NULL
  }

  # return
  lj
}

#-------------------------------------------------------------------------------
# RIGHT JOIN --------------------------------------------------------------------
#-------------------------------------------------------------------------------

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

  # Argument checks ------------------------------------------------------------
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
  #update args
  by           <- args_check$by
  keep         <- args_check$keep
  na_matches   <- args_check$na_matches
  multiple     <- args_check$multiple
  relationship <- args_check$relationship
  reportvar    <- args_check$reportvar
  dropreport   <- args_check$dropreport

  # Column names -----------------------------------
  if (keep == TRUE) {

    jn_type = "right"
    modified_cols <- set_col_names(x      = x,
                                   y      = y,
                                   by     = by,
                                   jn_type= jn_type,
                                   suffix = suffix)
    x = modified_cols$x
    y = modified_cols$y
  }


  # right join checks --------------------------------


  # Do right join ------------------------------------
  rj <- joyn(
    x                = x,
    y                = y,
    by               = by,
    match_type       = relationship,
    keep             = "right",
    y_vars_to_keep   = y_vars_to_keep,
    suffixes         = suffix,
    update_values    = update_values,
    update_NAs       = update_NAs,
    reportvar        = reportvar,
    reporttype       = reporttype,
    keep_common_vars = T,
    sort             = sort,
    verbose          = verbose,
    ...
  )


  # Do filter ---------------------------------------

  ### unmatched == "error"
  if (unmatched == "error") {

    if (unmatched_keys(join    = rj,
                       jn_type = "right")) {
      cli::cli_abort(
      paste0(
        cli::symbol$cross,
        " Error: some rows in `x` are not matched - this check is due to
           argument `unmatched = 'error'` ")
      )
    }
  }

  ### if dropreport = T
  if (dropreport == T) {
    get_vars(rj, reportvar) <- NULL
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

  #update args
  by           <- args_check$by
  keep         <- args_check$keep
  na_matches   <- args_check$na_matches
  multiple     <- args_check$multiple
  relationship <- args_check$relationship
  reportvar    <- args_check$reportvar
  dropreport   <- args_check$dropreport


  # Column names -----------------------------------
  if (keep == TRUE) {

    jn_type = "full"
    modified_cols <- set_col_names(x      = x,
                                   y      = y,
                                   by     = by,
                                   jn_type= jn_type,
                                   suffix = suffix)
    x = modified_cols$x
    y = modified_cols$y
  }


  # Do full join ------------------------------------
  fj <- joyn(
    x                = x,
    y                = y,
    by               = by,
    match_type       = relationship,
    keep             = "full",
    y_vars_to_keep   = y_vars_to_keep,
    suffixes         = suffix,
    update_values    = update_values,
    update_NAs       = update_NAs,
    reportvar        = reportvar,
    reporttype       = reporttype,
    keep_common_vars = T,
    sort             = sort,
    verbose          = verbose,
    ...
  )


  # Do filter ---------------------------------------


  ### unmatched == "error"
  if (unmatched == "error") {

    # Store warning message
    store_msg(
      type        = "warn",
      warn        = paste(cli::symbol$warn, "\nWarning:"),
      pale        = "\nargument",
      bolded_pale = "  warning = error",
      pale        = "\nis not active in this type of",
      bolded_pale = "  joyn"
    )

  } # close if unmatched == "error" condition

  ### if dropreport = T
  if (dropreport == T) {
    get_vars(fj, reportvar) <- NULL
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

  #update args
  by           <- args_check$by
  keep         <- args_check$keep
  na_matches   <- args_check$na_matches
  multiple     <- args_check$multiple
  relationship <- args_check$relationship
  reportvar    <- args_check$reportvar
  dropreport   <- args_check$dropreport

  # Column names -----------------------------------
  if (keep == TRUE) {

    jn_type = "inner"
    modified_cols <- set_col_names(x      = x,
                                   y      = y,
                                   by     = by,
                                   jn_type= jn_type,
                                   suffix = suffix)
    x = modified_cols$x
    y = modified_cols$y
  }


  # Do inner join ------------------------------------
  ij <- joyn(
    x                = x,
    y                = y,
    by               = by,
    match_type       = relationship,
    keep             = "inner",
    y_vars_to_keep   = y_vars_to_keep,
    suffixes         = suffix,
    update_values    = update_values,
    update_NAs       = update_NAs,
    reportvar        = reportvar,
    reporttype       = reporttype,
    keep_common_vars = T,
    sort             = sort,
    verbose          = verbose,
    ...
  )


  # Do filter ---------------------------------------

  ### unmatched == "error"
  if (unmatched == "error") {

    # 1. Detect unmacthed keys that would result in dropped rows in output
    # Case a. If joining by same var in x and y
    if (length(grep(pattern = "==?", x = by, value = TRUE)) == 0) {

      # Input key
      x_keys <- qDT(x[by])
      y_keys <- qDT(y[by])

      # Output key
      jn_key <- qDT(ij[by])

      # Unmatched keys
      unmatched_keys_x <- fsetdiff(x_keys, jn_key)
      unmatched_keys_y <- fsetdiff(x_keys, jn_key)
    }

    # Case b. If joining by var of different names in x and y

    else if (length(grep(pattern = "==?", x = by, value = TRUE)) != 0 & length(by) == 1) {
      x_by <- trimws(gsub("([^=]+)(\\s*==?\\s*)([^=]+)", "\\1", by))
      y_by <- trimws(gsub("([^=]+)(\\s*==?\\s*)([^=]+)", "\\3", by))

      x_keys <- qDT(x[x_by])
      y_keys <- qDT(y[y_by]) |> setnames(y_by, x_by) #rename key to use fsetdiff

      jn_key <- qDT(ij[x_by])

      unmatched_keys_x <- fsetdiff(x_keys, jn_key)
      unmatched_keys_y <- fsetdiff(y_keys, jn_key)

    }

    # Case c. If joining by different vars in x and y

    else if (
      length(grep(pattern = "==?", x = by, value = TRUE)) == 0 & length(by) == 2) {

      x_by <- by[1]
      y_by <- by[2]

      x_keys <- qDT(x[x_by])
      y_keys <- qDT(y[y_by])

      jn_key_x <- qDT(ij[x_by])
      jn_key_y <- qDT(ij[y_by])

      unmatched_keys_x <- fsetdiff(x_keys, jn_key_x)
      unmatched_keys_y <- fsetdiff(y_keys, jn_key_y)

      }

    # Case d. If joining by different vars in x and y - using also vars of diff names!!!
    else {

      #if (length(grep(pattern = "==?", x = by, value = TRUE)) == 1 ) {
        #ADD
      #}

      if(length(grep(pattern = "==?", x = by, value = TRUE)) == 2 & length(by) == 2) {
        x_by_1 <- trimws(gsub("([^=]+)(\\s*==?\\s*)([^=]+)", "\\1", by[1]))
        y_by_1 <- trimws(gsub("([^=]+)(\\s*==?\\s*)([^=]+)", "\\3", by[1]))


        x_by_2 <- trimws(gsub("([^=]+)(\\s*==?\\s*)([^=]+)", "\\1", by[2]))
        y_by_2 <- trimws(gsub("([^=]+)(\\s*==?\\s*)([^=]+)", "\\3", by[2]))

        x_key_1 <- qDT(x[x_by_1])
        y_key_1 <- qDT(y[y_by_1]) |> setnames(y_by_1, x_by_1)
        x_key_2 <- qDT(x[x_by_2])
        y_key_2 <- qDT(y[y_by_2]) |> setnames(y_by_2, x_by_2)


        jn_key_1 <- qDT(ij[x_by_1])
        jn_key_2 <- qDT(ij[x_by_2])

        #add unmatched keys x and y here!
        unmatched_keys_x <- fsetdiff(x_key_1, jn_key_1)
        unmatched_keys_y <- fsetdiff(y_key_1, jn_key_1)
        unmatched_keys_y <- fsetdiff(y_key_2, jn_key_2)
        unmatched_keys_y <- fsetdiff(y_key_2, jn_key_2)

      }

      else {unmatched_keys_x <- unmatched_keys_y <- data.frame()} #MEMO: to fix

    }

  # 2. If there are unmatched keys that would result in dropped rows in output
  #    -> stop

    if (nrow(unmatched_keys_y) >0 | nrow(unmatched_keys_x) >0) {
      cli::cli_abort(
        paste0(
          cli::symbol$cross,
          " Error: some rows in `x` and/or `y` are not matched - this check is due to
           argument `unmatched = 'error'` "))
    }

  } # close if unmatched == "error" condition


  ### if dropreport = T
  if (dropreport == T) {
    get_vars(ij, reportvar) <- NULL
  }

  # Return
  ij

}


# HELPER FUNCTIONS -------------------------------------------------------------
## Arguments checks ####

#' Perform necessary preliminary checks on arguments
#' @return list of checked arguments to pass on to the main join function
#' @keywords internal

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

#' Add x key var and y key var (with suffixes) to x and y
#' -when joining by different variables and keep is true
#' @param x data table: left table
#' @param y data table: right table
#' @param by character vector of variables to join by
#' @param suffix character(2) specifying the suffixes to be used for making non-by column names unique
#' @param jn_type character specifying type of join
#' @return list containing x and y
#' @keywords internal

set_col_names <- function(x, y, by, suffix, jn_type) {

  x_1 <- copy(x)
  y_1 <- copy(y)

  # If joining by different variables
  if (length(grep(pattern = "==?", x = by, value = TRUE)) != 0) {

    if (jn_type == "right") {
      by_x_names <- fix_by_vars(by = by, x_1, y_1)$xby
    }

    else if (jn_type == "left" | jn_type == "full" | jn_type == "inner") {
      by_y_names <- fix_by_vars(by = by, x_1, y_1)$yby
    }

  }

  # If joining by common var
  else {
    by_y_names <- by_x_names <- fix_by_vars(by = by, x_1, y_1)$by
    }

  # Add key vars with suffix to x and y
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

  return(list(x = x,
              y = y))

} #close function

## Detect unmatched keys - to be used in left and right joins ####

#' Detect unmatched keys
#' @param join joined data table, output of either a left or a right join
#' @param jn_type character specifying type of join
#' @return logical TRUE if unmacthed keys are found, FALSE if no unmacthed keys are found
#' @keywords internal

unmatched_keys <- function(join, jn_type) {

  unmatched_keys <- FALSE

  # Check unmatched keys in input

  # If right join - check x

  if (jn_type == "right") {

    if (any(
      join[, ncol(join)] == "y" |
      join[, ncol(join)] == 2
    )
    )

    {unmatched_keys <- TRUE}

  }

  # If left - check y

  else if (jn_type == "left") {

    if (any(
      join[, ncol(join)] == "x" |
      join[, ncol(join)] == 1
    )
    )

    {unmatched_keys <- TRUE}

  }

  return(unmatched_keys)
}












