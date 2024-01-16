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
  if (is.null(by)) {
    by <- intersect(
      names(x),
      names(y)
    )
  }
  if (copy == TRUE) {
    store_msg(
      type = "warn",
      warn = "Warning: that argument `copy = TRUE` is not active in this version of `joyn`"
    )
  }
  if (is.null(suffix) || !length(suffix) == 2 || !is.character(suffix)) {
    cli::cli_abort(
      paste0(
        cli::symbol$cross,
        " Error: argument `suffix` must be character vector of length 2"
      )
    )

  }
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
      type = "warn",
      warn = "Warning: `joyn` does not currently allow inequality joins, so `keep = NULL` will retain only keys in `x`"
    )
    keep <- FALSE
  }

  na_matches <- match.arg(na_matches)
  multiple   <- match.arg(
    multiple,
    choices = c(
      "all",
      "any",
      "first",
      "last"
    )
  )
  if (multiple == "any") {
    multiple <- "first"
  }
  unmatched  <- match.arg(
    unmatched,
    choices = c(
      "drop",
      "error"
    )
  )
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
  na_matches <- match.arg(
    na_matches,
    choices = c(
      "na",
      "never"
    )
  )
  if (na_matches == "never") {
    store_msg(
      type = "warn",
      err  = "Warning: ",
      warn = "Currently, `joyn` allows only `na_matches = 'na'`"
    )
  }
  if (is.null(reportvar) || isFALSE(reportvar)) {
    dropreport <- TRUE
    reportvar <- getOption("joyn.reportvar")
  } else{
    dropreport <- FALSE
  }

  # Column names -----------------------------------
  #xnames <- names(x)
  #ynames <- names(y)
  if (keep == TRUE) {

    x_1 <- copy(x)
    y_1 <- copy(y)

    if (length(grep(pattern = "==?", x = by, value = TRUE)) != 0) {
      by_y_names <- fix_by_vars(by = by, x_1, y_1)$yby
    } else {
      by_y_names <- fix_by_vars(by = by, x_1, y_1)$by
    }

    ykeys <- y |>
      fselect(by_y_names)
    names(ykeys) <- paste0(names(ykeys), suffix[2])
    y <- cbind(
      ykeys,
      y
    )
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
    if (any(
      lj[
        ,
        get(names(lj)[length(lj)])
      ] == "x"
    ) |
    any(
      lj[
        ,
        get(names(lj)[length(lj)])
      ] == 1
    )
    ) {

      cli::cli_abort(
        paste0(
          cli::symbol$cross,
          " Error: some rows in `y` are not matched - this check is due to
           argument `unmatched = 'error'` "
        )
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
  if (is.null(by)) {
    by <- intersect(
      names(x),
      names(y)
    )
  }
  if (copy == TRUE) {
    store_msg(
      type = "warn",
      warn = "Warning: that argument `copy = TRUE` is not active in this version of `joyn`"
    )
  }
  if (is.null(suffix) || !length(suffix) == 2 || !is.character(suffix)) {
    cli::cli_abort(
      paste0(
        cli::symbol$cross,
        " Error: argument `suffix` must be character vector of length 2"
      )
    )

  }
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
      type = "warn",
      warn = "Warning: `joyn` does not currently allow inequality joins, so `keep = NULL` will retain only keys in `x`"
    )
    keep <- FALSE
  }

  na_matches <- match.arg(na_matches)
  multiple   <- match.arg(
    multiple,
    choices = c(
      "all",
      "any",
      "first",
      "last"
    )
  )
  if (multiple == "any") {
    multiple <- "first"
  }
  unmatched  <- match.arg(
    unmatched,
    choices = c(
      "drop",
      "error"
    )
  )
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
  na_matches <- match.arg(
    na_matches,
    choices = c(
      "na",
      "never"
    )
  )
  if (na_matches == "never") {
    store_msg(
      type = "warn",
      err  = "Warning: ",
      warn = "Currently, `joyn` allows only `na_matches = 'na'`"
    )
  }
  if (is.null(reportvar) || isFALSE(reportvar)) {
    dropreport <- TRUE
    reportvar <- getOption("joyn.reportvar")
  } else{
    dropreport <- FALSE
  }

  # Column names -----------------------------------
  if (keep == TRUE) {

    x_1 <- copy(x)
    y_1 <- copy(y)

    if (length(grep(pattern = "==?", x = by, value = TRUE)) != 0) {
      by_x_names <- fix_by_vars(by = by, x_1, y_1)$xby
    } else {
      by_x_names <- fix_by_vars(by = by, x_1, y_1)$by
    }

    xkeys <- x |>
      fselect(by_x_names)
    names(xkeys) <- paste0(names(xkeys), suffix[1])
    x <- cbind(
      xkeys,
      x
    )
  }


  # left join checks --------------------------------


  # Do left join ------------------------------------
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
    if (any(
      rj[
        ,
        get(names(rj)[length(rj)])
      ] == "y"
    ) |
    any(
      rj[
        ,
        get(names(rj)[length(rj)])
      ] == 2
    )
    ) {

      cli::cli_abort(
        paste0(
          cli::symbol$cross,
          " Error: some rows in `y` are not matched - this check is due to
           argument `unmatched = 'error'` "
        )
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
  if (is.null(by)) {
    by <- intersect(
      names(x),
      names(y)
    )
  }
  if (copy == TRUE) {
    store_msg(
      type = "warn",
      warn = "Warning: that argument `copy = TRUE` is not active in this version of `joyn`"
    )
  }
  if (is.null(suffix) || !length(suffix) == 2 || !is.character(suffix)) {
    cli::cli_abort(
      paste0(
        cli::symbol$cross,
        " Error: argument `suffix` must be character vector of length 2"
      )
    )

  }
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
      type = "warn",
      warn = "Warning: `joyn` does not currently allow inequality joins, so `keep = NULL` will retain only keys in `x`"
    )
    keep <- FALSE
  }

  na_matches <- match.arg(na_matches)
  multiple   <- match.arg(
    multiple,
    choices = c(
      "all",
      "any",
      "first",
      "last"
    )
  )
  if (multiple == "any") {
    multiple <- "first"
  }
  unmatched  <- match.arg(
    unmatched,
    choices = c(
      "drop",
      "error"
    )
  )
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
  na_matches <- match.arg(
    na_matches,
    choices = c(
      "na",
      "never"
    )
  )
  if (na_matches == "never") {
    store_msg(
      type = "warn",
      err  = "Warning: ",
      warn = "Currently, `joyn` allows only `na_matches = 'na'`"
    )
  }
  if (is.null(reportvar) || isFALSE(reportvar)) {
    dropreport <- TRUE
    reportvar <- getOption("joyn.reportvar")
  } else{
    dropreport <- FALSE
  }

  # Column names -----------------------------------
  if (keep == TRUE) {

    x_1 <- copy(x)
    y_1 <- copy(y)

    if (length(grep(pattern = "==?", x = by, value = TRUE)) != 0) {
      by_y_names <- fix_by_vars(by = by, x_1, y_1)$yby
    } else {
      by_y_names <- fix_by_vars(by = by, x_1, y_1)$by
    }

    ykeys <- y |>
      fselect(by_y_names)
    names(ykeys) <- paste0(names(ykeys), suffix[2])
    y <- cbind(
      ykeys,
      y
    )
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
    if (any(
      fj[
        ,
        get(names(fj)[length(fj)])
      ] == "x"
    ) |
    any(
      fj[
        ,
        get(names(fj)[length(fj)])
      ] == 1
    )
    ) {

      cli::cli_abort(
        paste0(
          cli::symbol$cross,
          " Error: some rows in `y` are not matched - this check is due to
           argument `unmatched = 'error'` "
        )
      )

    }

  }
  ### if dropreport = T
  if (dropreport == T) {
    get_vars(fj, reportvar) <- NULL
  }

  # Return
  fj

}





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
  if (is.null(by)) {
    by <- intersect(
      names(x),
      names(y)
    )
  }
  if (copy == TRUE) {
    store_msg(
      type = "warn",
      warn = "Warning: that argument `copy = TRUE` is not active in this version of `joyn`"
    )
  }
  if (is.null(suffix) || !length(suffix) == 2 || !is.character(suffix)) {
    cli::cli_abort(
      paste0(
        cli::symbol$cross,
        " Error: argument `suffix` must be character vector of length 2"
      )
    )

  }
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
      type = "warn",
      warn = "Warning: `joyn` does not currently allow inequality joins, so `keep = NULL` will retain only keys in `x`"
    )
    keep <- FALSE
  }

  na_matches <- match.arg(na_matches)
  multiple   <- match.arg(
    multiple,
    choices = c(
      "all",
      "any",
      "first",
      "last"
    )
  )
  if (multiple == "any") {
    multiple <- "first"
  }
  unmatched  <- match.arg(
    unmatched,
    choices = c(
      "drop",
      "error"
    )
  )
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
  na_matches <- match.arg(
    na_matches,
    choices = c(
      "na",
      "never"
    )
  )
  if (na_matches == "never") {
    store_msg(
      type = "warn",
      err  = "Warning: ",
      warn = "Currently, `joyn` allows only `na_matches = 'na'`"
    )
  }
  if (is.null(reportvar) || isFALSE(reportvar)) {
    dropreport <- TRUE
    reportvar <- getOption("joyn.reportvar")
  } else{
    dropreport <- FALSE
  }

  # Column names -----------------------------------
  if (keep == TRUE) {

    x_1 <- copy(x)
    y_1 <- copy(y)

    if (length(grep(pattern = "==?", x = by, value = TRUE)) != 0) {
      by_y_names <- fix_by_vars(by = by, x_1, y_1)$yby
    } else {
      by_y_names <- fix_by_vars(by = by, x_1, y_1)$by
    }

    ykeys <- y |>
      fselect(by_y_names)
    names(ykeys) <- paste0(names(ykeys), suffix[2])
    y <- cbind(
      ykeys,
      y
    )
  }


  # Do full join ------------------------------------
  fj <- joyn(
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
    if (any(
      fj[
        ,
        get(names(fj)[length(fj)])
      ] == "x"
    ) |
    any(
      fj[
        ,
        get(names(fj)[length(fj)])
      ] == 1
    )
    ) {

      cli::cli_abort(
        paste0(
          cli::symbol$cross,
          " Error: some rows in `y` are not matched - this check is due to
           argument `unmatched = 'error'` "
        )
      )

    }

  }
  ### if dropreport = T
  if (dropreport == T) {
    get_vars(fj, reportvar) <- NULL
  }

  # Return
  fj

}























