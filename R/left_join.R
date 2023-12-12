#' Left join two data frames
#'
#' This is a `joyn` wrapper that works in a similar
#' fashion to [dplyr::left_join]
#'
#' @inheritParams dplyr::left_join
#' @inheritParams joyn
#' @param yvars_to_keep character: vector giving the variables in `y`
#' the include in the output table
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
    yvars_to_keep    = TRUE,
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
    y_vars_to_keep   = TRUE,
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
    lj$`.joyn` <- NULL
  }

  # Return
  return(lj)



}






























#'
#' cbind(
#'   y1,
#'   y1 |>
#'     fselect(
#'       c("y")
#'     ) |>
#'     frename(
#'       paste0, ".x"
#'     )
#' )
#'
#'
#' byexp <- grep("==?", by, value = TRUE)
#'
#' if (length(byexp) != 0) {
#'
#'   xby <- trimws(gsub("([^=]+)(\\s*==?\\s*)([^=]+)", "\\1", byexp))
#'   yby <- trimws(gsub("([^=]+)(\\s*==?\\s*)([^=]+)", "\\3", byexp))
#'   newkeys <- paste0("keyby", 1:length(xby))
#'
#'   setnames(x, xby, newkeys)
#'   setnames(y, yby, newkeys)
#'
#'   by[grepl("==?", by)] <- newkeys
#'
#'   return(list(by      = by,
#'               xby     = xby,
#'               yby     = yby,
#'               tempkey = newkeys)
#'   )
#'
#' } else {
#'
#'   return(list(by      = by,
#'               xby     = NULL,
#'               yby     = NULL,
#'               tempkey = NULL)
#'   )
#'
#' }
#'
#'
#'
#' if (length(grep("==?", by, value = TRUE))>0) {
#'   y_name <-
#' }
#'
#' y <- y |>
#'   ftransform(
#'     id.y = id
#'   )
#'
#'
#'
#'
#'
#' #' Internal function for dealing with the `dplyr` joins when `keep = T`
#' #'
#' #' When tidyverse joins are used, and the tidyverse argument `keep = T`
#' #' Note that the tidyverse `keep` is different to the `joyn` keep.
#' #'
#' #' @param dt data frame: object to add columns with suffix
#' #' @param by character: giving column names, `by` argument from join
#' #' @param suffix character: suffix to add to additional columns
#' #' @param keep character: "left" or "right" to use for matching suffix
#' #'
#' #' @return data frame: same as `dt` but with additional
#' #'         columns, which are duplicates of the `by` columns
#' #'         except that the names have an added suffix
#' #'
#' #' @examples
#' check_dplyr_keep <- function(
#'     dt,
#'     by,
#'     suffix,
#'     keep = c("left", "right")
#' ){
#'
#'   if (
#'     length(
#'       grep(
#'         "==?", by, value = TRUE
#'       )
#'     ) == 0
#'   ){
#'
#'     foo_dt <- dt |>
#'       fselect(
#'         by,
#'         c(names(dt)[!names(dt) %in% by]),
#'         by
#'       )
#'     names(foo_dt)[1:length(by)] <- paste0(by, suffix)
#'
#'   } else{
#'
#'     if(keep == "left"){
#'       dt1 <- copy(dt)
#'       dt2 <- copy(dt)
#'       ynames <- fix_by_vars(by = by, dt, dt)$yby
#'       foo_dt <- dt |>
#'         fselect(
#'           rep(ynames, 2)
#'         )
#'       names(foo_dt)[1:length(dt)] <- paste0(names(dt), suffix)
#'
#'     } else {
#'       xnames <- fix_by_vars(by = by, dt, dt)$xby
#'       foo_dt <- dt |>
#'         fselect(
#'           rep(xnames, 2)
#'         )
#'       names(foo_dt)[1:length(dt)] <- paste0(names(dt), suffix)
#'
#'     }
#'
#'
#'   }
#'
#'   return(foo_dt)
#'
#' }
#'
#' check_dplyr_keep(
#'   dt = y1,
#'   by = c("id = id"),
#'   suffix = ".y",
#'   keep = "left"
#' )
#'
#'
#' fooy <- y1 |>
#'   fselect(
#'     rep(c("id", "y"),2)
#'   )
#' names(fooy)[1:length(y1)] <- paste0(names(y1), ".y")
#' fooy
#'
#'
