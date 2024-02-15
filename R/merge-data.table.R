#' Merge two data frames
#'
#' This is a joyn wrapper that works in a similar fashion to [base::merge] and
#' [data.table::merge], which is why [joyn::merge] masks the other two.
#'
#' @inheritParams data.table::merge.data.table
#' @inheritParams joyn
#' @inheritDotParams joyn y_vars_to_keep update_values update_NAs reportvar
#'   reporttype keep_common_vars verbose
#' @return data.table merging x and y
#' @examples
#' x1 = data.frame(id = c(1L, 1L, 2L, 3L, NA_integer_),
#'                 t  = c(1L, 2L, 1L, 2L, NA_integer_),
#'                 x  = 11:15)
#' y1 = data.frame(id = c(1,2, 4),
#'                 y  = c(11L, 15L, 16))
#' joyn:::merge(x1, y1, by = "id")
#' # example of using by.x and by.y
#' x2 = data.frame(id1 = c(1, 1, 2, 3, 3),
#'                 id2 = c(1, 1, 2, 3, 4),
#'                 t   = c(1L, 2L, 1L, 2L, NA_integer_),
#'                 x   = c(16, 12, NA, NA, 15))
#' y2 = data.frame(id  = c(1, 2, 5, 6, 3),
#'                 id2 = c(1, 1, 2, 3, 4),
#'                 y   = c(11L, 15L, 20L, 13L, 10L),
#'                 x   = c(16:20))
#' jn <- joyn:::merge(x2,
#'             y2,
#'             match_type = "m:m",
#'             all.x = TRUE,
#'             by.x = "id1",
#'             by.y = "id2")
#' # example with all = TRUE
#' jn <- joyn:::merge(x2,
#'             y2,
#'             match_type = "m:m",
#'             by.x = "id1",
#'             by.y = "id2",
#'             all = TRUE)
merge <- function(x,
                  y,
                  by = NULL,
                  by.x = NULL,
                  by.y = NULL,
                  all = FALSE,
                  all.x = all,
                  all.y = all,
                  sort = TRUE,
                  suffixes = c(".x", ".y"),
                  no.dups = TRUE,
                  # default FALSE
                  allow.cartesian = getOption("datatable.allow.cartesian"),
                  match_type= c("m:m", "m:1", "1:m", "1:1"),
                  keep_common_vars = TRUE,
                  ...) {

  # clear joun env
  clear_joynenv()

  # Check arguments ------------
  # this comes directly from data.table::merge.data.table()
  match_type  <- match.arg(match_type)
  check_logical(sort, "sort")
  check_logical(no.dups, "no.dups")

  ## by vars -----------
  by <- check_dt_by(x, y, by, by.x, by.y)

  # wrap to joyn ------------

  if (isTRUE(all.x) && isTRUE(all.y)) {
    keep <- "full"
  } else if (isFALSE(all.x) && isFALSE(all.y)) {
    keep <- "inner"
  } else if (isTRUE(all.x) && isFALSE(all.y)) {
    keep <- "left"
  } else if (isFALSE(all.x) && isTRUE(all.y)) {
    keep <- "right"
  }

  # NOTE: we should think of anti-joins...

  # implement joyn --------
  dt <- joyn(x                = x,
             y                = y,
             by               = by,
             match_type       = match_type,
             keep             = keep,
             sort             = sort,
             allow.cartesian  = allow.cartesian,
             suffixes         = suffixes,
             keep_common_vars = keep_common_vars,
             ...)

  # wrangling (add filters) -------

  # return -------
  dt
}


check_logical <- \(x, name) {
  if (!x %in% c(TRUE, FALSE))
    cli::cli_abort("Argument {.arg {name}} should be logical,
                   either {.or {.code {c('TRUE', 'FALSE')}}}")
}


#' Check dt `by` vars
#'
#' check variable(s) by which data frames are joined: either a single `by` var, common to right and left dt,
#' or
#'
#' @param x left table
#' @param y right table
#' @param by character: variable to join by (common variable to x and y)
#' @param by.x character: specified var in x to join by
#' @param by.y character: specified var in y to join by
#'
#' @return character specifying checked variable(s) to join by
#'
#' @examples
#' \dontrun{
#' x = data.table(id1 = c(1, 1, 2, 3, 3),
#'                id2 = c(1, 1, 2, 3, 4),
#'                t   = c(1L, 2L, 1L, 2L, NA_integer_),
#'                x   = c(16, 12, NA, NA, 15))
#'y = data.table(id  = c(1, 2, 5, 6, 3),
#'                id2 = c(1, 1, 2, 3, 4),
#'                y   = c(11L, 15L, 20L, 13L, 10L),
#'                x   = c(16:20))
#' # example specifying by.x and by.y
#' joyn:::check_dt_by(x, y, by.x = "id1", by.y = "id2")
#' }

check_dt_by <- \(x, y, by, by.x, by.y) {
  nm_x <- names(x)
  nm_y <- names(y)

  colnames(x)[colnames(x) == by.x] <- by.x
  colnames(y)[colnames(y) == by.y] <- by.y

  ## set up 'by'/'by.x'/'by.y'
  if ((!is.null(x$by.x) ||
       !is.null(y$by.y)) &&
      length(x$by.x) != length(y$by.y)) {
    cli::cli_abort("`by.x` and `by.y` must be of same length.")
  }
  if (!missing(by) && !missing(by.x)) {
    store_msg("warn",
              warn = paste(cli::symbol$warning, "  Warning:"),
              pale = " Supplied both",
              bolded_pale = "  by and by.x/by.y. by",
              pale = "argument will be ignored.")
  }
  if (!is.null(by.x)) {

    if (length(by.x) == 0L ||
        !is.character(by.x) ||
        !is.character(by.y)) {

      cli::cli_abort("A non-empty vector of column names is required
                     for `by.x` and `by.y`.")
    }

    if (!all(by.x %in% nm_x)) {
      cli::cli_abort("Elements listed in `by.x` must be valid column names in x.")
    }
    if (!all(by.y %in% nm_y)) {
      cli::cli_abort("Elements listed in `by.y` must be valid column names in y.")
    }


    # Original data.table code is this:
    # by = cby.x
    # names(by) = by.y
    #
    # It is replaced by this:
    by = paste(by.x, "=", by.y)

  } else {
    if (is.null(by)) {
      by = intersect(key(x), key(y))
    }

    if (!length(by)) {
      by = intersect(nm_x, nm_y)
    }
    if (length(by) == 0L || !is.character(by)) {
      cli::cli_abort("A non-empty vector of column names for `by` is required.")
    }
    if (!all(by %in% intersect(nm_x, nm_y))) {
      cli::cli_abort("Elements listed in `by` must be valid column
                     names in x and y")
    }
    by = unname(by)
    by.x = by.y = by
  }

  return(by)
}
