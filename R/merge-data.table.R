#' Merge two data frames
#'
#' This is a joyn wrapper that works in a similar fashion to [base::merge] and
#' [data.table::merge], which is why [joyn::merge] masks the other two.
#'
#' @inheritParams data.table::merge.data.table
#' @inheritParams joyn
#' @inheritDotParams joyn y_vars_to_keep update_values update_NAs reportvar
#'   reporttype keep_common_vars verbose
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
  dt <- joyn(x               = x,
             y               = y,
             by              = by,
             match_type      = match_type,
             keep            = keep,
             sort            = sort,
             allow.cartesian = allow.cartesian,
             suffixes        = suffixes,
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



check_dt_by <- \(x, y, by, by.x, by.y) {
  nm_x <- names(x)
  nm_y <- names(y)
  ## set up 'by'/'by.x'/'by.y'
  if ((!is.null(by.x) ||
       !is.null(by.y)) &&
      length(by.x) != length(by.y)) {
    cli::cli_abort("`by.x` and `by.y` must be of same length.")
  }
  if (!missing(by) && !missing(by.x)) {
    store_msg("warn",
              warn = cli::symbol$warning,
              " Supplied both `by` and `by.x/by.y`.
              `by` argument will be ignored.")
  }
  if (!is.null(by.x)) {

    if (length(by.x) == 0L ||
        !is.character(by.x) ||
        !is.character(by.y)) {

      cli::cli_abort("A non-empty vector of column names is required
                     for `by.x` and `by.y`.")
    }

    if (!all(by.x %chin% nm_x)) {
      cli::cli_abort("Elements listed in `by.x` must be valid column names in x.")
    }
    if (!all(by.y %chin% nm_y)) {
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
    if (!all(by %chin% intersect(nm_x, nm_y))) {
      cli::cli_abort("Elements listed in `by` must be valid column
                     names in x and y")
    }
    by = unname(by)
    by.x = by.y = by
  }

  return(by)
}
