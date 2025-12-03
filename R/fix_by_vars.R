#' Fix variable names in `by` argument for merging
#'
#' Processes the `by` argument from a merge operation, handling expressions like "a == b".
#' Renames columns in `x` and `y` if necessary to create temporary keys for merging.
#'
#' @param by Character vector; the `by` argument from merge, possibly containing expressions like "a == b".
#' @param x data.table; the left table to be merged.
#' @param y data.table; the right table to be merged.
#'
#' @return A list with elements:
#'   \item{by}{Character vector of column names used for merging.}
#'   \item{xby}{Original column names from `x` used for merging.}
#'   \item{yby}{Original column names from `y` used for merging.}
#'   \item{tempkey}{Temporary key names created for merging, or NULL if not needed.}
#' @noRd
fix_by_vars <- function(by, x, y) {

  byexp <- grep("==?", by, value = TRUE)

  if (length(byexp) != 0) {

    # extract left and right variable names from expressions like "a == b"
    xby <- trimws(gsub("([^=]+)(\\s*==?\\s*)([^=]+)", "\\1", byexp))
    yby <- trimws(gsub("([^=]+)(\\s*==?\\s*)([^=]+)", "\\3", byexp))

    # If xby and yby are identical, no need for temporary renaming
    if (all(xby == yby)) {

      return(list(
        by      = xby,
        xby     = xby,
        yby     = yby,
        tempkey = NULL
      ))

    } else {
      # otherwise, create temporary key names
      newkeys <- paste0("keyby", seq_along(xby))

      setnames(x, xby, newkeys)
      setnames(y, yby, newkeys)

      by[grepl("==?", by)] <- newkeys

      return(list(
        by      = by,
        xby     = xby,
        yby     = yby,
        tempkey = newkeys
      ))
    }

  } else {

    return(list(
      by      = by,
      xby     = NULL,
      yby     = NULL,
      tempkey = NULL
    ))

  }
}
