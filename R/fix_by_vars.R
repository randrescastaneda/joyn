#' fix variable names in by argument
#'
#' @param by argument from merge
#' @param x left table
#' @param y right table
#'
#' @return list
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
