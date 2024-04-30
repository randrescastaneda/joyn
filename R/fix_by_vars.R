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

    xby <- trimws(gsub("([^=]+)(\\s*==?\\s*)([^=]+)", "\\1", byexp))
    yby <- trimws(gsub("([^=]+)(\\s*==?\\s*)([^=]+)", "\\3", byexp))
    newkeys <- paste0("keyby", 1:length(xby))

    # x <- frename(x,
    #              newkeys,
    #              cols = which(names(x) %in% xby))
    # y <- frename(y,
    #              newkeys,
    #              cols = which(names(y) %in% yby))
    setnames(x, xby, newkeys)
    setnames(y, yby, newkeys)

    by[grepl("==?", by)] <- newkeys

    return(list(by      = by,
                xby     = xby,
                yby     = yby,
                tempkey = newkeys)
           )

  } else {

    return(list(by      = by,
                xby     = NULL,
                yby     = NULL,
                tempkey = NULL)
           )

  }

}


