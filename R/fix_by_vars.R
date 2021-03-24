#' Title
#'
#' @param by
#' @param x
#' @param y
#'
#' @return
#' @export
#'
#' @examples
`fix_by_vars <- function(by, x, y) {

  byexp <- grep("==?", by, value = TRUE)

  if (length(byexp) != 0) {

    xby <- trimws(gsub("([^=]+)(\\s*==?\\s*)([^=]+)", "\\1", byexp))
    yby <- trimws(gsub("([^=]+)(\\s*==?\\s*)([^=]+)", "\\3", byexp))
    newkeys <- paste0("keyby", 1:length(xby))

    setnames(x, xby, newkeys)
    setnames(y, yby, newkeys)

    return(TRUE)

  } else {
    return(FALSE)
  }

}


