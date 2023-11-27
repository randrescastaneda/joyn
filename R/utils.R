#' rename to syntactically valid names
#'
#' @param name character: name to be coerced to syntactically valid name
#' @inheritParams merge
#'
#' @return valide character name
#' @export
#'
#' @examples
#' rename_to_valid("not valid")
rename_to_valid <- function(name, verbose = getOption("joyn.verbose")) {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # computations   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  if (!is.character(name)) {
    cli::cli_abort("name {.var name} should be character")
  }
    nreportnames <- make.names(name)
    if (!identical(name,nreportnames) & isTRUE(verbose)) {

      cli::cli_alert_info("name {.field {name}} is an invalid variable name.
                          It will be changed to {.field {nreportnames}}",
                          wrap = TRUE)
    }
    return(nreportnames)
}



#' fix variable names in by argument
#'
#' @param by argument from merge
#' @param x left table
#' @param y right table
#'
#' @return list
#' @keywords internal
fix_by_vars <- function(by, x, y) {

  byexp <- grep("==?", by, value = TRUE)

  if (length(byexp) != 0) {

    xby <- trimws(gsub("([^=]+)(\\s*==?\\s*)([^=]+)", "\\1", byexp))
    yby <- trimws(gsub("([^=]+)(\\s*==?\\s*)([^=]+)", "\\3", byexp))
    newkeys <- paste0("keyby", 1:length(xby))

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

