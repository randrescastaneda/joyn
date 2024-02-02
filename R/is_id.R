# Add global variables to avoid NSE notes in R CMD check
if (getRversion() >= '2.15.1')
  utils::globalVariables(
    c('N', '.', 'copies')
  )

#' Check if dt is uniquely identified by `by` variable
#'
#' report if dt is uniquely identified by `by` var or, if report = TRUE, the duplicates in `by` variable
#'
#' @param dt either right of left table
#' @param verbose logical: if TRUE messages will be displayed
#' @param by variable to merge by
#' @param return_report logical: if TRUE, returns data with summary of duplicates.
#' If FALSE, returns logical value depending on whether `dt` is uniquely identified
#' by `by`
#'
#' @return logical or data.frame, depending on the value of argument `return_report`
#' @export
#'
#' @examples
#' library(data.table)
#'
#' # example with data frame not uniquely identified by `by` var
#'
#' y <- data.table(id = c("c","b", "c", "a"),
#'                  y  = c(11L, 15L, 18L, 20L))
#' is_id(y3, by = "id")
#' is_id(y3, by = "id", return_report = TRUE)
#'
#' # example with data frame uniquely identified by `by` var
#'
#' y1 <- data.table(id = c("1","3", "2", "9"),
#'                  y  = c(11L, 15L, 18L, 20L))
#' is_id(y1, by = "id")
#'

is_id <- function(dt,
                  by,
                  verbose = TRUE,
                  return_report  = FALSE) {

  # make sure it is data.table
  if (!(is.data.table(dt))) {
    dt <- as.data.table(dt)
  } else {
    dt <- data.table::copy(dt)
  }

  # count
  m     <- dt[, .(copies =.N), by = mget(by)]
  is_id <- m[, mean(copies)] == 1

  if (verbose) {

    cli::cli_h3("Duplicates in terms of {.code {by}}")

    d <- freq_table(m, "copies")
    print(d[])

    cli::cli_rule(right = "End of {.field is_id()} report")

  }

  if (isFALSE(return_report)) {

    return(is_id)

  } else {

    return(m)

  }

}

