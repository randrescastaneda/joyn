# Add global variables to avoid NSE notes in R CMD check
if (getRversion() >= '2.15.1')
  utils::globalVariables(
    c('N')
  )

#' Make sure the match type is correct
#'
#' @param dt either right of left table
#' @param verbose logical: if TRUE messages will be displayed
#' @param by by argument in merge
#' @param return_report logical: if TRUE, returns data with summary of duplicates.
#' If FALSE, returns logical value depending on whether `dt` is uniquely identified
#' by `by`
#'
#' @return logical of data.frame depending on the value of argument `report`
#' @export
#'
#' @examples
#' is_id(y3, by = "id")
#' is_id(y3, by = "id", return_report = TRUE)
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

    if (requireNamespace("janitor", quietly = TRUE)) {

      disp <- janitor::tabyl(m, copies)
      disp <- janitor::adorn_totals(disp, "row")
      disp <- janitor::adorn_pct_formatting(disp, digits = 1)

      print(disp)

    } else {
      d <- m[, .(n = .N), by = copies
      ][, percent :=
          {
            total = sum(n)
            d <- round((n/ total)*100, digits = 1)
            d <- as.character(d)
            d <- paste0(d, "%")
          }
      ]
      setorder(d, copies)
      print(d[])

    }
    cli::cli_rule(right = "End of {.field is_id()} report")

  }

  if (isFALSE(return_report)) {

    return(invisible(is_id))

  } else {

    return(m)

  }

}

