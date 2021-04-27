# Add global variables to avoid NSE notes in R CMD check
if (getRversion() >= '2.15.1')
  utils::globalVariables(
    c('n', '.', 'percent')
  )

#' tabulate simple frequencies
#'
#' tabulate one variable in data frame to substitute base::table and
#' janitor::tabyl using data.table
#'
#' @param x  data frame
#' @param byvar character: name of variable to tabulate. Use STandard evaluation.
#' @param digits numeric: number of decimal places to display. Default is 1.
#' @param na.rm logical: if TRUE remove NAs from calculations. Default is TRUE
#'
#' @return data.table with frequencies.
#' @export
#'
#' @examples
#' freq_table(x4, "id1")
freq_table <- function(x,
                       byvar,
                       digits = 1,
                       na.rm  = TRUE) {

  if (!(is.data.table(x))) {
    x <- as.data.table(x)
  } else {
    x <- data.table::copy(x)
  }


  # Frequencies and format
  d <- x[, .(n = .N), by = byvar
  ][, percent :=
      {
        total = sum(n, na.rm = na.rm)
        d <- round((n/ total)*100, digits = digits)
        d <- as.character(d)
        d <- paste0(d, "%")
      }
  ]

  # Total row just for completeness
  setorderv(d, byvar)
  totd <- data.table::data.table(
    tempname = "total",
    n        = d[, sum(n, na.rm = na.rm)],
    percent  = "100%"
  )

  setnames(totd, "tempname", byvar)
  d <- data.table::rbindlist(list(d, totd),
                             use.names = TRUE)
  return(d)
}
