#' tabulate simple frequencies
#'
#' tabulate one variable in data frame to substitute base::table and
#' janitor::tabyl using data.table
#'
#' @param x  data frame
#' @param byvar character: name of variable to tabulate. Use STandard evaluation.
#' @param digits numeric: number of decimal places to display. Default is 1.
#'
#' @return
#' @export
#'
#' @examples
#' freq_table(x4, "id1")
freq_table <- function(x,
                       byvar,
                       digits = 1) {

  # Frequencies and format
  d <- x[, .(n = .N), by = byvar
  ][, percent :=
      {
        total = sum(n)
        d <- round((n/ total)*100, digits = digits)
        d <- as.character(d)
        d <- paste0(d, "%")
      }
  ]

  # Total row just for completeness
  setorderv(d, byvar)
  totd <- data.table::data.table(
    tempname = "total",
    n        = d[, sum(n)],
    percent  = "100%"
  )

  setnames(totd, "tempname", byvar)
  d <- data.table::rbindlist(list(d, totd),
                             use.names = TRUE)
  return(d)
}
