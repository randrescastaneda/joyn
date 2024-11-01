# Add global variables to avoid NSE notes in R CMD check
if (getRversion() >= '2.15.1')
  utils::globalVariables(
    c('n', '.', 'percent')
  )

#' Tabulate simple frequencies
#'
#' tabulate one variable frequencies
#'
#' @param x  data frame
#' @param byvar character: name of variable to tabulate. Use Standard evaluation.
#' @param digits numeric: number of decimal places to display. Default is 1.
#' @param na.rm logical: report NA values in frequencies. Default is FALSE.
#' @param freq_var_name character: name for frequency variable. Default is "n"
#'
#' @return data.table with frequencies.
#' @export
#'
#' @examples
#' library(data.table)
#' x4 = data.table(id1 = c(1, 1, 2, 3, 3),
#'                 id2 = c(1, 1, 2, 3, 4),
#'                 t   = c(1L, 2L, 1L, 2L, NA_integer_),
#'                 x   = c(16, 12, NA, NA, 15))
#' freq_table(x4, "id1")
freq_table <- function(x,
                       byvar,
                       digits = 1,
                       na.rm  = FALSE,
                       freq_var_name = "n") {

  x_name <- as.character(substitute(x))
  if (!is.data.frame(x)) {
    cli::cli_abort("Argument {.arg x} ({.field {x_name}}) must be a data frame")
  }
  if (isFALSE(is.data.table(x))) {
    x <- qDT(x)
  }


  fq <- qtab(x[, ..byvar], na.exclude = na.rm, dnn = byvar)

  ft <- fq |>
    as.data.table() |>
    setnames("N", "n") |>
    # filter zeros
    fsubset(n > 0)

  N <- fsum(ft$n)
  ft <- ft |>
    ftransform(percent = paste0(round(n / N * 100, digits), "%"))

  # add row with totals
  total_row <- rep("total", length(byvar)) |>
    as.list() |>
    as.data.table() |>
    setnames(new  = byvar) |>
    ftransform(n = N,
               percent = "100%")

  ft <- rowbind(ft, total_row)
  setrename(ft,
            n    = freq_var_name,
            .nse = FALSE)
}


#' Report frequencies from attributes in report var
#'
#' @param x dataframe from [joyn_workhorse]
#' @param y dataframe from original merge ("right" or "using")
#'
#' @return dataframe with frequencies of report var
#' @keywords internal
report_from_attr <- function(x,y, reportvar) {
  # from suggestion by @SebKrantz in #58
  # https://github.com/randrescastaneda/joyn/issues/58
  m <- attr(x, "join.match")$match

  N <- fnrow(x)
  nm_x <- attr(m, "N.nomatch") # Number of non-matched x values
  nm_y <- fnrow(y) - attr(m, "N.distinct") # Number of non-matched y values. If multiple = FALSE attr(m, "N.distinct") = number of unique matches.


  counts <- c(nm_x,  nm_y, N-nm_x-nm_y, N)
  report <- data.frame(
    .joyn1 = c("x", "y", "x & y", "total"),
    n = counts,
    percent = paste0(round(counts / N * 100, 1), "%")
  ) |>
    fsubset(n > 0)

  setrename(report, .joyn1 = reportvar, .nse = FALSE)

}
