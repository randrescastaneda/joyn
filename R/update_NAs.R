# Add global variables to avoid NSE notes in R CMD check
if (getRversion() >= '2.15.1')
  utils::globalVariables(
    c('report')
  )

#' Update NA values
#'
#' @param dt  data.table
#' @param var variable to be updated
#'
#' @return data.table
#' @noRd
update_NAs <- function(dt, var) {

  y.var <- paste0(var, ".y")

  dt[
    is.na(get(var)) & !(report %in% c(2, 4)),
    report := 4
  ][
    is.na(get(var)),
    (var) := get(y.var)
  ]

  return(dt)
}


