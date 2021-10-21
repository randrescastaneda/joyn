# Add global variables to avoid NSE notes in R CMD check
if (getRversion() >= '2.15.1')
  utils::globalVariables(
    c('report')
  )

#' update values in x with data from y
#'
#' @param dt joined table
#' @param var variable to be updated
#'
#' @return data.table
#' @noRd
update_values <- function(dt, var) {

  y.var <- paste0(var, ".y")

  dt[
    is.na(get(var)) & !(report %in% c(2, 4)),
    report := 4
  ][
    get(var) != get(y.var) & report != 5,
    report := 5
  ][
    is.na(get(y.var)) & report != 6,
    report := 6
  ][
    (get(var) != get(y.var)),
    (var) := get(y.var)
  ]

  return(dt)
}

