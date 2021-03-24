#' Title
#'
#' @param dt
#' @param var
#'
#' @return
#' @export
#'
#' @examples
update_NAs <- function(dt, var) {

  y.var <- paste0("y.", var)

  dt[
    is.na(get(var)) & !(report %in% c(2, 4)),
    report := 4
  ][
    is.na(get(var)),
    (var) := get(y.var)
  ]

  return(dt)
}


