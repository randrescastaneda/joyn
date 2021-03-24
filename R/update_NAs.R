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
    get(var) != get(y.var) & report != 5,
    report := 5
  ][
    is.na(get(y.var)) & report != 6,
    report := 6
  ][
    get(var) != get(y.var),
    (var) := get(y.var)
  ]

  return(dt)
}


