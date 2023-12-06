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

  # create variable for var.x is NA
  dt$varx_na <- dt |>
    fselect(var) |>
    complete.cases() # TRUE if not NA

  # create variable for var.y is NA
  dt$vary_na <- dt |>
    fselect(y.var) |>
    complete.cases() # TRUE if not NA

  # let `report` reflect updates
  dt[
    !dt$varx_na & !(dt$report %in% c(2, 4)),  # FALSE => is NA and not 2, 4 => NA updated
    c("report")
  ] <- 4L
  dt[
    dt$varx_na & dt$vary_na, # both TRUE => neither NA => updated
    c("report")
  ] <- 5L
  dt[
    !dt$vary_na, # FALSE => is NA => not updated
    c("report")
  ] <- 6L

  # Now update the vars if 4 or 5
  # dt[
  #   dt$report == 4 | dt$report == 5,
  #   mget(var)
  # ] <- dt[
  #   dt$report == 4 | dt$report == 5,
  #   mget(y.var)
  # ]
  if (!"data.table" %in% class(dt)) {

    dt[
      which(dt$report %in% c(4, 5)),
      var
    ] <- lapply(
      y.var,
      function(y) dt[
        dt$report %in% c(4, 5),
        y
      ]
    )

  } else{
    dt[
      report %in% c(4, 5),
      (var) := get(y.var)
    ]
  }


  # remove unnecessary columns
  dt <- dt[
    ,
    mget(
      names(dt)[
        which(
          !names(dt) %in% c("varx_na", "vary_na")
        )
      ]
    )
  ]

  #
  # dt[
  #   is.na(get(var)) & !(report %in% c(2, 4)),
  #   report := 4
  # ][
  #   get(var) != get(y.var) & report != 5,
  #   report := 5
  # ][
  #   is.na(get(y.var)) & report != 6,
  #   report := 6
  # ][
  #   (get(var) != get(y.var)),
  #   (var) := get(y.var)
  # ]
  #
  #

  return(dt)
}



