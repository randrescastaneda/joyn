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


  # Now update the vars if 4
  if (!"data.table" %in% class(dt)) {

    dt[
      which(dt$report %in% c(4)),
      var
    ] <- lapply(
      y.var,
      function(y) dt[
        dt$report %in% c(4),
        y
      ]
    )

  } else{
    dt[
      report %in% c(4),
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


  return(dt)

}


