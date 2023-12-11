# Add global variables to avoid NSE notes in R CMD check
if (getRversion() >= '2.15.1')
  utils::globalVariables(
    c('use_util_reportvar')
  )

#' update values in x with data from y
#'
#' @param dt joined table
#' @param var variable to be updated
#'
#' @return data.table
#' @noRd
update_values <- function(dt, var, reportvar = ".joyn") {

  y.var <- paste0(var, ".y")

  dt$use_util_reportvar <- dt |>
    fselect(get(reportvar))

  # create variable for var.x is NA
  dt$varx_na <- dt |>
    fselect(var) |>
    {\(.) !missing_cases(.)}() # TRUE if not NA

  # create variable for var.y is NA
  dt$vary_na <- dt |>
    fselect(y.var) |>
    {\(.) !missing_cases(.)}() # TRUE if not NA

  # let `use_util_reportvar` reflect updates
  dt[
    !dt$varx_na & !(dt$use_util_reportvar %in% c(2, 4)),  # FALSE => is NA and not 2, 4 => NA updated
    c("use_util_reportvar")
  ] <- 4L
  dt[
    dt$varx_na & dt$vary_na, # both TRUE => neither NA => updated
    c("use_util_reportvar")
  ] <- 5L
  dt[
    !dt$vary_na, # FALSE => is NA => not updated
    c("use_util_reportvar")
  ] <- 6L

  # Now update the vars if 4 or 5
  # dt[
  #   dt$use_util_reportvar == 4 | dt$use_util_reportvar == 5,
  #   mget(var)
  # ] <- dt[
  #   dt$use_util_reportvar == 4 | dt$use_util_reportvar == 5,
  #   mget(y.var)
  # ]
  if (!"data.table" %in% class(dt)) {

    dt[
      which(dt$use_util_reportvar %in% c(4, 5)),
      var
    ] <- lapply(
      y.var,
      function(y) dt[
        dt$use_util_reportvar %in% c(4, 5),
        y
      ]
    )

  } else{
    dt[
      use_util_reportvar %in% c(4, 5),
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

  # adjust reportvar
  dt |> fselect(reportvar) <- NULL
  names(dt)[
    which(
      names(dt) == "use_util_reportvar"
    )
  ] <- reportvar

  return(dt)
}



