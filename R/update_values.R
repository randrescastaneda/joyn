# Add global variables to avoid NSE notes in R CMD check
if (getRversion() >= '2.15.1')
  utils::globalVariables(
    c('use_util_reportvar')
  )

#' update values in x with data from y
#'
#' @param dt joined table
#' @param var variable to be updated
#' @param reportvar variables in `dt` that has report
#'
#' @return data.table
#' @noRd
update_values <- function(dt, var,
                          reportvar = ".joyn", suffix = NULL) {

  if (is.null(suffix)) {
    suffix <- c("", ".y")
  }
  x.var <- paste0(var, suffix[1])
  y.var <- paste0(var, suffix[2])

  dt$use_util_reportvar <- dt |>
    fselect(get(reportvar))

  # create variable for var.x is NA
  dt$varx_na <- dt |>
    fselect(x.var) |>
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

  if (!"data.table" %in% class(dt)) {

    dt[
      which(dt$use_util_reportvar %in% c(4, 5)),
      x.var
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
      (x.var) := get(y.var)
    ]
  }


  # remove unnecessary columns
  vars_to_keep <- names(dt)[names(dt) %!in% c("varx_na", "vary_na")]
  dt <- get_vars(dt, vars_to_keep)


  # adjust reportvar
  get_vars(dt, reportvar) <- NULL
  setrename(dt, use_util_reportvar = reportvar, .nse = FALSE)

  return(dt)
}



