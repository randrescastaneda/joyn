# Add global variables to avoid NSE notes in R CMD check
if (getRversion() >= '2.15.1')
  utils::globalVariables(
    c('use_util_reportvar')
  )

#' Update NA values
#'
#' @param dt  data.table
#' @param var variable to be updated
#'
#' @return data.table
#' @noRd
update_NAs <- function(dt, var, reportvar = ".joyn", suffix = NULL) {

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

  # let report reflect updates
  dt[
    !dt$varx_na & !(dt$use_util_reportvar %in% c(4)),  # FALSE => is NA and not 2, 4 => NA updated
    c("use_util_reportvar")
  ] <- 4L


  # Now update the vars if 4
  if (!"data.table" %in% class(dt)) {

    dt[
      which(dt$use_util_reportvar %in% c(4)),
      x.var
    ] <- lapply(
      y.var,
      function(y) dt[
        dt$use_util_reportvar %in% c(4),
        y
      ]
    )
    # Update x vars if NA by report is 2 (i.e. row only in y)
    dt[
      which(dt$use_util_reportvar %in% c(2) & dt$varx_na == FALSE),
      x.var
    ]  <- lapply(
      y.var,
      function(y) dt[
        which(dt$use_util_reportvar %in% c(2) & dt$varx_na == FALSE),
        y
      ]
    )

  } else{
    dt[
      use_util_reportvar %in% c(4),
      (x.var) := get(y.var)
    ]
  }

  # Update x vars if NA by report is 2 (i.e. row only in y)


  # remove unnecessary columns

  vars_to_keep <- names(dt)[names(dt) %!in% c("varx_na", "vary_na")]
  dt <- get_vars(dt, vars_to_keep)


  # adjust reportvar
  get_vars(dt, reportvar) <- NULL
  setrename(dt, use_util_reportvar = reportvar, .nse = FALSE)

  dt
}


