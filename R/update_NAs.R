# Add global variables to avoid NSE notes in R CMD check
if (getRversion() >= '2.15.1')
  utils::globalVariables(
    c('use_util_reportvar')
  )

#' Update NA values
#'
#' @param dt  joined data.table
#' @param var variable to be updated
#' @param reportvar variable in `dt` that stores joyn's report
#'
#' @return data.table
#' @noRd

update_NAs <- function(dt, var,
                       reportvar = getOption("joyn.reportvar"),
                       suffix    = getOption("joyn.suffixes")) {

  if (is.null(suffix)) {
    suffix <- getOption("joyn.suffixes")
  }
  x.var <- paste0(var, suffix[1])
  y.var <- paste0(var, suffix[2])


  dt <- dt |>
    ftransform(use_util_reportvar = get(reportvar),
               # create variable for var.x is NA
               # TRUE if not NA
               varx_na            = !missing_cases(mget(x.var)),
               vary_na            = !missing_cases(mget(y.var)))


  # let `use_util_reportvar` reflect updates
  dt$use_util_reportvar[
    !dt$varx_na & !(dt$use_util_reportvar %in% 4)] <- 4L

  # Replace values

  if (inherits(dt, "data.table")) {
    # Update x vars if NA by report is 2 (i.e. row only in y)
    dt[use_util_reportvar == 4 |
      (use_util_reportvar == 2 & varx_na == FALSE),
       eval(x.var) := mget(y.var)]
  } else {
    # Now update the vars if 4
    # Update x vars if NA by report is 2 (i.e. row only in y)
    to_replace <- which(dt$use_util_reportvar == 4 |
                          (dt$use_util_reportvar == 2 & dt$varx_na == FALSE))

    dt[to_replace, x.var] <- dt[to_replace, y.var]
  }


  # remove unnecessary columns
  # vars_to_keep <- names(dt)[names(dt) %!in% c("varx_na", "vary_na")]
  get_vars(dt, c("varx_na", "vary_na", reportvar)) <- NULL

  # adjust reportvar
  setrename(dt, use_util_reportvar = reportvar, .nse = FALSE)

  dt
}


