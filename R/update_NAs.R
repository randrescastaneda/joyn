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

update_NAs <- function(dt, var,
                       reportvar,
                       suffix = NULL) {

  if (is.null(suffix)) {
    suffix <- c("", ".y")
  }
  x.var <- paste0(var, suffix[1])
  y.var <- paste0(var, suffix[2])

  dt <- dt |>
    ftransform(use_util_reportvar = get(reportvar),
               # create variable for var.x is NA
               # TRUE if not NA
               varx_na            = !missing_cases(get(x.var)),
               vary_na            = !missing_cases(get(y.var)))


  # let `use_util_reportvar` reflect updates
  dt$use_util_reportvar[
    !dt$varx_na & !(dt$use_util_reportvar %in% 4)] <- 4L

  # Replace values
  # Now update the vars if 4
  to_replace <- which(dt$use_util_reportvar == 4 |
                        # Update x vars if NA by report is 2 (i.e. row only in y)
                        (dt$use_util_reportvar == 2 & dt$varx_na == FALSE))

  dt[[x.var]][to_replace] <- dt[[y.var]][to_replace]

  # remove unnecessary columns
  vars_to_keep <- names(dt)[names(dt) %!in% c("varx_na", "vary_na")]
  dt <- get_vars(dt, vars_to_keep)


  # adjust reportvar
  get_vars(dt, reportvar) <- NULL
  setrename(dt, use_util_reportvar = reportvar, .nse = FALSE)

  dt
}


