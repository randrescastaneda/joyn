#' Update NA and/or values
#'
#' @param dt  joined data.table
#' @param var variable to be updated
#' @param reportvar variable in `dt` that stores joyn's report
#' @param update_NA
#' @param update_values
#'
#' @return data.table
#' @noRd

# note (RT): fix documentation

update_na_values <- function(dt,
                          var,
                          reportvar = getOption("joyn.reportvar"),
                          suffix    = getOption("joyn.suffixes"),
                          rep_NAs = FALSE,
                          rep_values = FALSE) {

  if (is.null(suffix)) {
    suffix <- getOption("joyn.suffixes")
  }
  x.var <- paste0(var, suffix[1])
  y.var <- paste0(var, suffix[2])

  # Add util vars ####
  dt <- dt |>
    ftransform(use_util_reportvar = get(reportvar),
               # create variable for var.x and var.y is NA
               # TRUE if NOT NA
               varx_na            = !missing_cases(mget(x.var)),
               vary_na            = !missing_cases(mget(y.var)))

  # let `use_util_reportvar` reflect updates ####

  # reportvar = 4 >> update NA in x
  dt$use_util_reportvar[
    !dt$varx_na & !(dt$use_util_reportvar %in% 4)] <- 4L

  # reportvar = 5 >> update value in x with value from y
  dt$use_util_reportvar[
    dt$varx_na & dt$vary_na] <- 5L

  # reportvar = 6 >> do not update value/NA in x
  dt$use_util_reportvar[
    !dt$vary_na] <- 6L

  # Replace values ####

  # if update_NA = TRUE and update_values = FALSE
  if (rep_NAs == TRUE & rep_values == FALSE) {

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

  }

  # If update_NAs = FALSE and update_values = TRUE
  else if (rep_NAs == FALSE & rep_values == TRUE) {

    if (inherits(dt, "data.table")) {
      dt[use_util_reportvar %in% c(3, 5),
         eval(x.var) := mget(y.var)]
    } else {
      to_replace <- dt$use_util_reportvar %in% c(3, 5)
      dt[to_replace, x.var] <- dt[to_replace, y.var]
    }

  }

  else if (rep_NAs == TRUE & rep_values == TRUE) {

    if (inherits(dt, "data.table")) {
      dt[use_util_reportvar %in% c(2, 4, 5),
         eval(x.var) := mget(y.var)]
    } else {
      to_replace <- dt$use_util_reportvar %in% c(2, 4, 5)
      dt[to_replace, x.var] <- dt[to_replace, y.var]
    }

  }

  else {dt <- dt}

  # Remove util vars ####
  # remove unnecessary columns
  # vars_to_keep <- names(dt)[names(dt) %!in% c("varx_na", "vary_na")]
  get_vars(dt, c("varx_na", "vary_na", reportvar)) <- NULL

  # adjust reportvar
  setrename(dt, use_util_reportvar = reportvar, .nse = FALSE)

  # Return
  dt

}
