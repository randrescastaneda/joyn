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

  is_data_table <- inherits(dt, "data.table")

  # Add util vars ####
  dt_1 <- copy(dt)
  dt_1 <- dt_1 |>
    ftransform(#use_util_reportvar = get(reportvar),
               # create variable for var.x and var.y is NA
               # TRUE if NOT NA
               varx_na            = !missing_cases(mget(x.var)),
               vary_na            = !missing_cases(mget(y.var)))

  # let reportvar reflect updates ####

  # reportvar = 4 >> update NA in x
  if (rep_NAs) {
    dt_1[[reportvar]][
      !dt_1$varx_na & dt_1$vary_na] <- 4L
  }

  # reportvar = 5 >> update value in x with value from y
  if (rep_values) {
    dt_1[[reportvar]][
      dt_1$varx_na & dt_1$vary_na] <- 5L
    # FALSE => y is NA => not updated
    dt_1[[reportvar]][
      !dt_1$vary_na] <- 6L
  }

  # Replace values ####

    if (is_data_table) {

      dt_1[get(reportvar) == 4,
         (x.var) := mget(y.var)]

      dt_1[get(reportvar) == 5,
         eval(x.var) := mget(y.var)]
    } else {

      to_replace <- which(dt_1[[reportvar]] %in% c(4, 5))
      dt_1[to_replace, x.var] <- dt_1[to_replace, y.var]
    }

  # Remove util vars ####
  get_vars(dt_1, c("varx_na", "vary_na")) <- NULL

  # Return
  dt_1

}
