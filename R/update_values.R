# Add global variables to avoid NSE notes in R CMD check
if (getRversion() >= '2.15.1')
  utils::globalVariables(
    c('use_util_reportvar')
  )

#' update values in x with data from y
#'
#' @param dt joined table
#' @param var variable to be updated
#' @param reportvar variable in `dt` that stores joyn's report
#'
#' @return data.table
#' @noRd
update_values <- function(dt, var,
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
  # FALSE => is NA and not 2, 4 => NA updated
  dt$use_util_reportvar[
    !dt$varx_na & (dt$use_util_reportvar %!in% c(2, 4))] <- 4L
  # both TRUE => neither NA => updated
  dt$use_util_reportvar[
    dt$varx_na & dt$vary_na] <- 5L
  # FALSE => is NA => not updated
  dt$use_util_reportvar[
    !dt$vary_na] <- 6L

  # Replace values
  to_replace <- which(dt$use_util_reportvar %in% c(4, 5))

  dt[[x.var]][to_replace] <- dt[[y.var]][to_replace]


  # remove unnecessary columns
  vars_to_keep <- names(dt)[names(dt) %!in% c("varx_na", "vary_na")]
  dt <- get_vars(dt, vars_to_keep)


  # adjust reportvar
  get_vars(dt, reportvar) <- NULL
  setrename(dt, use_util_reportvar = reportvar, .nse = FALSE)

  return(dt)
}



