# Add global variables to avoid NSE notes in R CMD check
if (getRversion() >= '2.15.1')
  utils::globalVariables(
    c('N', '.', 'copies')
  )

#' Check if dt is uniquely identified by `by` variable
#'
#' report if dt is uniquely identified by `by` var or, if report = TRUE, the duplicates in `by` variable
#'
#' @param dt either right of left table
#' @param verbose logical: if TRUE messages will be displayed
#' @param by variable to merge by
#' @param return_report logical: if TRUE, returns data with summary of duplicates.
#' If FALSE, returns logical value depending on whether `dt` is uniquely identified
#' by `by`
#'
#' @return logical or data.frame, depending on the value of argument `return_report`
#' @export
#'
#' @examples
#' library(data.table)
#'
#' # example with data frame not uniquely identified by `by` var
#'
#' y <- data.table(id = c("c","b", "c", "a"),
#'                  y  = c(11L, 15L, 18L, 20L))
#' is_id(y, by = "id")
#' is_id(y, by = "id", return_report = TRUE)
#'
#' # example with data frame uniquely identified by `by` var
#'
#' y1 <- data.table(id = c("1","3", "2", "9"),
#'                  y  = c(11L, 15L, 18L, 20L))
#' is_id(y1, by = "id")
is_id <- function(dt,
                  by,
                  verbose  = getOption("joyn.verbose", default = FALSE),
                  return_report  = FALSE) {

  # Ensure dt is a data.table
  if (!is.data.table(dt)) {
    dt <- as.data.table(dt)
  }

  # Check for duplicates
  is_id <- !(anyDuplicated(dt, by = by) > 0)

  if (verbose) {
    if (is_id) {
      cli::cli_alert_success("No duplicates found by {.code {by}}")
    } else {
      cli::cli_alert_warning("Duplicates found by: {.code {by}}")
    }
  }

  if (return_report) {
    # Return the duplicated rows if requested
    if (verbose) cli::cli_h3("Duplicates in terms of {.code {by}}")

    d <- freq_table(x = dt,
                     byvar = by,
                     freq_var_name = "copies")

    if (verbose) {
      d |>
        fsubset(copies > 1) |>
        print()
    }

    if (verbose) cli::cli_rule(right = "End of {.field is_id()} report")
    return(invisible(d))
  } else {
    return(is_id)
  }
}
