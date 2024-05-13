
#' Internal workhorse join function, used in the backend of `joyn`
#'
#' Always executes a full join.
#'
#' @param x data object, "left" or "master"
#' @param y data object, "right" or "using"
#' @param by atomic character vector: key specifying join
#' @param match_type atomic character vector of length 1: either "1:1" (default)
#'   "1:m", "m:1", or "m:m". Relies on `collapse::join()`
#' @param suffixes atomic character vector: give suffixes to columns common to both
#'   `x` and `y`
#' @return data object of same class as `x`
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' # Full join
#' library(data.table)
#' x1 = data.table(id = c(1L, 1L, 2L, 3L, NA_integer_),
#'                 t  = c(1L, 2L, 1L, 2L, NA_integer_),
#'                 x  = 11:15)
#' y1 = data.table(id = c(1,2, 4),
#'                 y  = c(11L, 15L, 16))
#' joyn:::joyn_workhorse(x = x1, y=y1)
#' }
joyn_workhorse <- function(
    x,
    y,
    by         = intersect(names(x), names(y)),
    match_type = c("1:1"),
    suffixes     = getOption("joyn.suffixes") # data.table suffixes
) {

  # Argument checks ------------------------------------------------------------
  match_type <- match.arg(
    match_type,
    choices = c(
      "1:1",
      "1:m",
      "m:1",
      "m:m"
    )
  )
  if (
    length(by) == 0
  ) {
    store_msg(
      type        = "err",
      err         = cli::symbol$cross,
      err         = "  Error:",
      pale        = "  in joyn_workhorse,",
      bolded_pale = "  by",
      pale        = "  argument has length of 0"
    )

    store_msg(
      type  = "info",
      ok    = cli::symbol$info, "  ",
      note  = "\nNote:",
      pale  = "  Either specify by to identify columns to join on in x and y, or
              x and y should have common column names"
    )
  }
  # Measure time
  start_time <- Sys.time()

  # Do a full join -------------------------------------------------------------

  # if not 1:1 => use merge.data.table

  # not m:m => use collapse::join()
    dt_result <- tryCatch(
      expr = {
        source_pkg <- "collapse::join"

        collapse::join(x              = x,
                       y              = y,
                       how            = "full",
                       on             = by,
                       multiple       = TRUE,     # matches row in x with m in y
                       validate       = "m:m",    # no checks performed
                       suffix         = suffixes,   # data.table suffixes
                       keep.col.order = TRUE,
                       verbose        = 0,
                       column         = NULL)
      }, # end of expr section

      error = function(e) {


        joyn_msg("err", c("{.pkg {source_pkg}} returned the following:",
                          x = e$message))
      }, # end of error section

      warning = function(w) {
        if (grepl("[Oo]veridentified", w$message)) {
          store_msg(
            type  = "warn",
            ok    = paste(cli::symbol$warning, "\nWarning: "),
            pale  = "Your data is overidentified. Below the original message from {.pkg {source_pkg}}:",
            bolded_pale  = "\n{w$message}"
          )
        } else {
          store_msg(
            type  = "warn",
            ok    = paste(cli::symbol$warning, "\nWarning: "),
            pale  = "{.pkg {source_pkg}} returned the following warning:",
            bolded_pale  = "\n{w$message}"
          )
        }

          collapse::join( x              = x,
                          y              = y,
                          how            = "full",
                          on             = by,
                          multiple       = TRUE,     # matches row in x with m in y
                          validate       = "m:m",    # no checks performed
                          suffix         = suffixes,   # data.table suffixes
                          keep.col.order = TRUE,
                          verbose        = 0,
                          column         = NULL)  |>
            suppressWarnings()

      }

    ) # End of trycatch

  # Calculate the time taken
  end_time <- Sys.time()
  time_taken <- end_time - start_time

  store_msg(
    type    = "timing",
    timing  = paste(cli::symbol$record, "  Timing:"),
    pale    = "  The full joyn is executed in  ",
    timing  = round(time_taken, 6),
    pale    = "  seconds" )


  # Return ----
  return(
    dt_result
  )
}







