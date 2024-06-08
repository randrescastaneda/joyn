
#' Internal workhorse join function, used in the back-end of `joyn`
#'
#' Always executes a full join.
#'
#' @param x data object, "left" or "master"
#' @param y data object, "right" or "using"
#' @param by atomic character vector: key specifying join
#' @param suffixes atomic character vector: give suffixes to columns common to both
#' @param sort logical: sort the result by the columns in `by`
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
    sort       = FALSE,
    suffixes   = getOption("joyn.suffixes"), # data.table suffixes
    reportvar  =  getOption("joyn.reportvar")
) {

  # Argument checks ------------------------------------------------------------

  if (length(by) == 0) {

    store_joyn_msg(err = "In joyn_workhorse {.strongArg by} argument has length of 0")

    store_joyn_msg(info = "Either specify by to identify columns to join on in x
                   and y, or x and y should have common column names")

  }
  # Measure time
  start_time <- Sys.time()

  # Do a full join -------------------------------------------------------------
  source_pkg <- "collapse::join"

  # if not 1:1 => use merge.data.table

  # not m:m => use collapse::join()
    dt_result <- tryCatch(
      expr = {

        collapse::join(x              = x,
                       y              = y,
                       how            = "full",
                       on             = by,
                       multiple       = TRUE,     # matches row in x with m in y
                       validate       = "m:m",    # no checks performed
                       suffix         = suffixes,   # data.table suffixes
                       keep.col.order = TRUE,
                       sort           = sort,
                       verbose        = 0,
                       column         = ".joyn1",
                       attr           = TRUE)
      }, # end of expr section

      error = function(e) {


        joyn_msg("err", c("{.pkg {source_pkg}} returned the following:",
                          x = e$message))
      }, # end of error section

      warning = function(w) {
        if (grepl("[Oo]veridentified", w$message)) {

          store_joyn_msg(warn = "Your data is overidentified. Below the original
                         message from {.strong {source_pkg}}: \n{w$message}")

        } else {

          store_joyn_msg(warn = "{.strong {source_pkg}} returned the following
                         warning: \n{w$message}")

        }

        collapse::join(x              = x,
                       y              = y,
                       how            = "full",
                       on             = by,
                       multiple       = TRUE,     # matches row in x with m in y
                       validate       = "m:m",    # no checks performed
                       suffix         = suffixes,   # data.table suffixes
                       keep.col.order = TRUE,
                       sort           = sort,
                       verbose        = 0,
                       column         = ".joyn1",
                       attr           = TRUE)  |>
            suppressWarnings()

      }

    ) # End of trycatch

    # change values of .joyn1 to numeric to make it consistent with joyn
    mapping <- c('1' = 3, '2' = 1, '3' = 2)
    dt_result <- dt_result |>
      ftransform(.joyn1 = as.numeric(.joyn1)) |>
      ftransform(.joyn1 = mapping[as.character(.joyn1)]) |>
      frename(.joyn1 = reportvar, .nse = FALSE)


  # Calculate the time taken
  end_time <- Sys.time()
  time_taken <- end_time - start_time

  store_joyn_msg(timing = paste("The full joyn is executed in",
                                round(time_taken, 6)))

  # Return ----
    dt_result
}







