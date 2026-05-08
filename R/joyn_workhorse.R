
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


        store_joyn_msg(err = "{.pkg {source_pkg}} returned the following: {e$message}")
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

    # Remap .joyn1 values in one pass.
    # collapse::join returns: 1=matched (both), 2=x-only, 3=y-only
    # joyn convention (by factor label order): 1=x-only, 2=y-only, 3=matched
    # So we remap: collapse 1 → joyn 3 (matched "x & y"),
    # collapse 2 → joyn 1 (x-only), collapse 3 → joyn 2 (y-only).
    # Verified against factor level ordering.
    # Unnamed double vector: position i maps collapse code i → joyn code.
    # Must remain double (not integer) to preserve class=="numeric" contract
    # when reporttype="numeric". Avoids as.character() allocation.
    mapping <- c(3, 1, 2)
    dt_result <- ftransform(dt_result,
                            .joyn1 = mapping[as.integer(.joyn1)])
    data.table::setnames(dt_result, ".joyn1", reportvar)


  # Calculate the time taken
  end_time <- Sys.time()
  time_taken <- end_time - start_time

  store_joyn_msg(timing = paste("The full joyn is executed in",
                                round(time_taken, 6)))

  # Return ----
    dt_result
}







