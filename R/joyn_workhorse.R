
#' Internal workhorse join function, used in the backend of `joyn`
#'
#' Always executes a full join. Gives comm
#'
#' @param x data object, "left" or "master"
#' @param y data object, "right" or "using"
#' @param by atomic character vector: key specifying join
#' @param match_type atomic character vector length 1: either "1:1" (default)
#'   "1:m", "m:1", or "m:m". If "m:m" then executes `data.table::merge.data.table`
#'   in the backend, otherwise uses `collapse::join()`
#' @param suffix atomic character vector: give suffix to columns common to both
#'   `x` and `y`
#' @return data object of same class as `x`
#'
joyn_workhorse <- function(
    x,
    y,
    by         = intersect(names(x), names(y)),
    match_type = c("1:1"),
    suffix     = getOption("joyn.suffixes") # data.table suffixes
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
      type = "err",
      err  = 'Error in `joyn_workhorse`: `by` argument has length of 0',
      timing = cli::symbol$info, "  ",
      pale   = 'Either specify `by` to identify columns to join on in `x` and `y`, or
              `x` and `y` should have common column names'
    )
  }
  # Measure time
  start_time <- Sys.time()

  # Do a full join -------------------------------------------------------------

  # if not 1:1 => use merge.data.table
  if (match_type == "m:m") {

    dt_result <- data.table::merge.data.table(
      x               = x,
      y               = y,
      by              = by,
      all             = TRUE,
      sort            = FALSE,
      suffix          = suffix,
      allow.cartesian = TRUE
    )

  } else {

    # not m:m => use collapse::join()
    dt_result <- collapse::join( x              = x,
                                 y              = y,
                                 how            = "full",
                                 on             = by,
                                 multiple       = TRUE,     # matches row in x with m in y
                                 validate       = "m:m",    # no checks performed
                                 suffix         = suffix,   # data.table suffixes
                                 keep.col.order = TRUE,
                                 verbose        = 0,
                                 column         = NULL
    )


  }


  # Calculate the time taken
  end_time <- Sys.time()
  time_taken <- end_time - start_time

  store_msg(
    type = "timing",
    timing = cli::symbol$record, "  ",
    timing = paste("the full joyn is executed in", round(time_taken, 6), "seconds" )
  )

  # Return ----
  return(
    dt_result
  )

}







