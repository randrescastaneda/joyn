
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
#' @param keep atomic character vector of length 1:
#'   One of *"full"*, *"left"*, *"master"*, *"right"*,
#'   *"using"*, *"inner"*. Default is *"full"*. Even though this is not the
#'   regular behavior of joins in R, the objective of `joyn` is to present a
#'   diagnosis of the join which requires a full join. That is why the default
#'   is a a full join. Yet, if *"left"* or *"master"*, it keeps the observations
#'   that matched in both tables and the ones that did not match in x. The ones
#'   in y will be discarded. If *"right"* or *"using"*, it keeps the observations
#'   that matched in both tables and the ones that did not match in y. The ones in x
#'   will be discarded. If *"inner"*, it only keeps the observations that
#'   matched both tables.
#'
#' @return data object of same class as `x`
#'
joyn_workhorse <- function(
    x,
    y,
    by         = intersect(names(x), names(y)),
    match_type = c(
      "1:1",
      "1:m",
      "m:1",
      "m:m"
    ),
    suffix     = getOption("joyn.suffixes") # data.table suffixes
) {

  # Argument checks -----------------------------------------------------------
  match_type <- match.arg(
    match_type,
    choices = c(
      "1:1",
      "1:m",
      "m:1",
      "m:m"
    )
  )

  # Measure time
  start_time <- Sys.time()

  # Create report variables
  x <- x |>
    ftransform(
      x_report = 1L
    )
  y <- y |>
    ftransform(
      y_report = 2L
    )

  # Do a full join -------------------------------------------------------------

  # if many-to-many => use merge.data.table
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

    # not many-to-many => use collapse::join()
    dt_result <- collapse::join( x,
                                 y,
                                 how            = "full",
                                 on             = by,
                                 validate       = "m:m",    # no checks performed
                                 suffix         = suffix,   # data.table suffixes
                                 keep.col.order = TRUE,
                                 verbose        = 1,        # until collapse update
                                 column         = NULL
    )


  }


  # Report --------------------------------------------------------------------

  # replace NAs in report vars - using data.table
  setnafill(
    dt_result,
    fill = 0,
    cols = c("x_report", "y_report")
  )


  # Calculate the time taken
  end_time <- Sys.time()
  time_taken <- end_time - start_time

  # Store message ----
  store_msg(
    type   = "timing",
    timing = paste(
      "`joyn_workhorse` executed in",
      time_taken
    )
  )

  # Return ----
  return(
    dt_result
  )

}







