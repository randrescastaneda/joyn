#' Joyn workhorse function, always doing full merge
#'
#' @param x data object for left side of merge
#' @param y data object for right side of merge
#' @param by character vector: keys for merge, must be common to both tables
#' @param match_type character vector length 1: `1:1`, `m:1`, `1:m`, or `m:m`
#' @param ... other parameters from collapse::join
#'
#' @return
#' @export
#'
#' @examples
joyn_workhorse <- function(
    x,
    y,
    by,
    match_type = c(
      "1:1",
      "1:m",
      "m:1",
      "m:m"
    ),
    ...
) {

  # Argument checks ----
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


  if (!is.null(match_type) && match_type == "m:m") {

  # full cartesian m:m join using data.table ----
    result <- data.table::merge.data.table(
      x               = x,
      y               = y,
      by              = by,
      allow.cartesian = TRUE,
      suffix          = c(".x", ".y"), # keep suffixes consistent
      all             = TRUE
    )

    # Add` _merge` report column
    result <- add_merge_report_column(
      output_table = result,
      x            = x,
      y            = y,
      by           = by,
    )


  } else {
  # Full join using `collapse` ----
    result <- collapse::join(x, y, how = "full",
                             suffix = c(".x", ".y"),
                             keep.col.order = TRUE,
                             column = list("_merge", c("1", "2", "3")),
                             ...)
  }

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
    result
  )

}







