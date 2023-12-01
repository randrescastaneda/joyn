
#' Internal workhorse join function, used in the backend of `joyn`
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
#' @return
#'
joyn_workhorse <- function(
    x,
    y,
    by         = intersect(names(x), names(y)),
    match_type = getOption("joyn.match_type"),
    suffix     = getOption("joyn.suffixes"), # data.table suffixes
    keep       = c(
      "full",
      "left",
      "master",
      "right",
      "using",
      "inner"
    )
) {

  # Argument checks ----
  match_type <- match.arg(match_type)
  keep       <- match.arg(keep)

  # Measure time
  start_time <- Sys.time()


  if (!is.null(match_type) && match_type == "m:m") {

        # Perform join specified by `keep`
        if (keep == "inner") {

          dt_result <- data.table::merge.data.table(x               = x,
                                                    y               = y,
                                                    by              = by,
                                                    sort            = FALSE,
                                                    suffix          = suffix,
                                                    allow.cartesian = TRUE)

        } else if (keep %in% c("right", "using")) {

          dt_result <- data.table::merge.data.table(x               = x,
                                                    y               = y,
                                                    by              = by,
                                                    all.y           = TRUE,
                                                    sort            = FALSE,
                                                    suffix          = suffix,
                                                    allow.cartesian = TRUE)

        } else if (keep %in% c("left", "master")) {
          dt_result <- data.table::merge.data.table(x               = x,
                                                    y               = y,
                                                    by              = by,
                                                    all.x           = TRUE,
                                                    sort            = FALSE,
                                                    suffix          = suffix,
                                                    allow.cartesian = TRUE)

        } else  {

          dt_result <- data.table::merge.data.table(x               = x,
                                                    y               = y,
                                                    by              = by,
                                                    all             = TRUE,
                                                    sort            = FALSE,
                                                    suffix          = suffix,
                                                    allow.cartesian = TRUE)

        }


  } else {

    keep_collapse <- switch(
      keep,
      "master" = "left",
      "using"  = "right",
      keep
    )

    dt_result <- collapse::join( x,
                                 y,
                                 how            = keep_collapse,
                                 on             = by,
                                 validate       = "m:m",    # no checks performed
                                 suffix         = suffix,   # data.table suffixes
                                 keep.col.order = TRUE,
                                 verbose        = FALSE,    # suppress `collapse` messages
                                 column         = NULL      # list(getOption("joyn.reportvar"), c("1", "2", "3")),
                                 )
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
    dt_result
  )

}







