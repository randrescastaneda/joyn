#' Merge two data.frames
#'
#' @param x data frame: In Stata terminology, it is known as the master data
#' @param y data frame: In Stata terminology, it is known as the using data
#' @param by
#' @param roll
#' @param type
#' @param keep
#' @param yvars
#' @param reportvar
#' @param reporttype
#' @param updateNA
#' @param update_values
#' @param verbose
#'
#' @return
#' @export
#' @import data.table
#'
#' @examples
merge <- function(x,
                  y,
                  by            = NULL,
                  roll          = NULL,
                  yvars         = NULL,
                  type          = c("m:m", "m:1", "1:m", "1:1"),
                  keep          = c("both", "full", "left", "master", "right", "using", "inner"),
                  update_values = FALSE,
                  updateNA      = update_values,
                  reportvar     = "report",
                  reporttype    = c("character", "numeric"),
                  verbose       = TRUE) {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #                   Initial parameters   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  type       <- match.arg(type)
  keep       <- match.arg(keep)
  reporttype <- match.arg(reporttype)


  if (!(is.data.table(x))) {
    x <- as.data.table(x)
  } else {
    x <- data.table::copy(x)
  }


  if (!(is.data.table(y))) {
    y <- as.data.table(y)
  } else {
    y <- data.table::copy(y)
  }


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #                   Manage by when Null   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  if (is.null(by)) {
    by <- intersect(names(x), names(y))

    if (length(by) == 0) {
      msg     <- "no common variable names in `x` and `y`"
      hint    <- "Make sure all variables are spelled correctly.
      Check for upper and lower cases"
      problem <- "When `by = NULL`, joyn search for common variable
      names to be used as keys"
      rlang::abort(c(
                    msg,
                    i = hint,
                    x = problem
                    ),
                    class = "joyn_error"
                    )

    }

    if (verbose) {
      cli::cli_alert_info("joining by {.code {by}}")
    }

  } # end of isnull by

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #              Variables to keep in y   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  if (is.null(yvars)) {

    yvars <- names(y)
    yvars <- yvars[! yvars %in% by]

  } else {

    not_in_y <- setdiff(yvars, names(y))

    if (length(not_in_y) != 0) {
      msg     <- "variables to keep from `y` are not present in `y`"
      problem <- glue::glue("{glue::glue_collapse(glue::backtick(not_in_y),
                            sep = ', ', last ='  and ')} \\
                            not present in variable `y`")
      rlang::abort(c(
                    msg,
                    x = problem
                    ),
                    class = "joyn_error"
                    )

    }

  } # end of else

  # remove id variables

  if (any(yvars %in% by) && verbose) {
    cli::cli_alert("removing key variables {.code {yvars[yvars %in% by]}}
                   from {.field yvars}",
                   wrap =  TRUE)
  }
  yvars <- yvars[! yvars %in% by]

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #                   Check variables in X   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  xvars <- names(x)
  xvars <- xvars[!(xvars %in% by)]

  upvars <- intersect(xvars, yvars)

  if (length(upvars) != 0) {

    if (isTRUE(updateNA) || isTRUE(update_values)) {
      # rename vars in y so they are different to x's when joined
      y.upvars <- paste0("y.", upvars)
      newyvars <- yvars
      newyvars[newyvars %in% upvars] <- y.upvars

      setnames(y, old = yvars, new = newyvars)

      yvars <- newyvars


    } else {

      if (verbose) {
        cli::cli_alert_info("variable{?s} {.code {upvars}} in `y` {?is/are}
                            ignored in merge
                            because `updateNA` and `update_values` are FALSE.",
                            wrap = TRUE)
      }

      yvars <- yvars[!(yvars %in% upvars)]

    }


  }



  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #             include report variable   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  yvars <- c(yvars, "y_report")
  x[, x_report := 1]
  y[, y_report := 2]

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #                   Actual merge   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  # simple merge
  i.yvars <- paste0("i.", yvars)

  x[y,
    on = by,
    (yvars) := mget(i.yvars)]

  # complement
  ty <- y[!x,
          on = by]

  x <- rbindlist(l         = list(x, ty),
                 use.names = TRUE,
                 fill      = TRUE)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #                   Report variable   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  # replace NAs in report vars
  setnafill(x, fill = 0, cols = c("x_report", "y_report"))

  # report variable
  if (isFALSE(reportvar) || is.null(reportvar)) {

    reportvar  <- "report"
    dropreport <- TRUE

  } else {
    dropreport <- FALSE
  }


  # report variable
  x[, (reportvar) :=  x_report + y_report]

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #                   Update x   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (length(upvars) != 0) {

    if  (isTRUE(update_values)) {

      for (i in seq_along(upvars)) {
        update_values(x, upvars[i])
      }

    }

    if (isTRUE(updateNA) && isFALSE(update_values)) {

      for (i in seq_along(upvars)) {
        update_NAs(x, upvars[i])
      }

    }


  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #                   Display results and cleaning   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  # cleaning temporary report variables
  x[,
    c("x_report", "y_report") := NULL
  ]

  # convert to characters if chosen
  if (reporttype == "character") {
    x[,
      (reportvar) := fcase(get(reportvar) == 1, "x",
                           get(reportvar) == 2, "y",
                           get(reportvar) == 3, "x & y",
                           get(reportvar) == 4, "NA updated",
                           get(reportvar) == 5, "value updated",
                           get(reportvar) == 6, "not updated",
                           default        = "conflict. check")
    ]

  }

  # Display results
  if (verbose) {

    # Display results in screen

    cli::cli_h2("JOYn Report")

    if (requireNamespace("janitor", quietly = TRUE)) {

      disp <- janitor::tabyl(x, !!reportvar)
      disp <- janitor::adorn_totals(disp, "row")
      disp <- janitor::adorn_pct_formatting(disp, digits = 1)

      print(disp)

    } else {

      table(x[[reportvar]])

    }

    if (all(x$report %in% c("x", "y") || x$report %in% c(1, 2))) {
      cli::cli_alert_warning(
        cli::col_red("you have no matchig obs. Make sure argument
                             `by` is correct. Right now, `joyn` is joining by
                             {.code {by}}"),
        wrap = TRUE)
    }
  } # end of reporting joyn


  if (dropreport) {
    x[, (reportvar) := NULL]
  }

  return(x)

}
