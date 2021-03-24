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
#'
#' @return
#' @export
#' @import data.table
#'
#' @examples
merge <- function(x,
                  y,
                  by     = NULL,
                  roll   = NULL,
                  yvars  = NULL,
                  type   = c("m:m", "m:1", "1:m", "1:1"),
                  keep   = c("both", "full", "left", "master", "right", "using", "inner"),
                  reportvar = "report",
                  reporttype = c("character", "numeric")
                  ) {

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
      hint    <- "Make sure all variables are spelled correctly. Check for upper cases"
      problem <- "When `by = NULL`, joyn search for common variable names to be used as keys"
      rlang::abort(c(
                    msg,
                    i = hint,
                    x = problem
                    ),
                    class = "joyn_error"
                    )

    }

    cli::cli_alert_info("joining by {.code {by}}")

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

  if (any(yvars %in% by)) {
    cli::cli_alert("removing key variables {.code {yvars[yvars %in% by]}}
                   from {.field yvars}",
                   wrap =  TRUE)
  }
  yvars <- yvars[! yvars %in% by]

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
  if (is.character(reportvar)) {

    if (reporttype == "character") {

      x[,
        (reportvar) := {
          r <- x_report + y_report
          q <- fcase(r == 1, "x",
                     r == 2, "y",
                     r == 3, "x & y",
                     r == 4, "NA updated",
                     r == 5, "value updated",
                     r == 6, "not updated",
                     default = "conflict. check")
          q
        }
        ]

    } else {

      x[, (reportvar) :=  x_report + y_report]

    }

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
  } # end of reporting joyn

  x[,
    c("x_report", "y_report") := NULL
  ]


  return(x)

}
