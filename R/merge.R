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
                  reportvar = "report"
                  ) {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #                   Initial parameters   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  if (!(is.data.table(x))) {
    x <- as.data.table(x)
  }


  if (!(is.data.table(y))) {
    y <- as.data.table(y)
  }


  type <- match.arg(type)
  keep <- match.arg(keep)

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
  #             create report variable   ---------
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

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #                   Report variable   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  # replace NAs in report vars
  setnafill(x, fill = 0, cols = c("x_report", "y_report"))

  # report variable
  if (is.character(reportvar)) {
    x[,
      (reportvar) := {
        r <- x_report + y_report
        q <- fcase(r == 1, "only in x",
                   r == 2, "only in y",
                   r == 3, "in both",
                   r == 4, "NA updated",
                   r == 5, "value updated",
                   r == 6, "not updated",
                   default = "conflict. check")
        q
      }
      ]
  }

  x[,
    c("x_report", "y_report") := NULL
  ]


  return(x)

}
