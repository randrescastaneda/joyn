#' Merge two tables
#'
#' This is the main and, basically, the only function in joyn.
#'
#' @param x data frame: referred to \emph{left} in R terminology, or
#'   \emph{master} in Stata terminology.
#' @param y data frame: referred to \emph{right} in R terminology, or
#'   \emph{using} in Stata terminology.
#' @param by a character vector of variables to join by. If NULL, the default,
#'   joyn will do a natural join, using all variables with common names across
#'   the two tables. A message lists the variables so that you can check they're
#'   right (to suppress the message, simply explicitly list the variables that
#'   you want to join). To join by different variables on x and y use a vector
#'   of expressions. For example, \code{by = c("a = b", "z")} will use "a" in x,
#'   "b" in y, and "z" in both tables.
#' @param join_type character: one of \emph{"m:m"}, \emph{"m:1"}, \emph{"1:m"},
#'   \emph{"1:1"}. Default is \emph{"m:m"} since this is the default generally
#'   used in joins in R. However, following Stata's recommendation, it is better
#'   to be explicit and use any of the other three join types (See details in
#'   \emph{Join types sections}).
#'
#' @param keep character: One of \emph{"full"}, \emph{"left"}, \emph{"master"},
#'   \emph{"right"}, \emph{"using"}, \emph{"inner"}. Default is \emph{"full"}.
#'   Even though this is not the regular behavior of joins in R, the objective
#'   of \code{joyn} is to present a diagnosys of the join, so that it must use
#'   by default a full join. Yet, if \emph{"left"} or \emph{"master"}, it keeps
#'   the observations that matched in both tables and the ones that did not
#'   match in x. The ones in y will be discarded. If \emph{"right"} or
#'   \emph{"using"}, it keeps the observations that matched in both tables and
#'   the ones that did not match in y. The ones in x will be discarded. If
#'   \emph{"inner"}, it only keeps the observations that matched both tables.
#' @param roll double: \emph{to be implemented}
#' @param yvars character: Vector of variable names that will be kept after the
#'   merge. Be defatult it keeps all the varialbes in y into x.
#' @param reportvar character: Name of reporting variable. Default if "report".
#'   This is the same as variable "_merge" in Stata after performing a merge. If
#'   FALSE or NULL, the reporting variable will be excluded from the final
#'   table, though a summary of the join will be display after concluding.
#' @param reporttype charcter: One of \emph{"character"} or \emph{"numeric"}.
#'   Default is \emph{"character"}. If \emph{"numeric"}, the reporting variable
#'   will contain  numeric codes of the source and the contents of each
#'   observation in the joined table.
#' @param updateNA logical: If TRUE, it will update NA values ok all variables
#'   in x with actual values of variables in y that have the same name as the
#'   ones in x. If FALSE, NA values won't be updated.
#' @param update_values logical: If TRUE, it will update all values of variables
#'   in x with the actual of variables in y with the same name as the ones in x.
#'   \strong{NAs from y won't be used to update actual values in x}.
#' @param verbose logical: if FALSE, it won't display any message (programmer's
#'   option). Default is TRUE.
#' @param keep_y_in_x logical: If TRUE, it will keep the original variable from
#'   y when both tables have common variable names. Thus, the prefix "y." will
#'   be added to the original name to distinguish from the resulting variable in
#'   the joined table.
#'
#' @return a data.table joining x and y.
#' @export
#' @import data.table
#'
#' @section Join types: Using the same wording of the
#'   \href{https://www.stata.com/manuals/dmerge.pdf}{Stata manual:} \describe{
#'   \item{1:1}{specifies a one-to-one match merge. The variables specified in
#'   \code{by}  uniquely identify single observations in both table.} \item{1:m
#'   and m:1}{specify one-to-many and many-to-one match merges, respectively.
#'   This means that one of the tables only one observation that uniquely
#'   identifies \emph{many} (i.e, two or more) observations in the other table.}
#'   \item{m:m}{many-to-many merge. variables in \code{by} does not uniquely
#'   identify the observations in either table. Matching is performed by
#'   combining observations with equal values in \code{by}; within matching
#'   values, the first observation in the master (i.e. left or x) table is
#'   matched with the first matching observation in the using (i.e. right or y)
#'   table; the second, with the second; and so on. If there is an unequal
#'   number of observations within a group, then the last observation of the
#'   shorter group is used repeatedly to match with subsequent observations
#'   ofthe longer group. }
#'
#'   }
#'
#' @examples
#' # Simple merge
#' library(data.table)
#' x1 = data.table(id = c(1L, 1L, 2L, 3L, NA_integer_),
#' t  = c(1L, 2L, 1L, 2L, NA_integer_),
#' x  = 11:15)
#'
#' y1 = data.table(id = 1:2,
#'                 y  = c(11L, 15L))
#'
#' x2 = data.table(id = c(1, 1, 2, 3, NA),
#'                 t  = c(1L, 2L, 1L, 2L, NA_integer_),
#'                 x  = c(16, 12, NA, NA, 15))
#'
#' y2 = data.table(id = c(1, 2, 5, 6, 3),
#'               yd = c(1, 2, 5, 6, 3),
#'               y  = c(11L, 15L, 20L, 13L, 10L),
#'               x  = c(16:20))
#' merge(x1, y1)
#'
#' # Bad merge for not specifying by argument
#' merge(x2, y2)
#'
#' # good merge, ignoring variable x from y
#' merge(x2, y2, by = "id")
#'
#' # update NAs in x variable form x
#' merge(x2, y2, by = "id", updateNA = TRUE)
#'
#' # Update values in x with variables from y
#' merge(x2, y2, by = "id", update_values = TRUE)
merge <- function(x,
                  y,
                  by            = NULL,
                  roll          = NULL,
                  yvars         = NULL,
                  join_type     = c("m:m", "m:1", "1:m", "1:1"),
                  keep          = c("full", "left", "master",
                                    "right", "using", "inner"),
                  update_values = FALSE,
                  updateNA      = update_values,
                  reportvar     = "report",
                  reporttype    = c("character", "numeric"),
                  keep_y_in_x   = FALSE,
                  verbose       = TRUE) {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #                   Initial parameters   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  join_type  <- match.arg(join_type)
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
  #           Modify BY when is expression   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  fixby  <- fix_by_vars(by, x, y)
  by     <- fixby$by

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #           Consistency of join   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  tx <- gsub("([m1]):([m1])", "\\1", join_type)
  ty <- gsub("([m1]):([m1])", "\\2", join_type)

  if (tx == "1") {
    join_consistency(x, by, "x")
  }

  if (ty == "1") {
    join_consistency(x, by, "y")
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

      yvars    <- yvars[!(yvars %in% upvars)]
      y.upvars <- NULL

    }
  } # end of update vars

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

    if (isFALSE(keep_y_in_x) && !is.null(y.upvars)) {
      x[, (y.upvars) := NULL]
    }

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #              Display results and cleaning   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  ## cleaning temporary report variables ----
  x[,
    c("x_report", "y_report") := NULL
  ]

  ## rows to keep -----
  if (keep  %in% c("master", "left") ) {

    x <- x[get(reportvar)  != 2]

  } else if (keep  %in% c("using", "right") ) {

    x <- x[get(reportvar)  != 1]

  } else if (keep  == "inner") {
    x <- x[get(reportvar)  >= 3]
  }


  ## Rename by variables -----

  if (!is.null(fixby$xby)) {
    setnames(x, fixby$tempkey, fixby$xby)
    setnames(y, fixby$tempkey, fixby$yby)
  }


  ## convert to characters if chosen -------
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

  ## Display results------
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

    if (all(x$report %in% c("x", "y")) || all(x$report %in% c(1, 2))) {
      cli::cli_alert_warning(
        cli::col_red("you have no matchig obs. Make sure argument
                             `by` is correct. Right now, `joyn` is joining by
                             {.code {by}}"),
        wrap = TRUE)
    }
  } # end of reporting joyn

  # Report var
  if (dropreport) {
    x[, (reportvar) := NULL]
  }


  return(x)

}
