#' Merge two tables
#'
#' This is the main and, basically, the only function in joyn.
#'
#' @param x data frame: referred to *left* in R terminology, or *master* in
#'   Stata terminology.
#' @param y data frame: referred to *right* in R terminology, or *using* in
#'   Stata terminology.
#' @param by a character vector of variables to join by. If NULL, the default,
#'   joyn will do a natural join, using all variables with common names across
#'   the two tables. A message lists the variables so that you can check they're
#'   right (to suppress the message, simply explicitly list the variables that
#'   you want to join). To join by different variables on x and y use a vector
#'   of expressions. For example, `by = c("a = b", "z")` will use "a" in x, "b"
#'   in y, and "z" in both tables.
#' @param match_type character: one of *"m:m"*, *"m:1"*, *"1:m"*, *"1:1"*.
#'   Default is *"m:m"* since this is the default generally used in joins in R.
#'   However, following Stata's recommendation, it is better to be explicit and
#'   use any of the other three match types (See details in *match types
#'   sections*).
#'
#' @param keep character: One of *"full"*, *"left"*, *"master"*, *"right"*,
#'   *"using"*, *"inner"*. Default is *"full"*. Even though this is not the
#'   regular behavior of joins in R, the objective of `joyn` is to present a
#'   diagnosis of the join, so that it must use by default a full join. Yet, if
#'   *"left"* or *"master"*, it keeps the observations that matched in both
#'   tables and the ones that did not match in x. The ones in y will be
#'   discarded. If *"right"* or *"using"*, it keeps the observations that
#'   matched in both tables and the ones that did not match in y. The ones in x
#'   will be discarded. If *"inner"*, it only keeps the observations that
#'   matched both tables.
#' @param roll double: *to be implemented*
#' @param yvars character: Vector of variable names that will be kept after the
#'   merge. If TRUE (the default), it keeps all the brings all the variables in
#'   y into x. If FALSE or NULL, it does not bring any variable into x, but a
#'   report will be generated.
#' @param reportvar character: Name of reporting variable. Default if "report".
#'   This is the same as variable "_merge" in Stata after performing a merge. If
#'   FALSE or NULL, the reporting variable will be excluded from the final
#'   table, though a summary of the join will be display after concluding.
#' @param reporttype character: One of *"character"* or *"numeric"*. Default is
#'   *"character"*. If *"numeric"*, the reporting variable will contain  numeric
#'   codes of the source and the contents of each observation in the joined
#'   table.
#' @param update_NAs logical: If TRUE, it will update NA values of all variables
#'   in x with actual values of variables in y that have the same name as the
#'   ones in x. If FALSE, NA values won't be updated, even if `update_values` is
#'   `TRUE`
#' @param update_values logical: If TRUE, it will update all values of variables
#'   in x with the actual of variables in y with the same name as the ones in x.
#'   **NAs from y won't be used to update actual values in x**. Yet, by default,
#'   NAs in x will be updated with values in y. To avoid this, make sure to set
#'   `update_NAs = FALSE`
#' @param verbose logical: if FALSE, it won't display any message (programmer's
#'   option). Default is TRUE.
#' @param keep_y_in_x logical: If TRUE, it will keep the original variable from
#'   y when both tables have common variable names. Thus, the prefix "y." will
#'   be added to the original name to distinguish from the resulting variable in
#'   the joined table.
#' @param  sort logical: If TRUE, sort by key variables in `by`. Default is
#'   TRUE.
#' @param allow.cartesian logical: Check documentation in official [web
#'   site](https://rdatatable.gitlab.io/data.table/reference/merge.html/).
#'   Default is `NULL`, which implies that if the join is "1:1" it will be
#'   `FALSE`, but if the join has any "m" on it, it will be converted to `TRUE`.
#'   By specifying `TRUE` of `FALSE` you force the behavior of the join.
#'
#' @return a data.table joining x and y.
#' @export
#'
#' @section match types:
#'
#'   Using the same wording of the [Stata
#'   manual](https://www.stata.com/manuals/dmerge.pdf)
#'
#'   **1:1**: specifies a one-to-one match merge. The variables specified in
#'   `by`  uniquely identify single observations in both table.
#'
#'   **1:m and m:1**: specify _one-to-many_ and _many-to-one_ match merges,
#'   respectively. This means that in of the tables the observations are
#'   uniquely identify by the variables in `by`, while in the other table many
#'   (two or more)  of the observations are identify by the variables in `by`
#'
#'   **m:m** refers to _many-to-many merge_. variables in `by` does not uniquely
#'   identify the observations in either table. Matching is performed by
#'   combining observations with equal values in `by`; within matching values,
#'   the first observation in the master (i.e. left or x) table is matched with
#'   the first matching observation in the using (i.e. right or y) table; the
#'   second, with the second; and so on. If there is an unequal number of
#'   observations within a group, then the last observation of the shorter group
#'   is used repeatedly to match with subsequent observations of the longer
#'   group.
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
#' merge(x2, y2, by = "id", update_NAs = TRUE)
#'
#' # Update values in x with variables from y
#' merge(x2, y2, by = "id", update_values = TRUE)
#'
merge <- function(x,
                  y,
                  by              = intersect(names(x), names(y)),
                  yvars           = TRUE,
                  match_type      = c("m:m", "m:1", "1:m", "1:1"),
                  keep            = c("full", "left", "master",
                                    "right", "using", "inner"),
                  update_values   = FALSE,
                  update_NAs      = update_values,
                  reportvar       = getOption("joyn.reportvar"),
                  reporttype      = c("character", "numeric"),
                  roll            = NULL,
                  keep_y_in_x     = FALSE,
                  sort            = TRUE,
                  verbose         = getOption("joyn.verbose"),
                  allow.cartesian = NULL) {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #                   Initial parameters   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  ## correct inputs --------
  match_type  <- match.arg(match_type)
  keep        <- match.arg(keep)
  reporttype  <- match.arg(reporttype)

  ## report variable -------
  reportvar <- check_reportvar(reportvar)

  ## check data frame class  ------

  # the reuslting table should have the same class as the x table.
  class_x <- class(x)
  # If match type id m:m we need to convert to data.table
  if (match_type == "m:m") {
    x <- as.data.table(x)
    y <- as.data.table(y)
  }



  ## Modify BY when is expression   ---------

  fixby  <- check_by_vars(by, x, y)
  by     <- fixby$by


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #           Consistency of join   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  tx <- gsub("([m1]):([m1])", "\\1", match_type)
  ty <- gsub("([m1]):([m1])", "\\2", match_type)

  match_type_error <- FALSE

  if (tx == "1") {

    isidx <- is_id(x, by = by, verbose = FALSE)

    if (isFALSE(isidx)) {

      match_type_error <- TRUE
      if (verbose) {

        cli::cli_alert_danger("table {.field x} is not uniquely identified
                            by {.code {by}}", wrap = TRUE)
      }
    }
  }

  if (ty == "1") {

    isidy <- is_id(y, by = by, verbose = FALSE)

    if(isFALSE(isidy)) {

      match_type_error <- TRUE

      if (verbose) {

      cli::cli_alert_danger("table {.field y} is not uniquely identified
                            by {.code {by}}", wrap = TRUE)
      }
    }
    # join_consistency(y, by, "y")
  }

  if(match_type_error) {

    msg     <- "match type inconsistency"
    hint    <- "you could use `return_report = TRUE` in `joyn::is_id()`
    to see where the problem is"
    rlang::abort(c(
      msg,
      i = hint
      ),
     class = "joyn_error"
     )

    }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #              Variables to keep in y   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  ## treatmetn of yvars ------
  if (isTRUE(yvars)) {

    yvars <- names(y)

  }  else if (isFALSE(yvars) || is.null(yvars)) {

    temp_yvar <- paste0("temp_var", floor(stats::runif(1)*1000))
    yvars     <-  temp_yvar

    y[, (temp_yvar) := 1]

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

  ## Select variables in y ------
  y <- y[, .SD, .SDcols = c(by, yvars)]

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #                   Check variables in X   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  xvars <- names(x)
  xvars <- xvars[!(xvars %in% by)]

  upvars <- intersect(xvars, yvars)

  if (length(upvars) != 0) {

    # rename vars in y so they are different to x's when joined
      y.upvars <- paste0(upvars, ".y")
      newyvars <- yvars
      newyvars[newyvars %in% upvars] <- y.upvars

      setnames(y, old = yvars, new = newyvars)

      yvars <- newyvars


      if (isFALSE(update_NAs) && isFALSE(update_values)) {

      if (verbose) {
        cli::cli_alert_info("variable{?s} {.code {upvars}} in table y {?is/are}
                            ignored because arguments `update_NAs` and
                            `update_values` are FALSE.",
                            wrap = TRUE)
      }

      # we remove y.upvars from x ahead in the code when keep_y_in_x is false.

    }
  } # end of update vars

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #             include report variable   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  yvars_w <- c(yvars, "y_report") # working yvars
  x[, x_report := 1]
  y[, y_report := 2]

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #                   Actual merge   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  # simple merge
  i.yvars <- paste0("i.", yvars_w)


  # cartesian merge
  if (is.null(allow.cartesian)) {
    if (tx == "m" || ty == "m") {
      allow.cartesian <- TRUE
    } else {
      allow.cartesian <- FALSE
    }
  }

  # keep relevant variables in y

  y <- y[,
         mget(c(by, yvars_w))]


  if (keep == "inner") {

    x <- data.table::merge.data.table(x               = x,
                                      y               = y,
                                      by              = by,
                                      sort            = FALSE,
                                      allow.cartesian = allow.cartesian)

  } else if (keep %in% c("right", "using")) {

    x <- data.table::merge.data.table(x               = x,
                                      y               = y,
                                      by              = by,
                                      all.y           = TRUE,
                                      sort            = FALSE,
                                      allow.cartesian = allow.cartesian)

  } else if (keep %in% c("left", "master")) {
    x <- data.table::merge.data.table(x               = x,
                                      y               = y,
                                      by              = by,
                                      all.x           = TRUE,
                                      sort            = FALSE,
                                      allow.cartesian = allow.cartesian)

  } else  {

    x <- data.table::merge.data.table(x               = x,
                                      y               = y,
                                      by              = by,
                                      all             = TRUE,
                                      sort            = FALSE,
                                      allow.cartesian = allow.cartesian)

  }

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

    xnames      <- names(x)
    check_names <- c(xnames, reportvar)

    if (!identical(check_names,
                  make.names(check_names, unique = TRUE))) {

      check_names <- make.names(check_names, unique = TRUE)
      nrv         <- setdiff(check_names, xnames)

      if (verbose) {
        cli::cli_alert_info("reportvar {.code {reportvar}} is already
                            part of the resulting table. It will be changed
                            to {.code {nrv}}",
                            wrap = TRUE)
      }


      reportvar <- nrv

    }

    dropreport <- FALSE
  }


  # report variable
  x[, (reportvar) :=  x_report + y_report]

  ## rows to keep -----
  if (keep  %in% c("master", "left") ) {

    x <- x[get(reportvar)  != 2]

  } else if (keep  %in% c("using", "right") ) {

    x <- x[get(reportvar)  != 1]

  } else if (keep  == "inner") {
    x <- x[get(reportvar)  >= 3]
  }



  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #                   Update x   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (length(upvars) != 0) {

    if  (isTRUE(update_values)) {

      for (i in seq_along(upvars)) {
        update_values(x, upvars[i])
      }

    }

    if (isTRUE(update_NAs)) {

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


  ## Rename by variables -----

  if (!is.null(fixby$xby)) {
    setnames(x, fixby$tempkey, fixby$xby)
    by <- fixby$xby
    # not necessary
    # setnames(y, fixby$tempkey, fixby$yby)
  }

  ## Remove temporal yvars -----
  if (exists("temp_yvar")) {
    x[, (temp_yvar) := NULL]
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

    d <- freq_table(x, reportvar)

    print(d[])

    cli::cli_rule(right = "End of {.field JOYn} report")

    if (all(x[[reportvar]] %in% c("x", "y")) ||
        all(x[[reportvar]] %in% c(1, 2))) {
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

  if (sort) {
    setorderv(x, by, na.last = TRUE)
    setattr(x, 'sorted', by)
  }


  return(x)

}
