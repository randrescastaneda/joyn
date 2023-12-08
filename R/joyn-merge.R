#' Join two tables
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
#'   Default is *"1:1"* since this the most restrictive.
#'   However, following Stata's recommendation, it is better to be explicit and
#'   use any of the other three match types (See details in *match types
#'   sections*).
#'
#' @param keep  atomic character vector of length 1:
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
#' @param y_vars_to_keep character: Vector of variable names that will be kept
#'   after the merge. If TRUE (the default), it keeps all the brings all the
#'   variables in y into x. If FALSE or NULL, it does not bring any variable
#'   into x, but a report will be generated.
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
#' @param keep_common_vars logical: If TRUE, it will keep the original variable
#'   from y when both tables have common variable names. Thus, the prefix "y."
#'   will be added to the original name to distinguish from the resulting
#'   variable in the joined table.
#' @param  sort logical: If TRUE, sort by key variables in `by`. Default is
#'   TRUE.
#' @param allow.cartesian logical: Check documentation in official [web
#'   site](https://rdatatable.gitlab.io/data.table/reference/merge.html/).
#'   Default is `NULL`, which implies that if the join is "1:1" it will be
#'   `FALSE`, but if the join has any "m" on it, it will be converted to `TRUE`.
#'   By specifying `TRUE` of `FALSE` you force the behavior of the join.
#' @param roll double: *to be implemented*
#' @param suffixes A character(2) specifying the suffixes to be used for making
#'   non-by column names unique. The suffix behaviour works in a similar fashion
#'   as the [base::merge] method does.
#' @param yvars `r lifecycle::badge("superseded")`: use now `y_vars_to_keep`
#' @param keep_y_in_x `r lifecycle::badge("superseded")`: use now `keep_common_vars`
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
joyn <- function(x,
                  y,
                  by               = intersect(names(x), names(y)),
                  match_type       = "1:1",
                  keep             = c("full", "left", "master",
                                       "right", "using", "inner"),
                  y_vars_to_keep   = TRUE,
                  update_values    = FALSE,
                  update_NAs       = update_values,
                  reportvar        = getOption("joyn.reportvar"),
                  reporttype       = c("character", "numeric"),
                  roll             = NULL,
                  keep_common_vars = FALSE,
                  sort             = TRUE,
                  verbose          = getOption("joyn.verbose"),
                  suffixes         = getOption("joyn.suffixes"),
                  allow.cartesian  = deprecated(),
                  yvars            = deprecated(),
                  keep_y_in_x      = deprecated()) {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #                   Life cycle   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (lifecycle::is_present(yvars)) {
    lifecycle::deprecate_warn("0.1.5",
                              "merge(yvars)",
                              "merge(y_vars_to_keep)")
    y_vars_to_keep <- yvars
  }
  if (lifecycle::is_present(keep_y_in_x)) {
    lifecycle::deprecate_warn("0.1.5",
                              "merge(keep_y_in_x)",
                              "merge(keep_common_vars)")
    keep_common_vars <- keep_y_in_x
  }
  if (lifecycle::is_present(allow.cartesian)) {
    lifecycle::deprecate_warn(when = "0.1.5",
                              what = "merge(allow.cartesian)",
                              details = "Now always uses `allow.cartesian = TRUE`
                              if and only if `match_type == 'm:m'`")
    allow.cartesian <- NULL
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #                   Initial parameters   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  start_joyn <- Sys.time()
  # copy objects if data.tables
  if (any(class(x) == "data.table")) {
    x <- copy(x)
  }
  if (any(class(y) == "data.table")) {
    y <- copy(y)
  }

  ## X and Y -----------
  check_xy(x,y)

  ## correct inputs --------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  match_type <- match.arg(
    match_type,
    choices = c(
      "1:1",
      "1:m",
      "m:1",
      "m:m"
    )
  )
  keep        <- match.arg(keep)
  reporttype  <- match.arg(reporttype)

  ## report variable -------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  reportvar <- check_reportvar(reportvar)

  ## check data frame class  ------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  # the resulting table should have the same class as the x table.
  class_x <- class(x)

  # If match type is m:m we need to convert to data.table
  if (match_type == "m:m") {
    x <- as.data.table(x)
    y <- as.data.table(y)
  }


  ## Modify BY when is expression   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  fixby  <- check_by_vars(by, x, y)
  by     <- fixby$by


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #           Consistency of join   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  mts <- check_match_type(x, y, by, match_type, verbose)
  tx  <- mts[1]
  ty  <- mts[2]


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #              Variables to keep in y   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  ## treatment of y_vars_to_keep ------
  y_vars_to_keep <- check_y_vars_to_keep(y_vars_to_keep, y, by)

  ## Select variables in y ------
  filter_y_vars <-  c(by, y_vars_to_keep)
  y <- y |>
    fselect(filter_y_vars)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # new names in Y for same-name variables in X   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  newyvars <- check_new_y_vars(x, by, y_vars_to_keep)


  # rename variables in Y
  if (!is.null(y_vars_to_keep) & !is.null(newyvars)) {
    setnames(y, old = y_vars_to_keep, new = newyvars)
  }


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #             include report variable   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  yvars_w <- c(newyvars, ".yreport") # working yvars
  x <- x |>
    ftransform(.xreport = 1)
  y <- y |>
    ftransform(.yreport = 2)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #                   Actual merge   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  # simple merge
  i.yvars <- paste0("i.", yvars_w)

  # keep relevant variables in y
  y <- y |> fselect(
    by, yvars_w
  )

  if (
    (keep == "anti" | keep == "semi") &
    match_type == "m:m"
  ) stop(
    "Anti and semi joins cannot be performed on m:m joins at this stage"
  )

  # Perform workhorse join
  x <- joyn_workhorse(
    x          = x,
    y          = y,
    by         = by,
    match_type = match_type,
    suffix     = suffixes
  )

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #                   Report variable   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  # replace NAs in report vars
  setnafill(x, fill = 0, cols = c(".xreport", ".yreport"))

  # report variable
  if (isFALSE(reportvar) || is.null(reportvar)) {

    reportvar  <- ".joyn"
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
  x$use_util_report <- x$`.xreport` + x$`.yreport`
  names(x)[length(names(x))] <- reportvar



  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #                   Filter rows - `keep`   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

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


  # return(
  #   list(
  #     update_values,
  #     x,
  #     newyvars,
  #     sub(
  #       pattern = "\\.y$",
  #       "",
  #       newyvars,
  #     ),
  #     sub(
  #       pattern = "\\.y$",
  #       replacement = "",
  #       x = newyvars[
  #         grepl(
  #           pattern = "\\.y$",
  #           x       = newyvars
  #         )
  #       ]
  #     )
  #   )
  # )
  # update values

  if (isTRUE(update_values) || isTRUE(update_NAs)) {
    var_use <- sub(
      pattern = "\\.y$",
      replacement = "",
      x = newyvars[
        grepl(
          pattern = "\\.y$",
          x       = newyvars
        )
      ]
    )
  }

  #return(list(reportvar))
  if (isTRUE(update_values)) {

    x <- update_values(
      dt        = x,
      var       = var_use,
      reportvar = reportvar
    )

  }


  # update NAs
  if (isTRUE(update_NAs)) {

    x <- update_NAs(
      dt        = x,
      var       = var_use,
      reportvar = reportvar
    )

  }


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #              Display results and cleaning   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  ## cleaning temporary report variables ----
  x$`.xreport` <- NULL
  x$`.yreport` <- NULL


  ## Rename by variables -----

  if (!is.null(fixby$xby)) {
    setnames(x, fixby$tempkey, fixby$xby)
    by <- fixby$xby
    # not necessary
    # setnames(y, fixby$tempkey, fixby$yby)
  }

  ## Remove temporal yvars -----
  if (exists("temp_yvar")) {

    x <- x[
      ,
      mget(
        names(x)[
          which(
            !names(x) %in% temp_yvar
          )
        ]
      )
    ]

  }



  ## convert to characters if chosen -------
  if (reporttype == "character") {

    applySwitch <- function(value) {
      switch(as.character(value),
             "1" = "x",
             "2" = "y",
             "3" = "x & y",
             "4" = "NA updated",
             "5" = "value updated",
             "6" = "not updated",
             "conflict. check")
    }

    # Apply the function to the column
    x[[reportvar]] <- sapply(x[[reportvar]], applySwitch)

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
    x |> fselect(get(reportvar)) <- NULL
  }

  if (sort) {
    setorderv(x, by, na.last = TRUE)
    setattr(x, 'sorted', by)
  }

  if (verbose == TRUE) {
    end_joyn <- Sys.time()
    time_taken_joyn <- end_joyn - start_joyn
    store_msg(
      type = "timing",
      timing = cli::symbol$record, "  ",
      timing = paste("the entire joyn function, including checks, is executed in", round(time_taken_joyn, 6), "seconds" )
    )

    # return messages
    joyn_msg()

  }



  return(x)

}
