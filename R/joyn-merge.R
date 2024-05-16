#' Join two tables
#'
#' This is the primary function in the `joyn` package. It executes a full join,
#' performs a number of checks, and filters to allow the user-specified join.
#'
#' @param x data frame: referred to as *left* in R terminology, or *master* in
#'   Stata terminology.
#' @param y data frame: referred to as *right* in R terminology, or *using* in
#'   Stata terminology.
#' @param by a character vector of variables to join by. If NULL, the default,
#'   joyn will do a natural join, using all variables with common names across
#'   the two tables. A message lists the variables so that you can check they're
#'   correct (to suppress the message, simply explicitly list the variables that
#'   you want to join). To join by different variables on x and y use a vector
#'   of expressions. For example, `by = c("a = b", "z")` will use "a" in `x`,
#'   "b" in `y`, and "z" in both tables.
#' @param match_type character: one of *"m:m"*, *"m:1"*, *"1:m"*, *"1:1"*.
#'   Default is *"1:1"* since this the most restrictive. However, following
#'   Stata's recommendation, it is better to be explicit and use any of the
#'   other three match types (See details in *match types sections*).
#' @param keep  atomic character vector of length 1: One of *"full"*, *"left"*,
#'   *"master"*, *"right"*,
#'   *"using"*, *"inner"*. Default is *"full"*. Even though this is not the
#'   regular behavior of joins in R, the objective of `joyn` is to present a
#'   diagnosis of the join which requires a full join. That is why the default
#'   is a a full join. Yet, if *"left"* or *"master"*, it keeps the observations
#'   that matched in both tables and the ones that did not match in x. The ones
#'   in y will be discarded. If *"right"* or *"using"*, it keeps the
#'   observations that matched in both tables and the ones that did not match in
#'   y. The ones in x will be discarded. If *"inner"*, it only keeps the
#'   observations that matched both tables. Note that if, for example, a `keep =
#'   "left"`, the `joyn()` function still executes a full join under the hood
#'   and then filters so that only rows the output table is a left join. This
#'   behaviour, while inefficient, allows all the diagnostics and checks
#'   conducted by `joyn`.
#' @param y_vars_to_keep character: Vector of variable names in `y` that will be
#'   kept after the merge. If TRUE (the default), it keeps all the brings all
#'   the variables in y into x. If FALSE or NULL, it does not bring any variable
#'   into x, but a report will be generated.
#' @param reportvar character: Name of reporting variable. Default is ".joyn".
#'   This is the same as variable "_merge" in Stata after performing a merge. If
#'   FALSE or NULL, the reporting variable will be excluded from the final
#'   table, though a summary of the join will be display after concluding.
#' @param reporttype character: One of *"character"* or *"numeric"*. Default is
#'   *"character"*. If *"numeric"*, the reporting variable will contain  numeric
#'   codes of the source and the contents of each observation in the joined
#'   table. See below for more information.
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
#' @param keep_y_in_x `r lifecycle::badge("superseded")`: use now
#'   `keep_common_vars`
#' @param msg_type character: type of messages to display by default
#' @inheritParams data.table::setorderv
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
#'
#' @section reporttype:
#'
#'   If `reporttype = "numeric"`, then the numeric values have the following
#'   meaning:
#'
#'   1: row comes from `x`, i.e. "x" 2: row comes from `y`, i.e. "y" 3: row from
#'   both `x` and `y`, i.e. "x & y" 4: row has NA in `x` that has been updated
#'   with `y`, i.e. "NA updated" 5: row has valued in `x` that has been updated
#'   with `y`, i.e. "value updated" 6: row from `x` that has not been updated,
#'   i.e. "not updated"
#'
#' @section NAs order: `NA`s are placed either at first or at last in the
#'   resulting data.frame depending on the value of `getOption("joyn.na.last")`.
#'   The Default is `FALSE` as it is the default value of
#'   [data.table::setorderv].
#'
#'
#' @examples
#' # Simple join
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
#' joyn(x1, y1, match_type = "m:1")
#'
#' # Bad merge for not specifying by argument or match_type
#' joyn(x2, y2)
#'
#' # good merge, ignoring variable x from y
#' joyn(x2, y2, by = "id", match_type = "m:1")
#'
#' # update NAs in x variable form x
#' joyn(x2, y2, by = "id", update_NAs = TRUE, match_type = "m:1")
#'
#' # Update values in x with variables from y
#' joyn(x2, y2, by = "id", update_values = TRUE, match_type = "m:1")
#'
joyn <- function(x,
                 y,
                 by               = intersect(names(x), names(y)),
                 match_type       = c("1:1", "1:m", "m:1", "m:m"),
                 keep             = c("full", "left", "master",
                                      "right", "using", "inner",
                                      "anti"),
                 y_vars_to_keep   = ifelse(keep == "anti", FALSE, TRUE),
                 update_values    = FALSE,
                 update_NAs       = update_values,
                 reportvar        = getOption("joyn.reportvar"),
                 reporttype       = c("character", "numeric"),
                 roll             = NULL,
                 keep_common_vars = FALSE,
                 sort             = FALSE,
                 verbose          = getOption("joyn.verbose"),
                 suffixes         = getOption("joyn.suffixes"),
                 allow.cartesian  = deprecated(),
                 yvars            = deprecated(),
                 keep_y_in_x      = deprecated(),
                 na.last          = getOption("joyn.na.last"),
                 msg_type         = getOption("joyn.msg_type")) {

  clear_joynenv()

  ## correct inputs --------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  keep        <- match.arg(keep)
  reporttype  <- match.arg(reporttype)
  match_type  <- match.arg(match_type,
                           choices = c("1:1", "1:m", "m:1", "m:m"))

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

  # This is triggering too many warnings... We should revisit later

  # if (lifecycle::is_present(allow.cartesian) && match_type != "m:m") {
  #   lifecycle::deprecate_warn(when = "0.1.5",
  #                             what = "merge(allow.cartesian)",
  #                             details = "always uses `allow.cartesian = TRUE`
  #                             if and only if `match_type == 'm:m'`")
  #   allow.cartesian <- NULL
  # }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #                   Initial parameters   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  start_joyn <- Sys.time()
    x <- copy(x)
    y <- copy(y)


  ## X and Y -----------
  check_xy(x,y)

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

  ## Modify BY when is expression ---------
  fixby  <- check_by_vars(by, x, y)
  by     <- fixby$by

  ## Check suffixes -------------
  check_suffixes(suffixes)

  if (keep == "anti" &
      (isTRUE(update_values) || isTRUE(update_NAs))) {
    store_msg("warn",
              warn = paste(cli::symbol$warning, "  Warning:"),
              pale = " cannot use arguments {.code update_values = TRUE}
                         and/or {.code update_NAs = TRUE} for anti join")
    update_values <- FALSE
    update_NAs    <- FALSE
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #           Consistency of join   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  mts <- check_match_type(x, y, by, match_type, verbose)
  tx  <- mts[1]
  ty  <- mts[2]


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #              Variables to keep in y   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  common_vars <- intersect(names(x), names(y))
  if (!(is.null(fixby$yby))) {
    common_vars <- common_vars[!(common_vars %in% c(fixby$yby, fixby$tempkey))]
  } else {
    common_vars <- common_vars[!(common_vars %in% fixby$by)]
  }
  ## treatment of y_vars_to_keep ------
  y_vars_to_keep <- check_y_vars_to_keep(y_vars_to_keep, y, by)

  ## Select variables in y ------
  filter_y_vars <-  c(by, y_vars_to_keep)
  y <- y |>
    fselect(filter_y_vars)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #             include report variable   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  yvars_w <- c(y_vars_to_keep, ".yreport") # working yvars ZP -------------------------------------
  #yvars_w <- c(newyvars, ".yreport") # working yvars
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

  # Perform workhorse join
  x <- joyn_workhorse(
    x          = x,
    y          = y,
    by         = by,
    match_type = match_type,
    suffixes   = suffixes,
    sort       = sort
  )

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #                   Report variable   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  # replace NAs in report vars
  setnafill(x, fill = 0, cols = c(".xreport", ".yreport"))

  # report variable
  dropreport <- FALSE
  if (isFALSE(reportvar) || is.null(reportvar)) {

    reportvar  <- getOption("joyn.reportvar")
    dropreport <- TRUE

  } else {

    xnames      <- names(x)
    check_names <- c(xnames, reportvar)

    if (!identical(check_names,
                   make.names(check_names, unique = TRUE))) {

      check_names <- make.names(check_names, unique = TRUE)
      nrv         <- setdiff(check_names, xnames)

      store_msg(type        = "info",
                ok          = paste(cli::symbol$info, " Note:  "),
                pale        = "reportvar",
                bolded_pale = "  {reportvar}",
                pale        = "  is already part of the resulting table. It will be changed to",
                bolded_pale = " {nrv}")
      reportvar <- nrv
    }
  }


  # report variable
  collapse::settransform(x, use_util_report = .xreport + .yreport)
  # Can this be done more efficiently with collapse?
  data.table::setnames(x, "use_util_report", reportvar)


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #                   Filter rows - `keep`   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  ## rows to keep -----
  if (keep  %in% c("master", "left") ) {

    x <- x |>
      fsubset(get(reportvar)  != 2)

  } else if (keep  %in% c("using", "right") ) {

    x <- x |>
      fsubset(get(reportvar)  != 1)

  } else if (keep  == "inner") {

    x <- x |>
      fsubset(get(reportvar)  >= 3)
  } else if (keep == "anti") {
    x <- x |>
      fsubset(get(reportvar) == 1)
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #                   Update x   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  var_use <- NULL
  if (isTRUE(update_NAs) || isTRUE(update_values)) {
    var_use <- common_vars
  }

  if (isTRUE(update_NAs || update_values) & length(var_use) > 0 ) {

      x <- update_na_values(dt           = x,
                            var          = var_use,
                            reportvar    = reportvar,
                            suffixes     = suffixes,
                            rep_NAs      = update_NAs,
                            rep_values   = update_values
      )

  }


  ### common vars ----------

  if (isFALSE(keep_common_vars)) {
    patterns <-
      suffixes |>
      gsub("\\.", "\\\\.", x = _) |>
      paste0("$")

    varx <- grep(patterns[1], names(x), value = TRUE)
    vary <- grep(patterns[2], names(x), value = TRUE)

    # delete Y vars with suffix
    collapse::get_vars(x, vary) <- NULL

    # remove suffixes
    nsvar <- gsub(patterns[1], "", varx)
    data.table::setnames(x, varx, nsvar)

  }


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #              Display results and cleaning   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  ## cleaning temporary report variables ----
  collapse::settransform(x,
                         .xreport = NULL,
                         .yreport = NULL)

  ## Rename by variables -----

  if (!is.null(fixby$xby)) {
    data.table::setnames(x, fixby$tempkey, fixby$xby)
    by <- fixby$xby
    # not necessary
    # setnames(y, fixby$tempkey, fixby$yby)
  }


  ## convert to characters if chosen -------
  if (reporttype == "character") {

    rvar_to_chr <- \(x) {
      data.table::fcase(x == 1, "x",
                        x == 2, "y",
                        x == 3, "x & y",
                        x == 4, "NA updated",
                        x == 5, "value updated",
                        x == 6, "not updated",
                        default = "conflict")
    }

    settransformv(x, reportvar, rvar_to_chr)

  }

  # no matching obs
  if ((all(x[[reportvar]] %in% c("x", "y")) ||
      all(x[[reportvar]] %in% c(1, 2))) &&
      !keep == "anti") {

    store_msg("warn",
              warn = paste(cli::symbol$warning, "  Warning:"),
              pale = " you have no matching obs. Make sure argument
                     `by` is correct. Right now, `joyn` is joining by
                     {.code {by}}")
  }

  ## Display results------
  # freq table
  d <- freq_table(x, reportvar)
  rlang::env_poke(.joynenv, "freq_joyn", d)

  # Report var
  if (dropreport) {
    get_vars(x, reportvar) <- NULL
  }

  # store timing
  end_joyn <- Sys.time()
  time_taken_joyn <- end_joyn - start_joyn
  store_msg(
    type    = "timing",
    timing  = paste(cli::symbol$record, "  Timing:"),
    pale    = "  The entire joyn function, including checks,
    is executed in  ",
    timing  = round(time_taken_joyn, 6),
    pale    = "  seconds"
  )


  # return messages
  if (verbose == TRUE) {
    cli::cli_h2("JOYn Report")
    joyn_report()
    cli::cli_rule(right = "End of {.field JOYn} report")
    joyn_msg(msg_type)
  }

  setattr(x, "class", class_x)
  x

}
