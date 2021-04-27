#' Find possible unique identifies of data frame
#'
#' @param dt data frame
#' @param exclude character: Exclude variables to be selected as identifiers. It
#'   could be either the name of the variables of one type of the variable
#'   prefixed by "_". For instance, "_numeric" or "_character".
#' @param include character: Name of variable to be included, that might belong
#'   to the group excluded in the `exclude`
#' @param verbose logical: fi FALSE no message will be displayed. Default is
#'   TRUE
#'
#' @return list with possible identifiers
#' @export
#'
#' @examples
#' possible_ids(x4)
possible_ids <- function(dt,
                         exclude = NULL,
                         include = NULL,
                         verbose = getOption("possible_ids.verbose")) {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Check inputs   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  if (!is.data.frame(dt)) {
    stop("data must be a data frame")
  }

  if (is.data.table(dt)) {
    dt <- as.data.frame(dt)
  }


  if (is.null(exclude) && !is.null(include)) {
    if (verbose) {
      cli::cli_alert_warning("Since {.code exclude} is NULL, {.code include}
                             directive does not make sense. Ignored.",
                             wrap = TRUE)
    }
    warning("inconsistent use of `include`")
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## exclude variables from check ------

  vars    <- names(dt)

  ### Exclude variable according to their type ---------
  if (!is.null(include)) {

    # Find position of variable to include
    ii <- which(names(dt) %in% include)

  } else {

    ii <- NULL

  }

  ### Exclude variable by name ---------
  if (!is.null(exclude)) {

    if (grepl("^_", exclude)) {

      exclude <- match.arg(exclude, c("_character", "_numeric"))

      # find variable that meet criteria and exclude them, making sure to include
      # the variables of the user.
      ex <- gsub("^_", "", exclude)
      FUN <- paste0("is.", ex)

      n_cols <- unlist(lapply(dt, FUN))
      n_cols[ii] <- FALSE

      vars <- names(dt)[!n_cols]

    } else {
      vars <- vars[!(vars %in% exclude)]

      if (verbose) {
       if (identical(vars, names(dt))) {
         cli::cli_alert_warning("Variable {.field {exclude}} is not available in data frame.
                                Nothing is excluded.", wrap = TRUE)
       }
      }

    }
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## check all names are unieuq --------
  dup_var <- duplicated(vars)

  if (any(dup_var)) {

    dvars <- vars[dup_var]

    msg     <- "column names must be unique"
    hint    <- "try changing the names using `make.names()`"
    problem <- glue::glue("{dvars} is/are duplicated")
    rlang::abort(c(
                  msg,
                  i = hint,
                  x = problem
                  ),
                  class = "error_class"
                  )

  }


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Find duplicates   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  duplicates <- is_id(dt, by = vars, verbose = FALSE)
  if (duplicates) {
    if (verbose) {
      cli::cli_alert_success("There are no duplicates in data frame")
    }
  } else {
    if (verbose) {
      cli::cli_alert_warning("Data has duplicates. returning NULL")
    }
    is_id(dt, by = vars, verbose = TRUE)
    return(NULL)
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Find ids   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  nvars <- length(vars)

  found <- FALSE
  i = 0
  while(i < nvars && found == FALSE) {
    i = i + 1
    cm <- utils::combn(vars, m = i)

    lcm <- dim(cm)[2]  # number of combinations of size j

    selected_vars <- vector(length = lcm)
    for (j in 1:lcm) {
      tvars <- cm[, j] # testing vars
      selected_vars[j] <- is_id(dt, by = tvars, verbose = FALSE)
    }

    sv <- which(selected_vars)

    if (length(sv) > 0) {

      if (length(sv) == 1 && i > 1) {

        lv <- list(V1 = cm[, sv])

      } else if (i == 1) {

        ee <- as.data.frame(t(cm[, sv]))
        lv <- lapply(ee, unique)

      } else {

        ee <- as.data.frame(cm[, sv])
        lv <- lapply(ee, unique)

      }

      found <- TRUE

    }
  }

  if (verbose) {
    cli::cli_alert("we found {length(lv)} possible id{?s}")
  }

  return(lv)

}
