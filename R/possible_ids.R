#' Find possible unique identifies of data frame
#'
#' @param dt data frame
#' @param verbose logical: fi FALSE no message will be displayed. Default is TRUE
#'
#' @return
#' @export
#'
#' @examples
#' possible_ids(x4)
possible_ids <- function(dt,
                         verbose = TRUE) {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Check if data is data frame   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  if (!is.data.frame(dt)) {
    stop("data must be a data frame")
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## check all names are unieuq --------
  vars    <- names(dt)
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
      ee <- as.data.frame(cm[, sv])
      lv <- lapply(ee, unique)
      found <- TRUE

    }
  }

  if (verbose) {
    cli::cli_alert("we found {length(lv)} possible id{?s}")
  }

  return(lv)

}
