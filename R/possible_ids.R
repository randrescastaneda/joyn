#' Find possible unique identifies of data frame
#'
#' Identify possible variables uniquely identifying x
#'
#' @param dt data frame
#' @param exclude character: Exclude variables to be selected as identifiers. It
#'   could be either the name of the variables of one type of the variable
#'   prefixed by "_". For instance, "_numeric" or "_character".
#' @param include character: Name of variable to be included, that might belong
#'   to the group excluded in the `exclude`
#' @param verbose logical: If FALSE no message will be displayed. Default is
#'   TRUE
#'
#' @return list with possible identifiers
#' @export
#'
#' @examples
#' library(data.table)
#' x4 = data.table(id1 = c(1, 1, 2, 3, 3),
#'                 id2 = c(1, 1, 2, 3, 4),
#'                 t   = c(1L, 2L, 1L, 2L, NA_integer_),
#'                 x   = c(16, 12, NA, NA, 15))
#' possible_ids(x4)
possible_ids <- function(dt,
                         exclude = NULL,
                         include = NULL,
                         exclude_types = NULL,
                         include_types = NULL,
                         verbose = getOption("possible_ids.verbose", default = FALSE),
                         comb_size = 5,
                         get_all  = FALSE) {

  # Ensure dt is a data.table
  if (!is.data.frame(dt)) {
    stop("data must be a data frame")
  }
  if (!is.data.table(dt)) {
    dt <- as.data.table(dt)
  }

  # Get all variable names
  vars <- names(dt) |> copy()

  # Compute the primary class of each variable
  vars_class <- vapply(dt, function(x) class(x)[1], character(1))
  names(vars_class) <- vars  # Ensure names are preserved

  # Apply 'include' filter
  if (!is.null(include)) {
    vars <- intersect(vars, include)
  }

  # Apply 'include_types' filter
  if (!is.null(include_types)) {
    vars <- vars[vars_class[vars] %in% include_types]
  }

  # Apply 'exclude' filter
  if (!is.null(exclude)) {
    vars <- setdiff(vars, exclude)
  }

  # Apply 'exclude_types' filter
  if (!is.null(exclude_types)) {
    vars <- vars[!(vars_class[vars] %in% exclude_types)]
  }

  # Remove duplicate column names... just in case
  vars <- unique(vars)

  if (length(vars) == 0) {
    if (verbose) {
      cli::cli_alert_danger("No variables available after applying include/exclude filters.")
    }
    return(NULL) # should this be an error?
  }

  # Check if any variables uniquely identify the data individually
  unique_vars <- vapply(vars,
                        \(var) !anyDuplicated(dt[[var]]),
                        logical(1))

  # Collect variables that are unique identifiers
  unique_ids <- vars[unique_vars]

  # Initialize list to store possible IDs
  possible_ids_list <- list()

  # Add individual unique variables
  if (length(unique_ids) > 0) {
    possible_ids_list <- c(possible_ids_list, as.list(unique_ids))
    if (verbose) {
      cli::cli_alert_info("Found unique identifiers: {.code {unique_ids}}")
    }
  }

  # Remove unique identifiers from vars to reduce combinations
  vars <- setdiff(vars, unique_ids)

  # If data is uniquely identified by existing variables, return the unique IDs
  if (length(possible_ids_list) > 0 && fnrow(dt) == fnrow(unique(dt[, ..unique_ids]))) {
    return(possible_ids_list)
  }

  # Start with combinations of size 2 up to the number of remaining vars
  for (i in 2:min(length(vars), comb_size)) {  # Limit combination size to 5 for efficiency
    combos <- combn(vars, i, simplify = FALSE)

    if (verbose) {
      msg <- sprintf("combinations of %s variables", i)
      cli::cli_progress_bar(msg, total = length(combos))
    }

    for (combo in combos) {
      # Check if the combination uniquely identifies the data
      if (is_id(dt, by = combo, verbose = FALSE)) {
        # This is inefficient... it is copying every time...
        # I need to think better on how to do it.
        possible_ids_list <- c(possible_ids_list, list(combo))
        if (!get_all) {
          return(possible_ids_list)
        }
        # Remove variables in the current combo from vars to avoid redundant checks
        vars <- setdiff(vars, combo)
        break  # Break since we found a minimal unique key of size i
      }
      if (verbose) cli::cli_progress_update()
    }
    # Break if all variables are used
    if (length(vars) == 0) {
      break
    }
  }

  if (length(possible_ids_list) == 0) {
    if (verbose) {
      cli::cli_alert_warning("No unique identifier found.")
    }
    return(NULL)
  } else {
    return(possible_ids_list)
  }
}
