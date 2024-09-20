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
                         verbose = getOption("possible_ids.verbose",
                                             default = FALSE),
                         max_combination_size = 5,
                         max_processing_time = 60, # in seconds
                         get_all  = FALSE) {

  # defenses ---------
  # Ensure dt is a data.table
  if (!is.data.frame(dt)) {
    stop("data must be a data frame")
  }
  if (!is.data.table(dt)) {
    dt <- as.data.table(dt)
  }

  # Get all variable names
  vars <- names(dt) |> copy()

  # Exclude and include -------

  ## classes ----------
  vars <- filter_by_class(dt = dt,
                          vars = vars,
                          include_types = include_types,
                          exclude_types = exclude_types)

  ## var names --------
  vars <- filter_by_name(vars, include, exclude)


  # Remove duplicate column names... just in case
  vars <- unique(vars)

  if (length(vars) == 0) {
    if (verbose) {
      cli::cli_alert_danger("No variables available after applying
                            include/exclude filters.")
    }
    return(NULL) # should this be an error?
  }

  # Unique values ---------

  # Sort variables by number of unique values (ascending order)
  unique_counts <- vapply(dt[, ..vars], fnunique, numeric(1))
  vars          <- vars[order(unique_counts)]
  unique_counts <- unique_counts[order(unique_counts)]
  n_row         <- fnrow(dt)
  unique_ids    <- vars[unique_counts == n_row]

  # Initialize list to store possible IDs
  possible_ids_list <- list()
  # Add individual unique variables
  if (length(unique_ids) > 0) {
    possible_ids_list <- c(possible_ids_list, as.list(unique_ids))
    if (verbose) {
      cli::cli_alert_info("Found unique identifiers: {.code {unique_ids}}")
    }
    if (!get_all) return(possible_ids_list)
  }

  # Remove unique identifiers from vars to reduce combinations
  vars <- setdiff(vars, unique_ids)
  if (length(vars) == 0) {
    # All variables are unique identifiers
    return(possible_ids_list)
  }
  unique_counts <- unique_counts[vars]

  # combinations -----------

  # Start testing combinations
  start_time <- Sys.time()
  max_size <- min(length(vars), max_combination_size)

  for (comb_size in 2:max_size) {

    combos <- combn(vars, comb_size, simplify = FALSE)

    # Prune combinations where the product of unique counts is less
    # than n_rows
    combos_to_keep <- vapply(combos, \(combo) {
      prod(unique_counts[combo]) >= n_rows
    }, logical(1))

    combos <- combos[combo_to_keep]

    # Estimate processing time and prune combinations
    est_times <- vapply(combos, function(combo) {
      estimate_combination_time(n_rows, unique_counts[combo])
    }, numeric(1))

    if (verbose) {
      msg <- sprintf("combinations of %s variables", comb_size)
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
        # Remove variables in the current combo from vars to
        # avoid redundant checks
        vars <- setdiff(vars, combo)
        unique_counts <- unique_counts[vars]
        break  # Break since we found a minimal unique key of size i
      }
      # Check processing time
      elapsed_time <- as.numeric(difftime(Sys.time(),
                                          start_time,
                                          units = "secs"))
      if (elapsed_time > max_processing_time) {
        if (verbose) {
          mxt_msg <- "Maximum processing time exceeded.
          modify {.arg max_processing_time} argument to increse time.
          Stopping search."
          cli::cli_alert_warning(mxt_msg)
        }
        break
      }
      if (verbose) cli::cli_progress_update()
    }
    # Break if all variables are used
    if (length(vars) == 0 || elapsed_time > max_processing_time) {
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


filter_by_class <- function(dt, vars, include_types, exclude_types) {
  # Compute the primary class of each variable
  vars_class <- vapply(dt, function(x) class(x)[1], character(1))
  names(vars_class) <- vars  # Ensure names are preserved

  # Apply 'include_types' filter
  if (!is.null(include_types)) {
    vars <- vars[vars_class[vars] %in% include_types]
  }

  # Apply 'exclude_types' filter
  if (!is.null(exclude_types)) {
    vars <- vars[!(vars_class[vars] %in% exclude_types)]
  }
  vars
}

filter_by_name <- function(vars, include, exclude) {
  # Apply 'include' filter
  if (!is.null(include)) {
    vars <- intersect(vars, include)
  }

  # Apply 'exclude' filter
  if (!is.null(exclude)) {
    vars <- setdiff(vars, exclude)
  }
  vars
}



# Function to estimate processing time based on unique counts
estimate_combination_time <- function(n_rows, unique_counts) {
  # Simple estimation function
  # Time is proportional to (product of unique counts) / n_rows
  # Adjust the constant factor based on empirical observations
  est_time <- (prod(unique_counts) / n_rows) * 0.0001
  return(est_time)
}
