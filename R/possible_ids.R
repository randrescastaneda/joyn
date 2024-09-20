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
                         min_combination_size = 1,
                         max_combination_size = 5,
                         max_processing_time = 60, # in seconds
                         max_numb_possible_ids = 100,
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

  if (verbose) {
    cli::cli_alert_info("Variables to test: {.field {vars}}")
  }

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
  init_index    <- 0



  # Initialize list to store possible IDs
  possible_ids_list <- vector("list", max_numb_possible_ids)

  if (min_combination_size == 1) {
    unique_ids    <- vars[unique_counts == n_row]
    # Add individual unique variables
    init_index <- length(unique_ids)
    if (init_index > 0) {
      possible_ids_list[1:init_index] <- as.list(unique_ids)
      if (verbose) {
        cli::cli_alert_info("Found unique identifiers: {.code {unique_ids}}")
      }
      if (!get_all) return(remove_null(possible_ids_list))

      # Remove unique identifiers from vars to reduce combinations
      vars <- setdiff(vars, unique_ids)
      if (length(vars) == 0) {
        # All variables are unique identifiers
        return(remove_null(possible_ids_list))
      }
      unique_counts <- unique_counts[vars]
    }
  }

  # combinations -----------

  # Start testing combinations
  start_time <- Sys.time()
  max_size <- min(length(vars), max_combination_size)
  min_size <- max(min_combination_size, 2)

  j <- init_index + 1
  for (comb_size in min_size:max_size) {

    combos <- combn(vars, comb_size, simplify = FALSE)

    # Prune combinations where the product of unique counts is less
    # than n_rows
    combos_to_keep <- vapply(combos, \(combo) {
      prod(unique_counts[combo]) >= n_rows
    }, logical(1))

    combos <- combos[combos_to_keep]

    # Estimate processing time and prune combinations
    est_times <- vapply(combos, function(combo) {
      estimate_combination_time(n_rows, unique_counts[combo])
    }, numeric(1))

    if (verbose) {
      cli::cli_progress_bar(
        format = "combs of {cli::pb_extra$comb_size} vars: {cli::pb_bar} {cli::pb_percent} | ETA: {cli::pb_eta} | {cli::pb_current}/{cli::pb_total}",
        extra = list(comb_size = comb_size),
        total = length(combos))
    }

    for (combo in combos) {
      # Check if the combination uniquely identifies the data
      if (is_id(dt, by = combo, verbose = FALSE)) {
        # This is inefficient... it is copying every time...
        # I need to think better on how to do it.
        possible_ids_list[j] <- combo
        j <- init_index + 1
        if (j > max_numb_possible_ids) {
          if (verbose) {
            cli::cli_alert_warning(
            "Max number of possible IDs ({max_numb_possible_ids}) reached.
            You may modify it in argument {.arg max_numb_possible_ids}")
          }
          return(possible_ids_list)
        }
        if (!get_all) {
          return(remove_null(possible_ids_list))
        }
        # Remove variables in the current combo from vars to
        # avoid redundant checks
        vars <- setdiff(vars, combo)
        unique_counts <- unique_counts[vars]

        # Break since we found a minimal unique key of size i
        if (!get_all) break
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

  if (length(remove_null(possible_ids_list)) == 0) {
    if (verbose) {
      cli::cli_alert_warning("No unique identifier found.")
    }
    return(NULL)
  } else {
    return(remove_null(possible_ids_list))
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
  # Apply 'exclude' filter
  if (!is.null(exclude)) {
    vars <- setdiff(vars, exclude)
  }

  # Apply 'include' filter
  c(vars, include)

}



# Function to estimate processing time based on unique counts
estimate_combination_time <- function(n_rows, unique_counts) {
  # Simple estimation function
  # Time is proportional to (product of unique counts) / n_rows
  # Adjust the constant factor based on empirical observations
  est_time <- (prod(unique_counts) / n_rows) * 0.0001
  return(est_time)
}


remove_null <- \(x) {
  y <- vapply(x, \(.) !is.null(.), logical(1))
  x[y]
}
