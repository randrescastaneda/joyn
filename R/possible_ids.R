#' Find possible unique identifies of data frame
#'
#' Identify possible combinations of variables that  uniquely identifying dt
#'
#' @param dt data frame
#' @param vars character: A vector of variable names to consider for identifying unique combinations.
#' @param exclude character: Names of variables to exclude from analysis
#' @param include character: Name of variable to be included, that might belong
#'   to the group excluded in the `exclude`
#' @param exclude_classes character: classes to exclude from analysis (e.g.,
#'   "numeric", "integer", "date")
#' @param include_classes character: classes to include in the analysis (e.g.,
#'   "numeric", "integer", "date")
#' @param min_combination_size numeric: Min number of combinations. Default is
#'   1, so all combinations.
#' @param max_combination_size numeric. Max number of combinations. Default is
#'   5. If there is a combinations of identifiers larger than
#'   `max_combination_size`, they won't be found
#' @param max_processing_time numeric: Max time to process in seconds. After
#'   that, it returns what it found.
#' @param max_numb_possible_ids numeric: Max number of possible IDs to find. See
#'   details.
#' @param get_all logical: get all possible combinations based on the parameters
#'   above.
#' @param verbose logical: If FALSE no message will be displayed. Default is
#'   TRUE
#'
#' @section Number of possible IDs:
#'
#'   The number of possible IDs in a dataframe could be very large. This is why,
#'   `possible_ids()` makes use of heuristics to return something useful without
#'   wasting the time of the user. In addition, we provide multiple parameter so
#'   that the user can fine tune their search for possible IDs easily and
#'   quickly.
#'
#'   Say for instance that you have a dataframe with 10 variables. Testing every
#'   possible pair of variables will give you 90 possible unique identifiers for
#'   this dataframe. If you want to test all the possible IDs, you will have to
#'   test more 5000 combinations. If the dataframe has many rows, it may take a
#'   while.
#'
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
                         vars                        = NULL,
                         exclude                     = NULL,
                         include                     = NULL,
                         exclude_classes             = NULL,
                         include_classes             = NULL,
                         verbose                     = getOption("possible_ids.verbose",
                                                        default = FALSE),
                         min_combination_size        = 1,
                         max_combination_size        = 5,
                         max_processing_time         = 60, # in seconds
                         max_numb_possible_ids       = 100,
                         get_all                     = FALSE) {

  # defenses ---------
  # Ensure dt is a data.table
  if (!is.data.frame(dt)) {
    stop("data must be a data frame")
  }
  if (!is.data.table(dt)) {
    dt <- as.data.table(dt)
  }

  # Get variable
  # Vars --------

   if (is.null(vars)) {
     vars <- names(dt) |>
       copy()
     } else {

    # check if all vars are in dt
    missing_vars <- setdiff(vars, names(dt))

    if (length(missing_vars) > 0) {
       cli::cli_abort("The following variables are not in the data table: {.strongVar {missing_vars}}")
    }

    # check at least 2 vars are provided

     if (length(vars) < 2) {
      cli::cli_abort("Can't make combinations with a single var: {.strongVar {vars}}")
     }

    # exclude should not be used
      if (!(is.null(exclude) & is.null(exclude_classes))) {
        exclude         <- NULL
        exclude_classes <- NULL
        cli::cli_alert_danger("Args {.strongArg `exclude`} and {.strongArg `exclude_classes`} not available when using {.strongArg `vars`}")
      }

   }

  # Exclude and include -------

  ## classes ----------
  vars <- filter_by_class(dt              = dt,
                          vars            = vars,
                          include_classes = include_classes,
                          exclude_classes = exclude_classes)

  ## var names --------
  vars <- filter_by_name(vars, include, exclude, verbose)

  ##  no duplicated vars -------------
  if (anyDuplicated(vars)) {
    dupvars <- vars[duplicated(vars)] |>
      unique()
    cli::cli_abort("vars {.strongVar {dupvars}} are duplicated.")
  }

  if (verbose) {
    cli::cli_alert_info("Variables to test: {.strongVar {vars}}")
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
  n_rows        <- fnrow(dt)
  init_index    <- 0


  # Initialize list to store possible IDs
  possible_ids_list <- vector("list", max_numb_possible_ids)

  checked_ids <- vars |>
    copy()

  if (min_combination_size == 1) {
    unique_ids    <- vars[unique_counts == n_rows]
    # Add individual unique variables
    init_index <- length(unique_ids)
    if (init_index > 0) {
      possible_ids_list[1:init_index] <- as.list(unique_ids)
      if (verbose) {
        cli::cli_alert_info("Found unique identifiers: {.code {unique_ids}}")
      }
      if (!get_all) {
        ret_list <- store_checked_ids(checked_ids,
                                      possible_ids_list)
        return(ret_list)
      }

      # Remove unique identifiers from vars to reduce combinations
      #vars <- setdiff(vars, unique_ids)
      if (length(vars) == 0) {
        # All variables are unique identifiers
        ret_list <- store_checked_ids(checked_ids,
                                      possible_ids_list)
        return(ret_list)
      }
      unique_counts <- unique_counts[vars]
    }
  }

  # combinations -----------

  # Start testing combinations
  start_time   <- Sys.time()
  min_size     <- max(min_combination_size, 2)
  max_size     <- min(length(vars), max_combination_size)
  elapsed_time <- 0

  # where there is only one variable or not enough vars to combine
  if (min_size > max_size) {
    if (verbose) {
      cli::cli_alert_warning(
        "Can't make combinations of {.field {vars}} if the min number of
        combinations is {min_size} and the max is {max_size}")
    }

    cli::cli_abort("No unique identifier found.")
  }

  j <- init_index + 1
  for (comb_size in min_size:max_size) {

    # make sure length of vars is >= comb_size
    if (length(vars) < comb_size) {
      next
      # or break
    }

    combos <- utils::combn(vars, comb_size, simplify = FALSE)

    # Prune combinations where the product of unique counts is less
    # than n_rows
    combos_to_keep <- vapply(combos,
                             \(combo) {
                               prod(unique_counts[combo]) >= n_rows
                               },
                             logical(1))

    combos <- combos[combos_to_keep]

    # Estimate processing time and prune combinations
    est_times <- vapply(combos,
                        \(combo) {
                          estimate_combination_time(n_rows,
                                                    unique_counts[combo])
                          },
                        numeric(1))

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
        possible_ids_list[[j]] <- combo

        j <- j + 1

        if (j > max_numb_possible_ids) {
          if (verbose) {
            cli::cli_alert_warning(
            "Max number of possible IDs ({max_numb_possible_ids}) reached.
            You may modify it in argument {.arg max_numb_possible_ids}")
          }
          ret_list <- store_checked_ids(checked_ids = checked_ids,
                                        possible_ids = possible_ids_list)
          return(ret_list)
        }
        if (!get_all) {

          ret_list <- store_checked_ids(checked_ids = checked_ids,
                                        possible_ids = possible_ids_list)
          return(ret_list)

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
  }

  # ----------------------------- #
  # Return ####
  # ----------------------------- #

  ret_list <- store_checked_ids(checked_ids = checked_ids,
                                possible_ids = possible_ids_list)

  return(ret_list)
}


filter_by_class <- function(dt, vars, include_classes, exclude_classes) {
  # Compute the primary class of each variable
  vars_class <- vapply(dt, function(x) class(x)[1], character(1))
  names(vars_class) <- vars  # Ensure names are preserved

  # Apply 'include_classes' filter
  if (!is.null(include_classes)) {
    vars <- vars[vars_class[vars] %in% include_classes]
  }

  # Apply 'exclude_classes' filter
  if (!is.null(exclude_classes)) {
    vars <- vars[!(vars_class[vars] %in% exclude_classes)]
  }
  vars
}

filter_by_name <- function(vars, include, exclude, verbose) {
  # Apply 'exclude' filter
  if (!is.null(exclude)) {
    wno_exc <- which(!exclude %in% vars) # which not excluded
    if (length(wno_exc) > 0 & verbose) {
      no_exc <- exclude[wno_exc]
      cli::cli_alert_warning("var{?s} {.var {no_exc}} not found in dataframe")
    }
    vars <- setdiff(vars, exclude)
  }
  # Apply 'include' filter

  c(vars,
    setdiff(include, vars))

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


#' store checked variables as possible ids
#'
#' This function processes a list of possible IDs by removing any `NULL` entries,
#' storing a set of checked variables as an attribute and in the specified environment,
#' and then returning the updated list of possible IDs.
#'
#' @param checked_ids A vector of variable names that have been checked as possible IDs.
#' @param possible_ids A list containing potential identifiers. This list may contain `NULL` values, which will be removed by the function.
#' @param env An environment where the `checked_ids` will be stored. The default is `.joynenv`.
#'
#' @return A list of possible IDs with `NULL` values removed, and the `checked_ids` stored as an attribute.
#'
#'
#' @keywords internal
store_checked_ids <- function(checked_ids,
                              possible_ids,
                              env = .joynenv) {

  # Remove null from possible ids
  possible_ids <- remove_null(possible_ids)

  # Store checked_ids in environment
  rlang::env_poke(env   = env,
                  nm    = "checked_ids",
                  value = checked_ids)

  # Store attribute
  attr(possible_ids,
       "checked_ids") <- checked_ids

  # Return
  return(possible_ids)

}

#' Create variables that uniquely identify rows in a data table
#'
#' This function generates unique identifier columns for a given number of rows, based on the specified number of identifier variables.
#'
#' @param n_rows An integer specifying the number of rows in the data table for which unique identifiers need to be generated.
#' @param n_ids An integer specifying the number of identifiers to be created. If `n_ids` is 1, a simple sequence of unique IDs is created. If greater than 1, a combination of IDs is generated.
#' @param prefix A character string specifying the prefix for the identifier variable names (default is `"id"`).
#'
#' @return A named list where each element is a vector representing a unique identifier column. The number of elements in the list corresponds to the number of identifier variables (`n_ids`). The length of each element is equal to `n_rows`.
#'
#'
#' @keywords internal
create_ids <- function(n_rows, n_ids, prefix = "id") {

  vars <- vector("list",
                 n_ids)

  # If n_ids is 1, simply generate a sequence of IDs
  if (n_ids == 1) {
    vars[[1]] <- seq_len(n_rows)
    names(vars)[1] <- paste0(prefix, 1)

    return(vars)
  } else {

    # Get max unique values each variable can have
    max_vals <- ceiling(n_rows^(1 / n_ids))

    # Generate a sequence of unique identifiers

    all_ids <- expand.grid(rep(list(seq_len(max_vals)),
                               n_ids))

    #collapse::fnrow faster?

    if (fnrow(all_ids) > n_rows) {
      # Randomly sample the unique combinations
      all_ids <- all_ids[sample(fnrow(all_ids),
                                    n_rows), ]
    }

    # Store each unique identifier in the vars list
    for (i in seq_len(n_ids)) {
      vars[[i]] <- all_ids[[i]]
    }

    names(vars) <- paste0(prefix,
                          seq_len(n_ids))

    return(vars)
  }

}

