#' Auxiliary function to select vars of data table
#' @param exclude character: Names of variables to exclude
#' @param include character: Name of variable to be included, that might belong
#'   to the group excluded in the `exclude`
#' @param exclude_classes character: classes to exclude from analysis (e.g.,
#'   "numeric", "integer", "date")
#' @param include_classes character: classes to include in the analysis (e.g.,
#'   "numeric", "integer", "date")
#' @return character vector of selected vars
#' @export
filter_vars <- function(dt,
                        vars = names(dt),
                        include = NULL,
                        exclude = NULL,
                        include_classes = NULL,
                        exclude_classes = NULL,
                        verbose = TRUE) {

  # Ensure dt is a data.table
  stopifnot(
    {is.data.table(dt)}
  )


  ## classes ----------
  vars <- filter_by_class(dt              = dt,
                          vars            = vars,
                          include_classes = include_classes,
                          exclude_classes = exclude_classes)

  ## var names --------
  vars <- filter_by_name(vars,
                         include,
                         exclude,
                         verbose)

  ##  no duplicated vars -------------
  if (anyDuplicated(vars)) {
    dupvars <- vars[duplicated(vars)] |>
      unique()
    cli::cli_abort("vars {.strongVar {dupvars}} are duplicated.")
  }

  if (length(vars) == 0) {
    if (verbose) {
      cli::cli_alert_danger("No variables available after applying
                            include/exclude filters.")
    }
    return(NULL) # should this be an error?
  }

  return(vars)
}


#' Find possible unique identifies of data frame
#'
#' Identify possible combinations of variables that  uniquely identifying dt
#'
#' @param dt data frame
#' @param vars character: A vector of variable names to consider for identifying unique combinations.
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
                         vars                        = names(dt),
                         verbose                     = getOption("possible_ids.verbose",
                                                                 default = FALSE),
                         min_combination_size        = 1,
                         max_combination_size        = 5,
                         max_processing_time         = 60, # in seconds
                         max_numb_possible_ids       = 100,
                         get_all                     = FALSE) {

  # defenses ---------
  # Capture explicitly defined arguments
  args <- as.list(environment())
  check_possible_ids(args)

  if (!is.data.table(dt)) {
    dt <- qDT(dt)
  }

  # Filter vars by class and name
  dt[, .SD, .SDcols = vars]

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
      vars <- setdiff(vars, unique_ids)

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

  if (min_size > max_size || length(vars) < min_size) {
    if (verbose) {
      cli::cli_alert_warning(
        "Can't make combinations of {.field {vars}} if the min number of
      combinations is {min_size} and the max is {max_size}")
    }

    if (length(possible_ids_list) > 0) {
      # Return the unique identifiers found so far
      ret_list <- store_checked_ids(checked_ids, possible_ids_list)
      return(ret_list)
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

