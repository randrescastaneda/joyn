.onLoad <- function(libname, pkgname) {
  op <- options()
  op.joyn <- list(
    joyn.verbose         = TRUE,
    possible_ids.verbose = TRUE,
    joyn.reportvar       = ".joyn",
    joyn.suffixes        = c(".x", ".y"),
    joyn.match_type      = c("1:1", "1:m", "m:1", "m:m"),
    joyn.na.last         = FALSE
  )
  toset <- !(names(op.joyn) %in% names(op))

  #store them in .joynenv
  rlang::env_bind(.joynenv, op.joyn = op.joyn)

  if(any(toset)) {
    options(op.joyn[toset])
  }

  #get_joyn_options()

  invisible()
}


#' Get joyn options
#'
#' This function aims to display and store info on joyn options
#'
#' @param env environment, which is joyn environment by default
#' @param display logical, if TRUE displays(print) info on joyn option name(s) and
#'    corresponding default and current values
#' @param option character or NULL. If character, name of a specific joyn option.
#'    If NULL, all joyn options
#' @return invisibly as a list
#'
#' @keywords internal
get_joyn_options <- function(env = .joynenv,
                             display = TRUE,
                             option = NULL) {

  # joyn options
  op.joyn <- rlang::env_get(env, "op.joyn")

  if(is.null(option)) {
    # get all possible options
    op.joyn <- rlang::env_get(env, "op.joyn")

  } else {
    # get specified option
    op.joyn <- rlang::env_get(env, "op.joyn")[option]
  }

  if (display == TRUE) {
    options_info <- sapply(names(op.joyn), function(opt) {

      default_value <- op.joyn[[opt]]
      current_value <- getOption(opt)
      sprintf("%-20s default = %-20s > current = %s",
              opt,
              toString(default_value),
              toString(current_value))
    },
    USE.NAMES = FALSE)


    # print and display options
    cat("\nJoyn Options:\n")
    cat("---------------------------------------------------------------------\n")
    cat(paste(options_info, collapse = "\n"), "\n")
    cat("---------------------------------------------------------------------\n")

  }

  invisible(op.joyn)
}

# Set joyn options localy
set_joyn_options <- function(option,
                             value) {
  # check option name

  # set value
}


