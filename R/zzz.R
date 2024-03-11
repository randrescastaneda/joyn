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

get_joyn_options_v0 <- function() {
  op.joyn <- env_get(.joynenv, "op.joyn")

  # Show default values of all possible options
  defaults <- lapply(names(op.joyn), function(opt) {
    list(option = opt, default_value = op.joyn[[opt]])
  })

  # Show current values of all possible options
  current_values <- lapply(names(op.joyn), function(opt) {
    list(option = opt, current_values = getOption(opt))
  })

  # See and print
  cat("Joyn Options:\n")
  #cat(sprintf, c("%s: Default=%s, Current=%s\n", defaults, current_values))
}

# Display all possible option in joyn -their default values and current values
get_joyn_options <- function(env = .joynenv,
                             display = TRUE) {

  # joyn options
  op.joyn <- rlang::env_get(env, "op.joyn")

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
