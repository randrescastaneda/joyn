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

  #store it in .joynenv
  rlang::env_bind(.joynenv, op.joyn = op.joyn)

  if(any(toset)) {
    options(op.joyn[toset])
  }

  #get_joyn_options()

  invisible()
}

get_joyn_options <- function() {

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
  cat(do.call(sprintf, c("%s: Default=%s, Current=%s\n", defaults, current_values)))
}
