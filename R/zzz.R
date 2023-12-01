.onLoad <- function(libname, pkgname) {
  op <- options()
  op.joyn <- list(
    joyn.verbose         = TRUE,
    possible_ids.verbose = TRUE,
    joyn.reportvar       = ".joyn",
    joyn.suffixes        = c(".x", ".y")
  )
  toset <- !(names(op.joyn) %in% names(op))
  if(any(toset)) options(op.joyn[toset])

  invisible()
}
