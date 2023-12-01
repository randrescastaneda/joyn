.onLoad <- function(libname, pkgname) {
  op <- options()
  op.joyn <- list(
    joyn.verbose         = TRUE,
    possible_ids.verbose = TRUE,
    joyn.reportvar       = ".joyn",
    joyn.suffixes        = c(".x", ".y"),
    joyn.match_type      = c("1:1", "m:1", "1:m", "m:m")
  )
  toset <- !(names(op.joyn) %in% names(op))
  if(any(toset)) options(op.joyn[toset])

  invisible()
}
