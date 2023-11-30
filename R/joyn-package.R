#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @import data.table
#' @import collapse
## usethis namespace: end
# .datatable.aware = TRUE

# Prevent R CMD check from complaining about the use of pipe expressions
# standard data.table variables
if (getRversion() >= "2.15.1") {
  utils::globalVariables(
    names = c(
      ".",
      ".I",
      ".N",
      ".SD",
      ".",
      "!!",
      ":=",
      "..output",
      "x_report",
      "y_report"
    ),
    package = utils::packageName()
  )
}

NULL
