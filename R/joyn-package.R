#' @keywords internal
#' @aliases joyn-package
"_PACKAGE"

## usethis namespace: start
#' @rawNamespace import(collapse, except = fdroplevels)
#' @rawNamespace import(data.table, except = fdroplevels)
#' @importFrom lifecycle deprecated
## usethis namespace: end
.datatable.aware = TRUE

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
      ".xreport",
      ".yreport",
      'use_util_reportvar',
      'varx_na',
      'type',
      "joyn1"
    ),
    package = utils::packageName()
  )
}

NULL
