#' Merge two data frames
#'
#' This is a joyn wrapper that works in a similar fashion to [base::merge] and
#' [data.table::merge], which is why [joyn::merge] masks the other two.
#'
#' @inheritParams data.table::merge.data.table
#' @inheritDotParams joyn
merge <- function(x,
                  y,
                  by = NULL,
                  by.x = NULL,
                  by.y = NULL,
                  all = FALSE,
                  all.x = all,
                  all.y = all,
                  sort = TRUE,
                  suffixes = c(".x", ".y"),
                  no.dups = TRUE,
                  # default FALSE
                  allow.cartesian = getOption("datatable.allow.cartesian"),
                  ...) {

  # Check arguments ------------
  # this comes directly from data.table::merge.data.table()


  if (!sort %in% c(TRUE, FALSE))
    stopf("Argument 'sort' should be logical TRUE/FALSE")
  if (!no.dups %in% c(TRUE, FALSE))
    stopf("Argument 'no.dups' should be logical TRUE/FALSE")
  class_x = class(x)
  if (!is.data.table(y)) {
    y = as.data.table(y)
    if (missing(by) && missing(by.x)) {
      by = key(x)
    }
  }





  ## set up 'by'/'by.x'/'by.y'
  if ((!is.null(by.x) ||
       !is.null(by.y)) && length(by.x) != length(by.y))
    stopf("`by.x` and `by.y` must be of same length.")
  if (!missing(by) && !missing(by.x))
    warningf("Supplied both `by` and `by.x/by.y`. `by` argument will be ignored.")
  if (!is.null(by.x)) {
    if (length(by.x) == 0L ||
        !is.character(by.x) || !is.character(by.y))
      stopf("A non-empty vector of column names is required for `by.x` and `by.y`.")
    if (!all(by.x %chin% nm_x))
      stopf("Elements listed in `by.x` must be valid column names in x.")
    if (!all(by.y %chin% nm_y))
      stopf("Elements listed in `by.y` must be valid column names in y.")
    by = by.x
    names(by) = by.y
  } else {
    if (is.null(by))
      by = intersect(key(x), key(y))
    if (!length(by))
      # was is.null() before PR#5183  changed to !length()
      by = key(x)
    if (!length(by))
      by = intersect(nm_x, nm_y)
    if (length(by) == 0L || !is.character(by))
      stopf("A non-empty vector of column names for `by` is required.")
    if (!all(by %chin% intersect(nm_x, nm_y)))
      stopf("Elements listed in `by` must be valid column names in x and y")
    by = unname(by)
    by.x = by.y = by
  }

  # warn about unused arguments #2587
  if (length(list(...))) {
    ell = as.list(substitute(list(...)))[-1L]
    for (n in setdiff(names(ell), ""))
      warningf("Unknown argument '%s' has been passed.", n)
    unnamed_n = length(ell) - sum(names(ell) != "")
    if (unnamed_n)
      warningf("Passed %d unknown and unnamed arguments.", unnamed_n)
  }


}


