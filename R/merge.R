#' Merge two data frames
#'
#' This is a joyn wrapper that works in a similar fashion to [base::merge] and
#' [data.table::merge], which is why [joyn::merge] masks the other two.
#'
merge <-
  function(x,
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
           allow.cartesian = getOption("datatable.allow.cartesian"),# default FALSE
           ...) {

  }
