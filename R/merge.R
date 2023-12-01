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

  # Expand dots from merge.data.table ------------
  margs <- list(...)
  # mapply(assign, names(margs), margs)
  for (i in seq_along(margs)) {
    assign(names(margs)[i], margs[[i]])
  }


}


