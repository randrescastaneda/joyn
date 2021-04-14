# Add global variables to avoid NSE notes in R CMD check
if (getRversion() >= '2.15.1')
  utils::globalVariables(
    c('N')
  )

#' Make sure the match type is correct
#'
#' @param dt either right of left table
#' @param by by argument in merge
#' @param dtname name of dt for displaying purposes
#'
#' @return TRUE
#' @noRd
join_consistency <- function(dt,  by, dtname = NULL) {
  m <- dt[, .N,by = mget(by)
          ][,
            mean(N)
            ]

  if (m > 1) {
    msg     <- glue::glue(
      "databse {dtname} is not uniquely identified by {glue::glue_collapse(glue::backtick(by),
                          sep = ', ', last ='  and ')}")
    hint    <- "Check the match type, the key variables or the consistency of
    your data"
    rlang::abort(c(
      msg,
      i = hint
    ),
    class = "joyn_error"
    )

  }

  return(invisible(TRUE))
}
