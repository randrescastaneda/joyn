function(dt,  by, dtname = NULL) {
  m <- dt[, .N,
          by = mget(by)
  ][,
    mean(N)]

  if (m > 1) {
    msg     <- glue::glue("databse {.field {dtname}} is not uniquely identified
                          by {.code {by}}", wrap = TRUE)
    hint    <- "Check the join type, the key variables or the consistency of
    your data"
    rlang::abort(c(
      msg,
      i = hint,
      x = problem
    ),
    class = "joyn_error"
    )

  }

  return(invisible(TRUE))
}
"{glue::glue_collapse(glue::backtick(not_in_y),
                            sep = ', ', last ='  and ')} \\
                            not present in variable `y`"
