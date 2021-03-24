# from glue package: https://glue.tidyverse.org/articles/transformers.html
collapse_transformer <- function(regex = "[*]$", ...) {
  function(text, envir) {
    collapse <- grepl(regex, text)
    if (collapse) {
      text <- sub(regex, "", text)
    }
    res <- glue::identity_transformer(text, envir)
    if (collapse) {
      glue_collapse(res, ...)
    } else {
      res
    }
  }
}


