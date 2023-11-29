

#' style of text displayed
#'
#' This is from
#' https://github.com/r-lib/pkgbuild/blob/3ba537ab8a6ac07d3fe11c17543677d2a0786be6/R/styles.R
#' @param ... combination of type and text in the form `type1 = text1, type2 = text2`
#' @param sep a character string to separate the terms to [paste]
#'
#' @return formated text
#' @keywords internal
style <- function(..., sep = "") {
  args <- list(...)
  # st <- names(args)

  styles <- list(
    "ok"     = cli::col_green,
    "note"   = cli::make_ansi_style("orange"),
    "warn"   = \(x) cli::style_bold(cli::make_ansi_style("orange")(x)),
    "err"    = cli::col_red,
    "pale"   = cli::make_ansi_style("darkgrey"),
    "timing" = cli::make_ansi_style("cyan")
  )

  nms <- names(args)
  x <- lapply(seq_along(args), \(i) {
    if (nzchar(nms[i])) {

      styles[[nms[i]]](args[[i]])

    } else {

      args[[i]]

    }
  })

  paste(unlist(x), collapse = sep)
}

