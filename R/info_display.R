#' display type of joyn message
#'
#' @param type character: one or more of `c("all", "info", "note", "warn")`
#'
#' @return returns data frame with message invisibly. print message in console
#' @export
#'
#' @examples
#' joyn:::store_msg("info", ok = cli::symbol$tick, "  ", pale = "This is an info message")
#' joyn:::store_msg("warn", err = cli::symbol$cross, "  ", note = "This is a warning message")
#' joyn_msg("all")
joyn_msg <- function(type = c("all", "info", "note", "warn")) {

  # Check ---------
  type_to_use <- match.arg(type, several.ok = TRUE)
  joyn_msgs_exist()

  # get msgs ---------
  dt <- rlang::env_get(.joynenv, "joyn_msgs")

  if ("all" %!in% type_to_use) {
    dt <- dt |>
      fsubset(type %in% type_to_use)
  }

  # display results --------
  cat(dt[["msg"]], "\n", sep = "\n")


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Return   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  return(invisible(dt))
}


#' Store joyn message to .joynenv environment
#'
#' @param type character: type of message
#' @param ... combination of type and text in the form `type1 = text1, type2 =
#'   text2`, etc.
#'
#' @return current message data frame invisibly
#' @keywords internal
store_msg <- function(type, ...) {

  # check input ----------
  type <- match.arg(type, choices = c("info", "note", "warn"))
  check_style(...)

  # style type in dt form -----------
  dt_msg <-  msg_type_dt(type, ...)

  # create new messages   ---------
  if (rlang::env_has(.joynenv, "joyn_msgs")) {
    dt_old_msgs <- rlang::env_get(.joynenv, "joyn_msgs")
    dt_new_msgs <- rowbind(dt_old_msgs, dt_msg)
  } else {
    dt_new_msgs <- dt_msg
  }

  # store in env ------
  rlang::env_poke(.joynenv, "joyn_msgs", dt_new_msgs)


  # Return   ---------
  return(invisible(dt_new_msgs))

}

check_style <- \(...) {
  if (length(list(...)) == 0) {
    cli::cli_abort(c("no style provided",
                     "i" = "check list of styles in {.fun style}"))
  }
  invisible(TRUE)
}

#' convert style to data frame
#'
#' @inheritParams joyn_msg
#'
#' @return data frame
#' @keywords internal
msg_type_dt <- \(type, ...) {
  c(type = type, msg = style(...)) |>  # named vector
    as.list() |>  # convert to list to pass as data.frame
    data.frame() # convert
}


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
    "warn"   = \(x) {
      cli::make_ansi_style("#cc6677")(x) |>
      cli::style_bold()
      },
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


joyn_msgs_exist <- \() {
  if (!rlang::env_has(.joynenv, "joyn_msgs")) {
    cli::cli_abort(c("no messages stored in .joynenv",
                     "i" = "make sure that joyn has been executed at least once"))
  }
  invisible(TRUE)
}
