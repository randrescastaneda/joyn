#' display type of joyn message
#'
#' @param type character: one or more of the following:
#' `r cli::format_inline("{.or {.val {type_choices()}}}")` or `all`
#'
#' @return returns data frame with message invisibly. print message in console
#' @export
#'
#' @examples
#' # Storing msg with type "info"
#' joyn:::store_msg("info",
#'   ok = cli::symbol$tick, "  ",
#'   pale = "This is an info message")
#'
#' # Storing msg with type "warn"
#' joyn:::store_msg("warn",
#'   err = cli::symbol$cross, "  ",
#'   note = "This is a warning message")
#'
#' joyn_msg("all")

joyn_msg <- function(type = c("all", type_choices()),
                     msg  = NULL) {

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
  # cat(dt[["msg"]], "\n", sep = "\n")
  l <- lapply(dt[["msg"]], \(.) {
    cli::cli_text(.)
  })

  if ("err" %in% type_to_use & is.character(msg)) {
    cli::cli_abort(msg)
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Return   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  return(invisible(dt))
}


#' Store joyn message to .joynenv environment
#'
#' @inheritParams joyn_msg
#' @param ... combination of type and text in the form `style1 = text1, style2 =
#'   text2`, etc.
#'
#' @return current message data frame invisibly
#' @keywords internal
store_msg <- function(type, ...) {

  # check input ----------
  type <- match.arg(type, choices = type_choices())
  check_style(...)

  # style type in dt form -----------
  style_args <- list(...) |>
    lapply(\(x){
      cli::format_inline(x, .envir = parent.frame(3))
    })

  type_dt_args <- append(list(type = type), style_args)
  dt_msg <-  do.call(msg_type_dt, type_dt_args)


  # create new messages   ---------
  if (rlang::env_has(.joynenv, "joyn_msgs")) {
    dt_old_msgs <- rlang::env_get(.joynenv, "joyn_msgs")
    dt_new_msgs <- rowbind(dt_old_msgs, dt_msg)
  } else {
    dt_new_msgs <- dt_msg
  }

  dt_new_msgs <- funique(dt_new_msgs)

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

type_choices <- \(){
  rlang::env_get(.joynenv, "msg_type_choices")
}


#' convert style of joyn message to data frame containing type and message
#'
#' @inheritParams joyn_msg
#'
#' @return data frame with two variables, type and msg
#' @keywords internal
msg_type_dt <- \(type, ...) {
  c(type = type, msg = style(...)) |>  # named vector
    as.list() |>  # convert to list to pass as data.frame
    data.frame() # convert
}


#' style of text displayed
#'
#' This is an adaptation from
#' https://github.com/r-lib/pkgbuild/blob/3ba537ab8a6ac07d3fe11c17543677d2a0786be6/R/styles.R
#' @param ... combination of type and text in the form
#' `type1 = text1, type2 = text2`
#' @param sep a character string to separate the terms to [paste]
#'
#' @return formatted text
#' @keywords internal
style <- function(..., sep = "") {
  args <- list(...)
  if (is.null(names(args))) {
    names(args) <- rep("", length(args))
  }

  styles <- list(
    "ok"     = cli::make_ansi_style("#228B22"),

    "note"   =  \(x) {
      cli::make_ansi_style("#228B22")(x) |>
        cli::style_bold()
      },

    "warn"   = \(x) {
      cli::make_ansi_style("#FE5A1D")(x) |>
      cli::style_bold()
      },

    "err"    = \(x) {
      cli::make_ansi_style("#CE2029")(x) |>
      cli::style_bold()
      },

    "pale"   = cli::make_ansi_style("darkgrey"),

    "bolded_pale" = \(x) {
      cli::make_ansi_style("#555555")(x) |>
      cli::style_bold() |>
      cli::style_italic()
      },

    "timing" = \(x) {
      cli::make_ansi_style("#007FFF")(x) |>
      cli::style_bold()

      }
    )


  nms      <- names(args)
  nms2     <- nms[!nms %in% ""]
  nmstyles <- names(styles)

  if (any(!nms2 %in% nmstyles)) {
    no_names <- which(!nms2 %in% nmstyles)
    cli::cli_abort(c("{.val {nms2[no_names]}} {?is/are} not valid style{?s}.",
                     "i" = "Available styles are {.field {nmstyles}}"))
  }

  x <- lapply(seq_along(args), \(i) {
    if (nzchar(nms[i])) {

      styles[[nms[i]]](args[[i]])

    } else {

      args[[i]]

    }
  })

  paste(unlist(x), collapse = sep)
}

#' Presence of joyn msgs in the environment
#'
#' Checks the presence of joyn messages stored in joyn environment
#'
#' @return invisible TRUE
#' @keywords internal
#' @examples
#' \dontrun{
#' Storing a message
#' joyn:::store_msg("info", "simple message")
#' Checking if it exists in the environment
#' print(joyn:::joyn_msgs_exist())
#' }
joyn_msgs_exist <- \() {
  if (!rlang::env_has(.joynenv, "joyn_msgs")) {
    cli::cli_abort(c("no messages stored in .joynenv",
                     "i" = "make sure that joyn has been executed at least once"))
  }
  invisible(TRUE)
}


#' Clearing joyn environment
#' @keywords internal
#' @examples
#' \dontrun{
#' # Storing a message
#' joyn:::store_msg("info", "simple message")
#'
#' # Clearing the environment
#' joyn:::clear_joynenv()
#'
#' # Checking it does not exist in the environment
#' print(joyn:::joyn_msgs_exist())
#' }
clear_joynenv <- \(){
  # get the source function
  .joyn_source  <- sys.call(-1)[[1]]

  # get what function call clear_joynenv before
  first_source <- rlang::env_get(.joynenv, "joyn_source", default = NULL)

  # if the first function was joyn or it is null, then clear everything
  if (first_source == "joyn" || is.null(first_source) || is.symbol(first_source)) {
    rlang::env_unbind(.joynenv, "joyn_msgs")
    rlang::env_unbind(.joynenv, "joyn_source")
  }

  # replace the source with the current call
  rlang::env_poke(.joynenv, "joyn_source", .joyn_source)
  invisible(.joyn_source)
}

