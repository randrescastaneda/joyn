#' display type of joyn message
#'
#' @param msg_type character: one or more of the following:
#' `r cli::format_inline("{.or {c('all', 'basic', type_choices())}}")`
#' @param msg character vector to be parsed to [cli::cli_abort()]. Default is
#'   NULL. It only works if `"err" %in% msg_type`. This is an internal argument.
#'
#' @family messages
#'
#' @return returns data frame with message invisibly. print message in console
#' @export
#'
#' @examples
#' library(data.table)
#' x1 = data.table(id = c(1L, 1L, 2L, 3L, NA_integer_),
#' t  = c(1L, 2L, 1L, 2L, NA_integer_),
#' x  = 11:15)
#'
#' y1 = data.table(id = 1:2,
#'                 y  = c(11L, 15L))
#' df <- joyn(x1, y1, match_type = "m:1")
#' joyn_msg("basic")
#' joyn_msg("all")
joyn_msg <- function(msg_type = getOption("joyn.msg_type"),
                     msg  = NULL) {

  # Check ---------
  type_to_use <- match.arg(arg = msg_type,
                           choices = c("all", "basic", type_choices()),
                           several.ok = TRUE)
  joyn_msgs_exist()

  # get msgs ---------
  dt <- rlang::env_get(.joynenv, "joyn_msgs") |>
    roworder(type)

  dt <-
    if (!any(c("all", "basic") %in% type_to_use)) {
      dt |>
        fsubset(type %in% type_to_use)
    } else if ("basic" %in% type_to_use) {
      dt |>
        fsubset(type %in% c("info", "note", "warn"))
    } else {
      dt
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
#' @family messages
#'
#' @return current message data frame invisibly
#' @keywords internal
#' @examples
#' # Storing msg with msg_type "info"
#' joyn:::store_msg("info",
#'   ok = cli::symbol$tick, "  ",
#'   pale = "This is an info message")
#'
#' # Storing msg with msg_type "warn"
#' joyn:::store_msg("warn",
#'   err = cli::symbol$cross, "  ",
#'   note = "This is a warning message")
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

# Wrapper for store_msgs
# create_joyn_msgs

store_joyn_msg <- function(err       = NULL,
                           warn      = NULL,
                           timing    = NULL,
                           info      = NULL) {

  # Check that only one among err, warn, timing and info is not null, otherwise stop

  # Error messages -----------------------------------------

  if (!is.null(err)) {

    store_msg("err",
              err  = paste(cli::symbol$cross, "Error: "),
              pale = err)

  }

  # Warning messages -----------------------------------------

  else if (!is.null(warn)) {

    store_msg("warn",
              warn = paste(cli::symbol$warn, "Warning: "),
              pale = warn)
  }

  # Timing messages -----------------------------------------

  else if (!is.null(timing)) {

    # detect number
    num_pattern <- "[0-9]+\\.?[0-9]*"

    timing_num <- regmatches(timing,
                             gregexpr(num_pattern, timing))

    store_msg(type    = "timing",
              timing  = paste(cli::symbol$record, "  Timing:"),
              pale    = sub(timing_num, "", timing),
              timing  = timing_num,
              pale    = " seconds.")

  }

  # Info messages -----------------------------------------

  else if (!is.null(info)) {

    store_msg(type = "info",
              ok   = paste(cli::symbol$info, " Note:  "),
              pale = info)

  }

  invisible(TRUE)
}

check_style <- \(...) {
  if (length(list(...)) == 0) {
    cli::cli_abort(c("no style provided",
                     "i" = "check list of styles in {.fun style}"))
  }
  invisible(TRUE)
}

#' Choice of messages
#'
#' @return character vector with choices of types
#' @family messages
#' @keywords internal
type_choices <- \(){
  rlang::env_get(.joynenv, "msg_type_choices")
}


#' convert style of joyn message to data frame containing type and message
#'
#' @inheritParams joyn_msg
#' @family messages
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
#' @family messages
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
#' @family messages
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
#' @family messages
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





#' Print JOYn report table
#'
#' @inheritParams joyn
#' @family messages
#' @return invisible table of frequencies
#' @export
#'
#' @examples
#' library(data.table)
#' x1 = data.table(id = c(1L, 1L, 2L, 3L, NA_integer_),
#' t  = c(1L, 2L, 1L, 2L, NA_integer_),
#' x  = 11:15)
#'
#' y1 = data.table(id = 1:2,
#'                 y  = c(11L, 15L))
#'
#' d <- joyn(x1, y1, match_type = "m:1")
#' joyn_report(verbose = TRUE)
joyn_report <- function(verbose = getOption("joyn.verbose")) {
  if (!rlang::env_has(.joynenv, "freq_joyn")) {
    cli::cli_abort(c("no frequencies table stored in {.field .joynenv}",
                     "i" = "make sure that joyn has been
                     executed at least once"))
  }

  freq <- rlang::env_get(.joynenv, "freq_joyn")
  if (verbose) {
    print(freq)
  }
  return(invisible(freq))
}
