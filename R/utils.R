#' Rename to syntactically valid names
#'
#' @param name character: name to be coerced to syntactically valid name
#' @inheritParams joyn
#'
#' @return valid character name
#' @export
#'
#' @examples
#' joyn:::rename_to_valid("x y")
rename_to_valid <- function(name, verbose = getOption("joyn.verbose")) {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # computations   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  if (!is.character(name)) {
    cli::cli_abort("name {.var name} should be character")
  }
  nreportnames <- make.names(name)
  if (!identical(name, nreportnames) & isTRUE(verbose)) {

    cli::cli_alert_info("name {.field {name}} is an invalid variable name.
                        It will be changed to {.field {nreportnames}}",
                        wrap = TRUE)
  }
  return(nreportnames)
}

#' Split matching type
#'
#' Split matching type (one of `"1:1", "m:1", "1:m", "m:m"`) into its two components
#'
#' @inheritParams joyn
#'
#' @return character vector
#' @keywords internal
split_match_type <- function(match_type) {

  match_types <- c("1:1", "m:1", "1:m", "m:m")

  if (!match_type %in% match_types) {
    cli::cli_abort("invalid match type")
  }

  strsplit(match_type, ":", fixed = TRUE) |>
    unlist()

}


#' Is data frame balanced by group?
#'
#' Check if the data frame is balanced by group of columns, i.e., if it contains every combination of the elements in the specified variables
#'
#' @param df data frame
#' @param by  character: variables used to check if `df` is balanced
#' @param return character: either "logic" or "table". If "logic", returns `TRUE`
#'   or `FALSE` depending on whether data frame is balanced. If "table" returns the unbalanced
#'   observations - i.e. the combinations of elements in specified variables not found in input `df`
#'
#' @return logical, if return == "logic", else returns data frame of unbalanced observations
#' @export
#'
#' @examples
#' x1 = data.frame(id = c(1L, 1L, 2L, 3L, NA_integer_),
#'                 t  = c(1L, 2L, 1L, 2L, NA_integer_),
#'                 x  = 11:15)
#' is_balanced(df = x1,
#'             by = c("id", "t"),
#'             return = "table") # returns combination of elements in "id" and "t" not present in df
#' is_balanced(df = x1,
#'             by = c("id", "t"),
#'             return = "logic") # FALSE
is_balanced <- function(df,
                        by,
                        return = c("logic", "table")) {

  #_____________________________________________________
  # Arguments-------------------------------------------
  return <- match.arg(return)
  if (!is.character(by)) {
    cli::cli_abort("Argument `by` must be character vector")
  }
  if (!all(by %in% names(df))) {
    cli::cli_abort("`by` must give column names of `df`")

  }
  #_____________________________________________________
  # unique values of "by" ------------------------------
  lt_base <- lapply(by,
                    function(y){
                      df |>
                        get_vars(y) |>
                        funique() |>
                        na_omit() |>
                        reg_elem()
                    })

  #_____________________________________________________
  # name of list elements ------------------------------
  names(lt_base) <- by

  #_____________________________________________________
  # expand grid ----------------------------------------
  df_balanced <-
    expand.grid(lt_base,
                stringsAsFactors = FALSE) |>
    collapse::qDF()

  #_____________________________________________________
  #
  #unmatch  <- dt_balanced[!dt,  on = by]
  unmatch   <- collapse::join(
    x       = df_balanced,
    y       = df,
    how     = "anti",
    verbose = FALSE
  )
  balanced <- c(nrow(unmatch) == 0)

  if (return == "logic") {
    return(balanced)
  } else {
    setattr(unmatch, "is_balanced", balanced)
    return(unmatch)
  }

}

#' Unmask joyn's functions
#'
#'
#' @param fun_name character vector of one or more functions to unmask
#' @param pkg_name character specifying package from which joyn masks the function(s)
#' @return invisible TRUE
#' @export
#'
#' @examples
#' unmask_joyn_fun(fun_name = c("left_join", "right_join", "full_join"),
#'                 pkg_name = "dplyr")
#'
#' unmask_joyn_fun(fun_name = "inner_join",
#'                 pkg_name = "dplyr")
#'
unmask_joyn_fun <- function(fun_name,
                            pkg_name) {

  joyn:::clear_joynenv()

  lapply(fun_name,
         conflicted::conflict_prefer,
         winner = pkg_name,
         loser = "joyn",
         quiet = TRUE)

  joyn:::store_msg(type        = "info",
                  ok          = paste(cli::symbol$info, " Note:  "),
                  pale        = "function",
                  bolded_pale = "  {fun_name}",
                  pale        = "  unmasked.",
                  bolded_pale = " {pkg_name}::{fun_name}",
                  pale        = " preferred")
  joyn:::joyn_msg()

  invisible(TRUE)
}

# Trying out functions to avoid conflicted
# (!): the function used by set_collapse removes objs from the namespace,
#      thus I am following the same reasoning here

rm_from_namespace <- function(fun_name) {

  # get namespace exports
  exported_obj <- getNamespaceExports("joyn")

  # functions to remove -those specified by the user that are exported in joyn
  rmfun <- fun_name[fun_name %in% exported_obj]
  # get namespace of package
  nm_env <- rlang::ns_env("joyn")

  # MEMO: cannot remove bindings from a locked environment
  #       namespace environments are locked by R when loaded in a session:
  #       Once an environment is locked it cannot be unlocked ->
  #       --> cannot remove bindings BUT can modify existing bindings

  rlang::env_unlock(env = nm_env)

  #remove(rmfun, envir = asNamespace("joyn"))
  remove(rmfun,
         envir = nm_env)

}

# Another option: unlock a binding and modify it

unmask_aux_fun <- function(fun_name) {

  # get namespace exports
  exported_obj <- getNamespaceExports("joyn")

  # get functions to remove -those specified by the user that are exported in joyn
  remove_fun <- fun_name[fun_name %in% exported_obj]

  # Get the namespace environment of joyn
  joyn_ns <- rlang::ns_env("joyn")

  # unlock binding
  unlockBinding(fun_name, joyn_ns)

  # modify binding

  # Reassign 'left_join' to 'dplyr::join'
  #rlang::env_bind(lhs = left_join, rhs = dplyr::left_join, .env = joyn_ns)
  assign(left_join, dplyr::left_join, joyn_ns)

}

