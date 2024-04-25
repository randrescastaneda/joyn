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

#' Unmask joyn's functions ({conflicted} version)
#'
#'
#' @param fun_name character vector of one or more functions to unmask
#' @param pkg_name character specifying package from which joyn masks the function(s)
#' @return invisible TRUE
#' @keywords internal
#'
#'
# unmask_joyn_fun_conflicted <- function(fun_name,
#                                 pkg_name) {
#
#   clear_joynenv()
#
#   lapply(fun_name,
#          conflicted::conflict_prefer,
#          winner = pkg_name,
#          loser = "joyn",
#          quiet = TRUE)
#
#   store_msg(type        = "info",
#                   ok          = paste(cli::symbol$info, " Note:  "),
#                   pale        = "function",
#                   bolded_pale = "  {fun_name}",
#                   pale        = "  unmasked.",
#                   bolded_pale = " {pkg_name}::{fun_name}",
#                   pale        = " preferred")
#   joyn_msg()
#
#   invisible(TRUE)
# }


#' Unmask joyn's function(s) (local version, vectorized)
#'
#'
#' @param fun_name character vector of one or more functions to unmask
#' @param pkg_name character specifying package from which joyn masks the function(s)
#' @return invisible TRUE
#' @keywords internal -to be replaced with export once done
unmask_joyn_fun <- function(fun_name,
                            pkg_name) {

  # if package {pkg_name} is not loaded, stop and inform user

  if (!pkg_name %in% tolower(.packages())) {

    store_msg(type = "err",
              err  = paste(cli::symbol$cross, "Error:"),
              pale = "   package {pkg_name} must be loaded."
              )

      joyn_msg("err")
      cli::cli_abort("{pkg_name} is not loaded")
  }

  # if function {fun_name} is not an exported object of {pkg_name}, stop and inform user

  if (!any(fun_name %in% getNamespaceExports(pkg_name))) {

    store_msg(type = "err",
              err  = paste(cli::symbol$cross, "Error:"),
              pale = "   {fun_name} must be exported object(s) of {pkg_name}."
    )

    joyn_msg("err")
    cli::cli_abort("{fun_name} not exported from {pkg_name}")
  }

  # get namespace exports
  joyn_ns_exports <- getNamespaceExports("joyn")

  # get functions to unmask -filter those that are in joyn_ns exports
  fun_name <- fun_name[fun_name %in% joyn_ns_exports]

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # apply the new mask ----
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  lapply(fun_name, function(fn) {

    new_mask <- getExportedValue(ns   = getNamespace(pkg_name),
                                 name = fn)

    assign(x     = fn,
           value = new_mask,
           envir = .GlobalEnv)

  })

  #Inform the user
  clear_joynenv()

  store_msg(type        = "info",
            ok          = paste(cli::symbol$info, " Note:  "),
            pale        = "function",
            bolded_pale = "  {fun_name}",
            pale        = "  unmasked.",
            bolded_pale = " {pkg_name}::{fun_name}",
            pale        = " preferred")

  joyn_msg("info")

  invisible(TRUE)
}


#' Unmask joyn function (namespace version, NOT vectorized)
#'
#' @param fun_name character of function to unmask
#' @param pkg_name character specifying package from which joyn masks the function
#'
#' @return invisibly unmask function that joyn masks from another package
#' @keywords internal
unmask_joyn_fun_ns <- function(fun_name,
                               pkg_name) {

  # Checks ####

  # if package {pkg_name} is not loaded, stop and inform user
  if (!pkg_name %in% tolower(.packages())) {

    store_msg(type = "err",
              err  = paste(cli::symbol$cross, "Error:"),
              pale = "   package {pkg_name} must be loaded."
    )

    joyn_msg("err")
    cli::cli_abort("{pkg_name} is not loaded")
  }

  # if function {fun_name} is not an exported object of {pkg_name}, stop and inform user

  if (!any(fun_name %in% getNamespaceExports(pkg_name))) {

    store_msg(type = "err",
              err  = paste(cli::symbol$cross, "Error:"),
              pale = "   {fun_name} must be exported object(s) of {pkg_name}."
    )

    joyn_msg("err")
    cli::cli_abort("{fun_name} not exported from {pkg_name}")
  }

  # get joyn namespace
  joyn_ns <- getNamespace("joyn")

  # unlock binding
  unlockBinding(fun_name, env = joyn_ns)

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Unmask functions ----
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  # get namespace exports
  joyn_ns_exports_names <- getNamespaceExports(joyn_ns)

  # get functions to unmask -filter those that are in joyn_ns exports
  fun_name <- fun_name[fun_name %in% joyn_ns_exports_names]

  # get joyn's namespace exports' environment
  joyn_ns_exports <- .getNamespaceInfo(joyn_ns,
                                       "exports")

  # debug lines to be removed ----
  print("EXPORTS BEFORE")
  print(getNamespaceExports(joyn_ns))

  # remove binding from joyn's namespace exports' environment
  remove(list = fun_name,
         envir = joyn_ns_exports)

  joyn_ns <- getNamespace("joyn")

  # debug lines to be removed ----
  print("EXPORTS AFTER `remove`")
  print(getNamespaceExports(joyn_ns))

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Apply the new mask ----
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  new_mask <- getExportedValue(ns = getNamespace(pkg_name), name = fun_name)

  rlang::env_poke(env    = joyn_ns,
                  nm     = fun_name,
                  value  = new_mask,
                  create = TRUE)


  # Lock the binding again
  lockEnvironment(joyn_ns,
                  bindings = TRUE)

  # Detach and reattach "joyn" if currently loaded

  if(anyv(search(), "package:joyn")) {   #anyv is the collapse version of any()
    detach_package(joyn)
    suppressPackageStartupMessages(attachNamespace(joyn_ns))
  }

  # Inform the user
  clear_joynenv()

  store_msg(type        = "info",
            ok          = paste(cli::symbol$info, " Note:  "),
            pale        = "function",
            bolded_pale = "  {fun_name}",
            pale        = "  unmasked.",
            bolded_pale = " {pkg_name}::{fun_name}",
            pale        = " preferred")

  joyn_msg()

  # return
  invisible(TRUE)

}


#' Detach a package from search list
#'
#' This is an auxiliary function to avoid errors when detaching a package
#' -e.g., when multiple versions are loaded at the same time
#'
#' @param pkg name of the package to detach
#' @param character logical. TRUE when pkg_name provided as a character string, FALSE otherwise; Default to FALSE
#' @return invisibly detach package from search list
#' @keywords internal
#'
detach_package <- function(pkg_name) {

  search_item <- paste("package", pkg, sep = ":")

  if(search_item %in% search()) {

    detach(search_item,
           unload = TRUE,
           character = TRUE)

  }
}
