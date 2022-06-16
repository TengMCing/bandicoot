# define_pkg_fns ----------------------------------------------------------

#' Load functions from package namespaces into current environment
#'
#' This function loads functions from package namespaces via `::` and
#' assigns them to the preferred function names in the current environment.
#'
#' Preferred function names can be provide via named arguments
#' like `info = cli_alert_info`.
#'
#' @param pkg Package.
#' @param ... Functions. Preferred names can be provide via named arguments.
#' @return No return value, called for side effects.
#'
#' @examples
#' define_pkg_fn(pkg = cli, cli_alert_info, cli_alert_warning)
#' define_pkg_fn(cli, cli_alert_warning, info = cli_alert_info)
#'
#' @export
define_pkg_fn <- function(pkg, ...) {

  # Capture function call
  fn_list <- as.list(sys.call())

  # Extract `...`, check how `pkg` is provided
  if (is.null(fn_list$pkg)) {
    pkg <- fn_list[[2]]
    fn_list[1:2] <- NULL
  } else {
    pkg <- fn_list$pkg
    fn_list$pkg <- NULL
    fn_list[[1]] <- NULL
  }

  # Check if `pref_names` are provided via named arguments and delete all "`"
  # A unnamed list will return NULL, a named list but without names will return empty strings
  pref_names <- names(fn_list)

  if (is.null(pref_names)) {
    pref_names <- as.character(fn_list)
  } else {
    pref_names[pref_names == ""] <- as.character(fn_list)[pref_names == ""]
  }
  pref_names <- gsub('`', '', pref_names)

  assign_list <- lapply(1:length(fn_list), function(i) {

    # Arguments in `...` must be symbols
    if (!is.symbol(fn_list[[i]]) && !is.character(fn_list[[i]])) stop("`", as.expression(fn_list[[i]]), "` is not a symbol or a character!")

    # `Base` an `sum` are not used, they are here to pass the CMD check
    eval(substitute(`::`(base, sum),
                    list(base = pkg, sum = fn_list[[i]])))
  })

  names(assign_list) <- pref_names
  list2env(assign_list, envir = parent.frame())

  return(invisible(NULL))
}





# bind_fn_2_env -----------------------------------------------------------

#' Bind functions of the current environment to a target environment
#'
#' This function is equivalent to `environment(fn) <- env`. Hence functions
#' must bind to names.
#'
#' Pass character function names to `...` will cause error.
#'
#' @param env Environment.
#' @param ... Functions.
#' @return No return value, called for side effects.
#'
#' @examples
#' # Access the associated environment inside a function
#'
#' self <- NULL
#' e <- new.env()
#'
#' # The associated environment needs to have a reference to itself
#' e$self <- e
#'
#' e$show_self <- function() return(self)
#'
#' # The function can only access the global variable `self`
#' e$show_self()
#'
#' # Bind the function to the environment `e`
#' bind_fn_2_env(env = e, e$show_self)
#'
#' # Both point to the same environment
#' e$show_self()
#' e
#'
#' @export
bind_fn_2_env <- function(env, ...) {

  # Capture function call
  fn_list <- as.list(sys.call())

  # `env` must be provided
  if (!is.environment(env)) stop("`env` is not an environment!")

  # Extract `...`, check how `env` is provided
  if (is.null(fn_list$env)) {
    fn_list[1:2] <- NULL
  } else {
    fn_list[[1]] <- NULL
    fn_list$env <- NULL
  }

  for (fn in fn_list) {

    # Arguments in `...` must be functions
    if (!is.function(eval(fn, envir = parent.frame()))) stop("`", as.expression(fn), "` is not a function!")

    # Change the function environment to the target environment
    eval(substitute(environment(fn) <- env), envir = parent.frame())
  }

  return(invisible(NULL))
}




# sub_fn_body_name --------------------------------------------------------

#' Substitute a symbol in a function body
#'
#' This function substitute all `old_names` with `new_names` in a function
#' body, **and drops all the attributes**.
#'
#' @param fn Function.
#' @param old_name Character. Name that needs to be replaced.
#' @param new_name Character. Replacement of the old name.
#' @return A function.
#'
#' @examples
#'
#' a <- function() self$x + self$y
#' a
#'
#' sub_fn_body_name(a, "self", "this")
#'
#' @seealso [body()]
#'
#' @export
sub_fn_body_name <- function(fn, old_name, new_name) {

  # Check if names are characters
  if (!is.character(old_name)) stop("`old_name` is not a string!")
  if (!is.character(new_name)) stop("`new_name` is not a string!")

  # Get the function body
  fn_body <- body(fn)
  fn_attr <- attributes(fn)


  # Substitute old names with new names
  assign(old_name, as.symbol(new_name), envir = environment())
  body(fn) <- do.call(substitute, list(expr = fn_body, env = environment()))

  return(fn)
}


# import_bandicoot --------------------------------------------------------


# Create names of functions that other packages may need to use

oop_dependencies <- c("register_method", "use_method", "copy_attr", "new_class", "bind_fn_2_env")

base_dependencies <- append(list("BASE", "base_"), oop_dependencies)

rand_var_dependencies <- append(list("RAND_VAR", "rand_var",
                                     "RAND_UNIFORM", "rand_uniform",
                                     "RAND_NORMAL", "rand_normal",
                                     "RAND_UNIFORM_D", "rand_uniform_d",
                                     "RAND_LOGNORMAL", "rand_lognormal"),
                                base_dependencies)

closed_form_dependencies <- append(list("CLOSED_FORM", "closed_form"), base_dependencies)
