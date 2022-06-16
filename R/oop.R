# register_method ---------------------------------------------------------

#' Register method for an object environment
#'
#' This function register a function as a method of an object environment.
#'
#' Methods will be executed inside a container, which is a child
#' environment of the parent of the object environment. Thus, methods can not
#' access variables of the object environment directly, but can access
#' variables of the parent of the object environment directly. The designed
#' way for methods to access the object environment is by using the name
#' "self", this name can be changed by specifying a string in `self_name`.
#' The default name of the container is "..method_env..". This also can be changed
#' by specifying a string in `container_name`. An object can have multiple
#' containers, but every container is recommended to contain only one self
#' reference.
#' \cr
#' \cr
#' Method needs to be provided as `a = function() 1`, where `a` is the name of
#' the method and the right hand side of the equal sign is the function.
#' Warning will be raised if the container contains contents other than the
#' self reference.
#'
#' @param env Environment. Object environment.
#' @param ... Named Functions. Functions needs to be provided in named format,
#' like `a = function() 1`.
#' @param container_name Character. Name of the container. Methods will be
#' executed inside this container.
#' @param self_name Character. Name of the self reference. Methods needs to use
#' this name to access the object environment.
#' @return Return the object itself.
#'
#' @examples
#'
#' a <- function() self$x
#'
#' e <- new.env()
#' e$x <- 1
#'
#' # Register the method `aa` for environment `e` with `self_name = "self"`
#' register_method(e, aa = a, self_name = "self")
#'
#' # There is an environment `..method_env..` in the environment `e`
#' names(e)
#'
#' # The container is empty (except `self`)
#' names(e$..method_env..)
#'
#' # `self` is a reference to `e`
#' identical(e, e$..method_env..$self)
#'
#' # The method `aa` will be evaluated in the container
#' identical(environment(e$aa), e$..method_env..)
#'
#' # Therefore, `self$x` is a reference to variable `x` of the environment `e`
#' e$aa()
#'
#' @export
register_method <- function(env, ..., container_name = "..method_env..", self_name = "self") {

  # Capture function call
  fn_list <- as.list(sys.call())

  # `env` must be an environment
  if (!is.environment(env)) stop("`env` is not an environment!")

  # `container_name` and `self_name` must be provided as characters
  if (!is.character(container_name)) stop("`container_name` must be characters!")
  if (!is.character(self_name)) stop("`self_name` must be characters!")

  # Check if container exists
  if (container_name %in% names(env)) {

    # Check if the container is an environment
    if (!is.environment(env[[container_name]])) stop(container_name, " exists, but it is not an environment! Consider remove it.")

    # Check if the container is empty (except self)
    if (sum(names(env[[container_name]]) != self_name) > 0) warning("The container is not empty!")

    # Check if the container is the child of the parent of the instance environment
    if (!identical(parent.env(env[[container_name]]), parent.env(env))) stop(container_name, " exists, but it is not a child of the parent of the instance environment! Consider remove it.")

    # Check if self exists
    if (self_name %in% names(env[[container_name]])) {

      # Check if self points to the instance environment
      if (!identical(env[[container_name]][[self_name]], env)) stop(self_name, " exists, but it is not the same as the provided environment! Consider remove it.")

    } else {

      # Otherwise, create self
      env[[container_name]][[self_name]] <- env
    }

  } else {

    # Otherwise, create the container and self
    env[[container_name]] <- new.env(parent = parent.env(env))
    env[[container_name]][[self_name]] <- env
  }

  # Extract `...`, check how `env` is provided
  if (is.null(fn_list$env)) {
    fn_list[1:2] <- NULL
  } else {
    fn_list[[1]] <- NULL
    fn_list$env <- NULL
  }

  fn_list$container_name <- NULL
  fn_list$self_name <- NULL

  # Check if `fn_names` are provided via named arguments and delete all "`"
  # A unnamed list will return NULL, a named list but without names will return empty strings
  fn_names <- names(fn_list)

  if (is.null(fn_names) || "" %in% fn_names) stop("All methods should have a name.")
  fn_names <- gsub('`', '', fn_names)

  for (i in 1:length(fn_list)) {

    # Eval the function in the parent frame
    eval_fn <- eval(fn_list[[i]], envir = parent.frame())

    # Check whether it is a function
    if (!is.function(eval_fn)) stop("`", as.expression(fn_list[[i]]), "` is not a function!")

    # Bind it to the container of the instance environment
    env[[fn_names[i]]] <- eval_fn
    bind_fn_2_env(env[[container_name]], env[[fn_names[i]]])
  }

  return(env)
}

# print.bandicoot_oop -----------------------------------------------------

#' S3 method of printing `bandicoot_oop` object
#'
#' This function print the string representation of the object.
#'
#' @param x `bandicoot_oop` object.
#' @param ... ignored.
#' @return No return value, called for side effects.
#' @export
print.bandicoot_oop <- function(x, ...) {
  if ("..str.." %in% names(x)) {
    cli::cli_h3(x$..str..())
  } else {
    cli::cli_h3("<unknown object>")
  }

  return(invisible(NULL))
}


# new_class ---------------------------------------------------------------

#' Define a new class
#'
#' This function declare a new class, and copies attributes and methods from
#' parent classes.
#'
#' Parents can be provided in `...`, where methods and attributes will be
#' overrided by the left classes. If `...` is empty and `empty_class == FALSE`,
#' [BASE] will be used as the parent class.
#'
#' @param ... Environments. Parent class environments.
#' @param env Environment. The new class environment.
#' @param class_name Name of the new class.
#' @param empty_class Boolean. Whether to create an empty class. This should only
#' be used when you don't want to inherited from [BASE], or you want to define
#' your own base object class. Will be ignored if `...` is not empty. If `...`
#' is empty and `empty_class == FALSE`, [BASE] will be used as the parent class.
#' @return A class environment with S3 class "bandicoot_oop".
#'
#' @examples
#' MYCLASS <- new_class(class_name = "MYCLASS")
#' MYCLASS
#' names(MYCLASS)
#'
#' # Inhert from BASE class
#' TEST <- new_class(BASE, class_name = "TEST")
#' TEST
#' names(TEST)
#'
#' @export
new_class <- function(..., env = new.env(parent = parent.frame()), class_name = NULL, empty_class = FALSE) {

  # Class should has a name
  if (is.null(class_name)) stop("`class_name` is null!")

  env$..class.. <- c()

  # If users leave ... empty and they don't want to define an empty class
  if ((!empty_class) && (length(list(...)) == 0)) {
    parent_cls_list <- list(BASE)
  } else {
    parent_cls_list <- list(...)
  }

  # Methods will be overrided by the left classes
  for (parent in rev(parent_cls_list)) {

    if (parent$..instantiated..) stop("Parent is not a class!")

    env$..class.. <- c(parent$..class.., env$..class..)

    # Copy all the methods and attributes from the class/instance
    # except the container, the init_call, and the class information
    copy_attr(env, parent, avoid = c("..method_env..",
                                     "..init_call..",
                                     "..class..",
                                     "..type..",
                                     "..instantiated.."))
  }

  env$..class.. <- c(class_name, env$..class..)
  env$..type.. <- class_name
  env$..instantiated.. <- FALSE

  # Set S3 class
  class(env) <- "bandicoot_oop"

  # Return the class
  return(env)
}


# copy_attr ---------------------------------------------------------------

#' Copy attributes and methods from classes or instances
#'
#' This function copy attributes and methods from classes or instances to
#' class or instance.
#'
#' Multiple classes or instances can be provided in `...`, where the right one
#' will override the left one if they have the same attribute or method name.
#' Attributes or methods that don't want to be copied can be specified in
#' `avoid`.
#'
#' @param env Environment. The destination environment.
#' @param ... Environments. Source environments.
#' @param avoid Character. Names that don't want to be copied.
#' @return Return the object itself.
#'
#' @examples
#'
#' test <- new.env()
#' names(BASE)
#' copy_attr(test, BASE, avoid = c("..method_env..", "..init_call..", "..dict.."))
#' names(test)
#'
#' @export
copy_attr <- function(env, ..., avoid = c("..method_env..", "..init_call..")) {

  if (!is.environment(env)) stop("`env` is not an environment!")

  # Get list of classes
  class_list <- list(...)

  if (length(class_list) == 0) stop("At least provide one source to copy from!")

  for (cls in class_list) {

    method_list <- list()
    attr_list <- list()

    method_names <- names(cls)[unlist(lapply(names(cls), function(x) is.function(cls[[x]])))]

    # Get list of methods
    for (method_name in method_names) {
      if (method_name %in% avoid) next
      method_list[[method_name]] <- cls[[method_name]]
    }

    # Get list of attributes
    for (attr_name in setdiff(names(cls), method_names)) {
      if (attr_name %in% avoid) next
      attr_list[[attr_name]] <- cls[[attr_name]]
    }

    method_list$env <- env

    # Bind methods to the target container
    do.call(register_method, method_list)

    # Set attributes
    list2env(attr_list, envir = env)
  }

  return(env)
}


# use_method --------------------------------------------------------------

#' Use a method in an object environment
#'
#' This function makes a copy of the function, then set the evaluation
#' environment to the container of the object environment.
#'
#' @param env Environment. Object.
#' @param fn Function. Method.
#' @param container_name Character. Name of the container.
#' @return A method.
#'
#' @examples
#'
#' TEST <- new_class(class_name = "TEST")
#'
#' register_method(TEST, ..str.. = function() "test")
#'
#' test <- TEST$instantiate(dist = "uniform", prm = list(a = 1, b = 2))
#' test$..str..()
#'
#' # Use method `..str..` from BASE class
#' use_method(test, BASE$..str..)()
#'
#' @export
use_method <- function(env, fn, container_name = "..method_env..") {

  if (!is.function(fn)) stop("`fn` is not a function!")

  # Bind function copy to the target environment
  bind_fn_2_env(env[[container_name]], fn)
  return(fn)
}

# BASE --------------------------------------------------------------------

# nocov start

class_BASE <- function(env = new.env(parent = parent.frame())) {

  # Pass CMD check
  self <- NULL

  # Define a new class, empty_class = TRUE because we want to define a base object class
  new_class(env = env, class_name = "BASE", empty_class = TRUE)

  # Default instantiate method
  instantiate_ <- function(..., env = new.env(parent = parent.frame())) {

    # Create an new object, called the class `..new..` method
    self$..new..(env = env, init_call = sys.call())

    # Init, called the object `..init..` method
    env$..init..(...)

    # `instantiate` method should return the environment by convention
    return(env)
  }

  new_ <- function(env = new.env(parent = parent.frame()), init_call = sys.call()) {

    # Copy all the methods and attributes from the class/instance
    # except the container, the instantiate method, init_call
    bandicoot::copy_attr(env, self, avoid = c("..method_env..",
                                              "instantiate",
                                              "..init_call..",
                                              "..instantiated.."))

    # Set the `init_call`
    env$..init_call.. <- init_call

    # Mark the object as an instance
    env$..instantiated.. <- TRUE

    # Set the S3 class
    class(env) <- "bandicoot_oop"

    # `..new..` method should return the environment by convention
    return(env)
  }

  # Default init method
  init_ <- function(...) return(self)

  methods_ <- function() names(self)[unlist(lapply(names(self), function(x) is.function(self[[x]])))]

  has_attr_ <- function(attr_name) attr_name %in% names(self)

  set_attr_ <- function(attr_name, attr_val) {
    self[[attr_name]] <- attr_val
    return(self)
  }

  get_attr_ <- function(attr_name) self[[attr_name]]

  del_attr_ <- function(attr_name) {

    if (attr_name %in% names(self)) {
      rm(list = attr_name, envir = self)
    }
    return(self)
  }

  dict_ <- function() names(self)

  len_ <- function() NULL

  repr_ <- function() paste(deparse(self$..init_call.., width.cutoff = 500L), collapse = " ")

  str_ <- function() {
    if (self$..instantiated..) {
      return(paste0("<", self$..type.., " object>"))
    } else {
      return(paste0("<", self$..type.., " class>"))
    }
  }

  register_method(env,
                  instantiate = instantiate_,
                  ..new.. = new_,
                  ..init.. = init_,
                  ..methods.. = methods_,
                  has_attr = has_attr_,
                  set_attr = set_attr_,
                  del_attr = del_attr_,
                  get_attr = get_attr_,
                  ..dict.. = dict_,
                  ..repr.. = repr_,
                  ..str.. = str_,
                  ..len.. = len_)
  return(env)
}

# nocov end


#' Load functions from the bandicoot into target environment
#'
#' This function is critical when other packages want to use the bandicoot OOP
#' system. Since this OOP system is based on environment, any instance will only
#' run on the environment they defined. So, function like [use_method] which
#' only exists in the package environment can not be accessed by the instance,
#' unless the function has been loaded into the current environment by calling
#' `use_method <- bandicoot::use_method`, or `require(bandicoot)` or
#' `library(bandicoot)`. This issue can usually be addressed by
#' using the package name directly inside the method body like
#' `this_method <- function() bandicoot::use_method()`. However, if it is not
#' possible, then this function helps loads corresponding function into target
#' environment.
#'
#' This function will call [define_pkg_fn].
#'
#' @param env Environment. The target environment.
#' @param import_oop Boolean. Whether or not to import OOP tools.
#' @param import_base Boolean. Whether or not to import BASE class.
#' @return No return value, called for side effects.
#'
#' @export
import_bandicoot <- function(env = parent.frame(),
                             import_oop = TRUE,
                             import_base = TRUE) {
  final_list <- list()

  if (import_oop) final_list <- append(final_list, oop_dependencies)
  if (import_base) final_list <- append(final_list, base_dependencies)

  do.call(define_pkg_fn, append(list(pkg = "bandicoot"), final_list),
          envir = env)
}
