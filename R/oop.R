# C3_mro ------------------------------------------------------------------

C3_mro <- function(class_tree) {

  if (!is.list(class_tree[[1]])) return(class_tree[[1]])

  C3_remove_good_head <- function(current_list, good_head) {

    result_list <- list()
    i <- 0
    good_head_value <- current_list[[good_head]][1]

    for (item in current_list) {
      if (length(item[item != good_head_value]) != 0) {
        i <- i + 1
        result_list[[i]] <- item[item != good_head_value]
      }
    }

    return(result_list)
  }

  C3_find_good_head <- function(current_list) {

    if (length(current_list) == 1) return(1)

    for (idx in 1:length(current_list)) {
      tails_ <- unlist(lapply(current_list[-idx], function(x) x[-1]))

      if (length(tails_) == 0) return(idx)

      if (!any(current_list[[idx]][1] %in% tails_)) {
        return(idx)
      }
    }

    stop("Fail to create class! Unable to resolve the Method Resultion Order.")
  }

  C3_merge <- function(merge_list) {

    merge_result <- c()
    while (length(merge_list) > 0) {
      good_head <- C3_find_good_head(merge_list)
      merge_result <- c(merge_result, merge_list[[good_head]][1])
      merge_list <- C3_remove_good_head(merge_list, good_head)
    }

    return(merge_result)
  }

  mro <- c(names(class_tree)[1],
           C3_merge(
             append(lapply(names(class_tree[[1]]),
                           function(x) {
                             tmp_list <- list(class_tree[[1]][[x]])
                             names(tmp_list) <- x
                             C3_mro(tmp_list)
                             }),
                    as.list(names(class_tree[[1]])))
             )
           )

  mro

}

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
#' @param class_name Character. Name of the class of the object environment.
#' This is important for `super()` to resolve the correct parent class.
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
#' register_method(e, aa = a, self_name = "self", class_name = "test")
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
register_method <- function(env, ..., container_name = "..method_env..", self_name = "self", class_name = env$..type..) {

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
  fn_list$class_name <- NULL

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

    # Prepare the assignment expression
    set_mro_current <- substitute(..mro_current.. <- class_name,
                                  list(class_name = class_name))

    # Get the function body
    body_list <- as.list(body(eval_fn))
    mro_current_exist <- FALSE

    if (length(body_list) > 1) {
      # Get the second expression
      second_expr <- as.list(body_list[[2]])

      # If the second expression is of the form `..mro_current.. <- class_name`
      # Modify the expression to represent the correct current class
      if (identical(second_expr[[1]], as.symbol("<-")) &&
          identical(second_expr[[2]], as.symbol("..mro_current.."))) {

        mro_current_exist <- TRUE
        body_list[[2]] <- set_mro_current
        body(eval_fn) <- as.call(body_list)

      }
    }

    if (!mro_current_exist) {
      # Set `..mro_current.. <- class_name` as the first expression of the function body
      # This makes it clear which class the function belongs to
      body(eval_fn) <- substitute({set_mro_current; original},
                                  list(set_mro_current = set_mro_current,
                                       original = body(eval_fn)))
    }

    # Bind it to the container of the instance environment
    env[[fn_names[i]]] <- eval_fn
    bind_fn_2_env(env[[container_name]], env[[fn_names[i]]])
  }

  return(invisible(env))
}


# new_class ---------------------------------------------------------------

#' Define a new class
#'
#' This function declare a new class, and copies attributes and methods from
#' parent classes.
#'
#' Parents can be provided in `...`, where methods and attributes will be
#' overrided by the left classes because `bandicoot` does not support dynamic
#' dispatch at the moment. However, this behaviour usually aligns with the method
#' resolution order defined by the C3 algorithm used in Python.
#' If `...` is empty and `empty_class == FALSE`,
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

  # If it is an empty class, define a single node class tree
  # Otherwise, copy the class trees as nodes
  if (empty_class) {
    env$..class_tree.. <- list(class_name)
    names(env$..class_tree..) <- class_name
  } else {
    env$..class_tree.. <- list(lapply(parent_cls_list, function(x) x$..class_tree..[[1]]))
    names(env$..class_tree..) <- class_name
    all_parent_names <- unlist(lapply(parent_cls_list, function(x) x$..type..))
    names(env$..class_tree..[[1]]) <- all_parent_names
    env$..bases.. <- all_parent_names
  }

  env$..mro.. <- C3_mro(env$..class_tree..)

  for (parent in rev(parent_cls_list)) {

    if (parent$..instantiated..) stop("Parent is not a class!")

    env$..class.. <- c(parent$..class.., env$..class..)

    # Copy all the methods and attributes from the class/instance
    # except the container, the init_call, and the class information
    copy_attr(env,
              parent,
              avoid = c("..method_env..",
                        "..init_call..",
                        "..class..",
                        "..type..",
                        "..instantiated..",
                        "..class_tree..",
                        "..mro..",
                        "..bases.."),
              class_name = class_name)
  }

  env$..class.. <- unique(c(class_name, env$..class..))
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
#' @param class_name Character. Name of the class the method is defined.
#' This is important for `super()` to resolve the correct parent class.
#' @return Return the object itself.
#'
#' @examples
#'
#' test <- new.env()
#' names(BASE)
#' copy_attr(test, BASE, avoid = c("..method_env..", "..init_call..", "..dir.."))
#' names(test)
#'
#' @export
copy_attr <- function(env, ..., avoid = c("..method_env..", "..init_call.."), class_name = env$..type..) {

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
    do.call(register_method, append(method_list, list(class_name = class_name)))

    # Set attributes
    list2env(attr_list, envir = env)
  }

  return(invisible(env))
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


# super -------------------------------------------------------------------

#' Get the parent class (the next class based on the method resolution order)
#'
#' This function gets the parent class or the next class based on the method
#' resolution order. This is useful when one wants to access the overwritten
#' parent class method or the overwritten parent class attribute.
#'
#' Note that this function assumes the parent class can be found in the
#' parent environment of the current object. If one wants to find the
#' parent class from a package, it needs to be specified via the `where`
#' argument.
#'
#' @param self_name Character. The name of the self reference.
#' @param mro_current_name Character. The name of the variable storing
#' the current class. This is used to determine the next class.
#' @param where Environment/Character. The target environment to search for the
#' parent class. If `where == NULL`, the parent environment of self will be used.
#' If a character value is provided, the package with the same name will be used.
#' @return A `bandicoot` object which is an environment.
#'
#' @examples
#'
#' # Define class O
#' O <- new_class(class_name = "O")
#' register_method(O, foo = function() {
#'   print("Calling class O `foo` method")
#'   print(paste0("Self is ", self$my_name))
#'   print(paste0("Next class is ", super()$..type..))
#' })
#'
#' # Define class F
#' F <- new_class(O, class_name = "F")
#' register_method(F, foo = function() {
#'   print("Calling class F `foo` method")
#'   print(paste0("Self is ", self$my_name))
#'   print(paste0("Next class is ", super()$..type..))
#'   use_method(self, super()$foo)()
#' })
#'
#' # Define class E
#' E <- new_class(O, class_name = "E")
#' register_method(E, foo = function() {
#'   print("Calling class E `foo` method")
#'   print(paste0("Next class is ", super()$..type..))
#'   use_method(self, super()$foo)()
#' })
#'
#' # Define class D
#' D <- new_class(O, class_name = "D")
#' register_method(D, foo = function() {
#'   print("Calling class D `foo` method")
#'   print(paste0("Self is ", self$my_name))
#'   print(paste0("Next class is ", super()$..type..))
#'   use_method(self, super()$foo)()
#' })
#'
#' # Define class C
#' C <- new_class(D, F, class_name = "C")
#' register_method(C, foo = function() {
#'   print("Calling class C `foo` method")
#'   print(paste0("Self is ", self$my_name))
#'   print(paste0("Next class is ", super()$..type..))
#'   use_method(self, super()$foo)()
#' })
#'
#' # Define class B
#' B <- new_class(E, D, class_name = "B")
#' register_method(B, foo = function() {
#'   print("Calling class B `foo` method")
#'   print(paste0("Self is ", self$my_name))
#'   print(paste0("Next class is ", super()$..type..))
#'   use_method(self, super()$foo)()
#' })
#'
#' # Define class A
#' A <- new_class(B, C, class_name = "A")
#' register_method(A, foo = function() {
#'   print("Calling class A `foo` method")
#'   print(paste0("Self is ", self$my_name))
#'   print(paste0("Next class is ", super()$..type..))
#'   use_method(self, super()$foo)()
#' })
#'
#' # To understand why the order is A, B, E, C, D, F, O,
#' # please check [https://www.python.org/download/releases/2.3/mro/].
#' a <- A$instantiate()
#' a$my_name <- "a"
#' a$foo()
#'
#'
#' @export
super <- function(self_name = "self", mro_current_name = "..mro_current..", where = NULL) {

  env <- eval(as.symbol(self_name), envir = parent.frame())
  current_class <- eval(as.symbol(mro_current_name), envir = parent.frame())

  if (length(env$..mro..) == 0) stop("The MRO is empty!")
  if (!current_class %in% env$..mro..) stop("The current class is not in the MRO. Can not resolve the next class!")
  if (which(current_class == env$..mro..) == length(env$..mro..)) stop("The current class is the last class in the MRO. Can not find the next class!")

  # Get the next class name
  next_class <- env$..mro..[which(current_class == env$..mro..) + 1]

  # Try to get the parent class
  if (is.null(where)) {
    next_class_object <- eval(as.symbol(next_class), envir = parent.env(env))
  } else if (is.character(where)) {
    next_class_object <- utils::getFromNamespace(next_class, where)
  } else if (is.environment(where)) {
    next_class_object <- eval(as.symbol(next_class), envir = where)
  } else {
    stop("The argument `where` is not an environment or a character!")
  }

  is_bandicoot_oop(next_class_object)

  return(next_class_object)
}

# BASE --------------------------------------------------------------------

# nocov start

class_BASE <- function(env = new.env(parent = parent.frame())) {

  # Pass CMD check
  self <- NULL

  # Define a new class, empty_class = TRUE because we want to define a base object class
  new_class(env = env, class_name = "BASE", empty_class = TRUE)

  # Default instantiate method
  instantiate_ <- function(..., env = new.env(parent = parent.frame()), init_call = sys.call()) {

    # Create an new object, called the class `..new..` method
    self$..new..(env = env, init_call = init_call)

    # Init, called the object `..init..` method
    env$..init..(...)

    # `instantiate` method should return the environment by convention
    return(env)
  }

  new_ <- function(env = new.env(parent = parent.frame()), init_call = sys.call()) {

    # Copy all the methods and attributes from the class/instance
    # except the container, the instantiate method, init_call
    bandicoot::copy_attr(env,
                         self,
                         avoid = c("..method_env..",
                                   "instantiate",
                                   "..init_call..",
                                   "..instantiated.."),
                         class_name = self$..type..)

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
  init_ <- function(...) return(invisible(self))

  methods_ <- function() names(self)[unlist(lapply(names(self), function(x) is.function(self[[x]])))]

  has_attr_ <- function(attr_name) attr_name %in% names(self)

  set_attr_ <- function(attr_name, attr_val) {
    self[[attr_name]] <- attr_val
    return(invisible(self))
  }

  get_attr_ <- function(attr_name) self[[attr_name]]

  del_attr_ <- function(attr_name) {

    if (attr_name %in% names(self)) {
      rm(list = attr_name, envir = self)
    }
    return(invisible(self))
  }

  dir_ <- function() names(self)

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
                  ..dir.. = dir_,
                  ..repr.. = repr_,
                  ..str.. = str_,
                  ..len.. = len_)
  return(env)
}

# nocov end


# is_bandicoot_oop --------------------------------------------------------


#' Check whether the object is a `bandicoot_oop` object
#'
#' This function check whether the object is a `bandicoot_oop` object.
#'
#' @param obj Any object.
#' @param why Boolean. Whether or not to print the reason when the check fail.
#' @return A Boolean value.
#'
#' @examples
#'
#' e <- new.env()
#' is_bandicoot_oop(e)
#'
#' e <- new_class(class_name = "test")
#' is_bandicoot_oop(e)
#'
#' @export
is_bandicoot_oop <- function(obj, why = FALSE) {

  if (!is.environment(obj)) ifelse(why, {message("Object is not an environment!"); return(FALSE)}, return(FALSE))

  if (!"bandicoot_oop" %in% class(obj)) ifelse(why, {message("`bandicoot_oop` not in object's S3 classes!"); return(FALSE)}, return(FALSE))

  if (!"..class.." %in% names(obj)) ifelse(why, {message("Object does not contain `..class..`!"); return(FALSE)}, return(FALSE))
  if (!is.character(obj$..class..)) ifelse(why, {message("`..class..` is not character!"); return(FALSE)}, return(FALSE))

  if (!"..type.." %in% names(obj)) ifelse(why, {message("Object does not contain `..type..`!"); return(FALSE)}, return(FALSE))
  if (!is.character(obj$..type..)) ifelse(why, {message("`..type..` is not character!"); return(FALSE)}, return(FALSE))

  if (!"..instantiated.." %in% names(obj)) ifelse(why, {message("Object does not contain `..instantiated..`!"); return(FALSE)}, return(FALSE))
  if (!is.logical(obj$..instantiated..)) ifelse(why, {message("`..instantiated..` is not Boolean!"); return(FALSE)}, return(FALSE))

  return(TRUE)
}


# as_bandicoot_oop --------------------------------------------------------

#' Turn an environment into a `bandicoot_oop` object
#'
#' This function tries to turn an environment into a `bandicoot_oop` object.
#'
#' @param env An environment.
#' @param ..class.. Character. A series of class names.
#' @param ..type.. Character. The class name of this object.
#' @param ..instantiated.. Boolean. Whether this object is an instance.
#' @param overwrite_container Boolean. Whether or not to overwrite the container.
#' @param register Boolean. Whether or not to register functions if there are any.
#' @param in_place Boolean. Whether or not to modify the environment in-place.
#' If not, a new environment will be created.
#' @param container_name Character. Name of the container.
#' @param self_name Character. Name of the self reference.
#' @return A Boolean value.
#'
#' @examples
#'
#' e <- new.env()
#' e$a <- function() self
#'
#' as_bandicoot_oop(e,
#'                  ..class.. = "test",
#'                  ..type.. = "test",
#'                  ..instantiated.. = FALSE,
#'                  register = TRUE,
#'                  in_place = TRUE)
#'
#' e
#' e$a()
#'
#' @export
as_bandicoot_oop <- function(env, ..class.. = NULL, ..type.. = NULL, ..instantiated.. = NULL,
                             overwrite_container = FALSE,
                             register = FALSE,
                             in_place = FALSE,
                             container_name = "..method_env..",
                             self_name = "self") {

  obj <- env

  # If user does not want to modify the object in-place
  if (!in_place) {

    # New an object
    new_obj <- new.env(parent = parent.env(obj))

    # Copy everything from the old object
    copy_attr(new_obj, obj, avoid = c(""), class_name = obj$..type..)

    obj <- new_obj
  }

  if (is_bandicoot_oop(obj)) {message("Object passes `is_bandicoot_oop()`. Take no action. If you want to new a container, consider using `register_method()`."); return()}
  if (container_name %in% names(obj) && (!overwrite_container)) stop(paste0("Object already contains `", container_name, "`. Do you want to manually remove it or overwrite it by setting `overwrite_container = TRUE`?"))

  # Overwrite the container
  obj[[container_name]] <- new.env(parent = parent.env(obj))
  obj[[container_name]][[self_name]] <- obj

  all_fn_name <- names(obj)[unlist(lapply(names(obj), function(x) is.function(obj[[x]])))]

  # If there are any functions in the object
  if (length(all_fn_name) > 0) {

    # If the user doesn't want to register those functions
    if (!register) stop("Object contains functions. Do you want to manually remove them or register them by setting `register = TRUE`")

    # Register the function one by one
    for (fn_name in all_fn_name) {
      call_list <- list(env = obj, fn = obj[[fn_name]], self_name = self_name, container_name = container_name)
      names(call_list) <- c("env", fn_name, "self_name", "container_name")
      do.call(register_method, call_list)
    }
  }

  if (is.null(..class..)) {
    if (!"..class.." %in% names(obj)) stop("Object doesn't contain `..class..`. Do you want to provide one via argument `..class..`?")
  } else {
    obj$..class.. <- ..class..
  }

  if (is.null(..type..)) {
    if (!"..type.." %in% names(obj)) stop("Object doesn't contain `..type..`. Do you want to provide one via argument `..type..`?")
  } else {
    obj$..type.. <- ..type..
  }

  if (is.null(..instantiated..)) {
    if (!"..instantiated.." %in% names(obj)) stop("Object doesn't contain `..instantiated..`. Do you want to provide one via argument `..instantiated..`?")
  } else {
    obj$..instantiated.. <- ..instantiated..
  }

  class(obj) <- "bandicoot_oop"

  # final check
  if (!is_bandicoot_oop(obj, TRUE)) stop("Can't proceed")

  return(obj)
}

# make_instantiator -------------------------------------------------------


#' Make convenient wrapper for instantiation method
#'
#' This function creates a instantiator wrapper for a class by inspecting
#' the required arguments of its `..new..` and `..init..` methods. It
#' captures the class name from the provided argument, so it is best
#' invoked directly, e.g., `my_class <- make_instantiator(MY_CLASS)`.
#'
#' Care must be taken when either `..new..` or `..init..` includes `...` as a
#' formal argument, as this may lead to unexpected behavior in argument passing
#' within the wrapper function. After creating the wrapper, it is recommended to
#' reorder or modify the formals as needed using [formals()].
#'
#' @param class_env Environment. A class environment.
#' @param env Environment. Environment in which the wrapper should be defined.
#' @param new_defaults alist. New defaults to be used in the formal argument of
#' the wrapper. Formal argument not presented in `..new..` or `..init..` will
#' be ignored and a warning will be raised. See also [alist()].
#' @return A wrapper function.
#'
#' @examples
#'
#' MYCLASS <- new_class(class_name = "MYCLASS")
#' register_method(MYCLASS, ..init.. = function(name) self$name <- name)
#' myclass <- make_instantiator(MYCLASS)
#' myclass
#'
#' myclass("Mike")$name
#'
#' myclass_2 <- make_instantiator(MYCLASS, new_defaults = alist(name = "MIKE"))
#' myclass_2()$name
#'
#' @export
make_instantiator <- function(class_env, env = parent.frame(), new_defaults = alist()) {
  if (!is_bandicoot_oop(class_env)) {
    stop("`class_env` is not a `bandicoot_oop` object!")
  }

  if (class_env$..instantiated..) {
    stop("`class_env` is not a class!")
  }

  new_formals <- formals(class_env$..new..)
  init_formals <- formals(class_env$..init..)

  commons <- intersect(names(new_formals), names(init_formals))
  if (length(commons) > 0) {
    commons <- paste0("`", commons, "`", collapse = ", ")
    stop(paste0("Formal argument ", commons, " matched by multiple actual arguments! Please check your `..new..` and `..init..` method."))
  }

  result_formals <- c(init_formals, new_formals)

  result_names <- names(result_formals)

  for (default_name in names(new_defaults)) {
    if (!default_name %in% result_names) {
      warning(paste0("Unmatched formal argument ", default_name, " will be ignored!"))
    } else {
      result_formals[[default_name]] <- new_defaults[[default_name]]
    }
  }

  result_call <- lapply(result_names, function(name) as.symbol(name))
  result_call <- c(bquote(.(substitute(class_env))$instantiate), result_call)
  names(result_call) <- c("constructor", result_names)
  result_call <- as.call(result_call)

  wrapper <- as.function(c(result_formals, quote({})), envir = env)
  body(wrapper) <- substitute({result_call}, list(result_call = result_call))
  return(wrapper)
}
