
# BASE --------------------------------------------------------------------

BASE <- new.env()

#' BASE class environment
#'
#' @name BASE
#'
#' @description This class provides essential attributes and methods. It makes
#' the assumption that the container name is `..method_env..` and the name of
#' the reference to self is `self`. If you would like to use other container
#' names and self names, you need to overwrite the class definition of BASE.
#' \cr
#' \cr
#' The class environment is defined as an empty environment by [new.env()]
#' at build-time, and the class descriptor is run at load-time by
#' [.onLoad()].
#' This ensures methods and attributes of the class is built with the
#' load-time (usually latest) installed dependencies (if it depends on any).
#' Derived classes should follow the same principle to avoid running the class
#' descriptor at build-time, and only defines the content of the class at
#' load-time.
#' \cr
#' \cr
#' Since `bandicoot` does not support dynamic dispatch,
#' calling the correct parent method can be difficult in a
#' complex class system.
#' So, users can use the `..mro..` (method resolution order) attribute and the
#' [super()] function to determine the correct super/next class.
#' If users decide to store parent environments in
#' the derived class such that parent method can be called more handily,
#' awareness needs to be raised when saving and loading these classes/instances.
#' It is very likely the same class stored in different objects becomes different
#' environments.
#'
#' @param ... Ignored.
#' @param env Environment. The instance environment.
#' @param init_call Call. Contents of the `..init_call...` It is recommended to leave it
#' as default.
#' @return An instance environment.
#'
#' @details # Class information
#' ## Attributes
#' * B:
#'    * [BASE$..bases..]
#' * C:
#'    * [BASE$..class..]
#'    * [BASE$..class_tree..]
#' * I:
#'    * [BASE$..instantiated..]
#' * M:
#'    * [BASE$..method_env..]
#'    * [BASE$..mro..]
#'
#' ## Methods
#' * D:
#'    * [BASE$del_attr()]
#'    * [BASE$..dir..()]
#' * G:
#'    * [BASE$get_attr()]
#' * H:
#'    * [BASE$has_attr()]
#' * I:
#'    * [BASE$..init..()]
#'    * [BASE$instantiate()]
#' * L:
#'    * [BASE$..len..()]
#' * M:
#'    * [BASE$..methods..()]
#' * N:
#'    * [BASE$..new..()]
#' * R:
#'    * [BASE$..repr..()]
#' * S:
#'    * [BASE$set_attr()]
#'    * [BASE$..str..()]
#'
#' @export
BASE

#' @describeIn BASE Class constructor, same as [BASE$instantiate()].
#' @export
base_ <- function(...,
                  env = new.env(parent = parent.frame()),
                  init_call = sys.call()) {
  BASE$instantiate(..., env = env, init_call = init_call)
}

#' Class name
#'
#' @name BASE$..type..
#'
#' @description A string.
#'
#' @examples
#'
#' BASE$..type..
#'
#' # Inherit from BASE
#' TEST <- new_class(BASE, class_name = "TEST")
#' TEST$..type..
BASE$..type..

#' Class name and parent class names
#'
#' @name BASE$..class..
#'
#' @description A string vector.
#'
#' @examples
#'
#' BASE$..class..
#'
#' # Inherit from BASE
#' TEST <- new_class(BASE, class_name = "TEST")
#' TEST$..class..
BASE$..class..

#' Class name and parent class names represented in a tree
#'
#' @name BASE$..class_tree..
#'
#' @description A list.
#'
#' @examples
#'
#' BASE$..class_tree..
#'
#' # Inherit from BASE
#' TEST <- new_class(BASE, class_name = "TEST")
#' TEST$..class_tree..
BASE$..class_tree..

#' Method resolution order
#'
#' @name BASE$..mro..
#'
#' @description Method resolution order defined using C3 algorithm.
#'
#' @examples
#'
#' BASE$..mro..
#'
#' # Inherit from BASE
#' TEST <- new_class(BASE, class_name = "TEST")
#' TEST$..mro..
BASE$..mro..

#' Direct parent classes
#'
#' @name BASE$..bases..
#'
#' @description Direct parent classes
#'
#' @examples
#'
#' BASE$..bases..
#'
#' # Inherit from BASE
#' TEST <- new_class(BASE, class_name = "TEST")
#' TEST$..bases..
BASE$..bases..

#' The container
#'
#' @name BASE$..method_env..
#'
#' @description A container where methods will be executed.
#'
#' @examples
#'
#' BASE$..method_env..
#'
#' # Inherit from BASE
#' TEST <- new_class(BASE, class_name = "TEST")
#' TEST$..method_env..
BASE$..method_env..

#' Instantiate status
#'
#' @name BASE$..instantiated..
#'
#' @description Whether or not the object is an instance.
#'
#' @examples
#'
#' BASE$..instantiated..
#'
#' # Instantiate
#' test <- BASE$instantiate()
#' test$..instantiated..
BASE$..instantiated..

#' All names in the class or instance environment
#'
#' @name BASE$..dir..
#'
#' @description This function returns all names in the environment.
#'
#' ## Usage
#' ```
#' BASE$..dir..()
#' ```
#'
#' @return A vector of string.
#'
#' @examples
#'
#' BASE$..dir..()
#'
#' # Instantiate
#' test <- BASE$instantiate()
#' test$..dir..()
BASE$..dir..

#' String representation of the object
#'
#' @name BASE$..str..
#'
#' @description This function returns a string representation of the object.
#'
#' ## Usage
#' ```
#' BASE$..str..()
#' ```
#'
#' @return A string.
#'
#' @examples
#'
#' BASE$..str..()
#'
#' # Inherit from BASE
#' TEST <- new_class(BASE, class_name = "TEST")
#' TEST$..str..()
#'
#' # Instantiate
#' test <- BASE$instantiate()
#' test$..str..()
BASE$..str..

#' "Official" String representation of the object
#'
#' @name BASE$..repr..
#'
#' @description This function returns a "official" string representation of
#' the object, which may be used to reconstruct the object given an appropriate
#' environment.
#'
#' ## Usage
#' ```
#' BASE$..repr..()
#' ```
#'
#' @return A string.
#'
#' @examples
#'
#' BASE$..repr..()
#'
#' test <- base_()
#' test$..repr..()
#'
#' test <- BASE$instantiate()
#' test$..repr..()
#'
#' test <- BASE$..new..()
#' test$..repr..()
BASE$..repr..

#' Length of the class or the instance
#'
#' @name BASE$..len..
#'
#' @description User could override this method in derived class.
#'
#' ## Usage
#' ```
#' BASE$..len..()
#' ```
#'
#' @examples
#'
#' BASE$..len..()
#'
#' # Inherit from BASE
#' TEST <- new_class(BASE, class_name = "TEST")
#'
#' # Override the `..len..` method
#' register_method(TEST, ..len.. = function() 1)
#' TEST$..len..()
BASE$..len..

#' Whether or not an attribute or method exists
#'
#' @name BASE$has_attr
#'
#' @description This function checks whether or not an attribute or method
#' exists.
#'
#' ## Usage
#' ```
#' BASE$has_attr(attr_name)
#' ```
#'
#' @param attr_name Character. Attribute name.
#' @return True or FALSE.
#'
#' @examples
#'
#' BASE$has_attr("test")
#'
#' BASE$has_attr("..len..")
BASE$has_attr

#' Get value of an attribute or a method
#'
#' @name BASE$get_attr
#'
#' @description This function gets the value of an attribute or a method.
#'
#' ## Usage
#' ```
#' BASE$get_attr(attr_name)
#' ```
#'
#' @param attr_name Character. Attribute name.
#' @return The attribute value.
#'
#' @examples
#'
#' BASE$get_attr("test")
#'
#' BASE$get_attr("..methods..")
BASE$get_attr


#' Set value of an attribute or a method
#'
#' @name BASE$set_attr
#'
#' @description This function sets the value of an attribute or a method.
#'
#' ## Usage
#' ```
#' BASE$set_attr(attr_name, attr_val)
#' ```
#'
#' @param attr_name Character. Attribute name.
#' @param attr_val Any value.
#' @return Return the object itself.
#'
#' @examples
#'
#' test <- BASE$instantiate()
#' test$set_attr("x", 1)
#' test$x
BASE$set_attr

#' Delete an attribute
#'
#' @name BASE$del_attr
#'
#' @description This function delete an attribute.
#'
#' ## Usage
#' ```
#' BASE$del_attr(attr_name)
#' ```
#'
#' @param attr_name Character. Attribute name.
#' @return Return the object itself.
#'
#' @examples
#'
#' test <- BASE$instantiate()
#' test$set_attr("x", 1)
#' test$x
#' test$del_attr("x")
#' test$x
BASE$del_attr

#' List all methods of a class or an instance
#'
#' @name BASE$..methods..
#'
#' @description This function lists all methods of a class or an instance.
#'
#' ## Usage
#' ```
#' BASE$..methods..()
#' ```
#'
#' @return A string vector.
#'
#' @examples
#'
#' BASE$..methods..()
BASE$..methods..

#' Initialization method
#'
#' @name BASE$..init..
#'
#' @description This function will be called after an instance is built. User
#' could override this function in derived class.
#'
#' ## Usage
#' ```
#' BASE$..init..(...)
#' ```
#'
#' @param ... Ignored by `BASE`, but user can define their owns.
#' @return Return the object itself.
#'
#' @examples
#'
#' BASE$..init..
#'
#' # Inherit from BASE
#' TEST <- new_class(BASE, class_name = "TEST")
#'
#' # Override the `..init..` method
#' register_method(TEST, ..init.. = function(a) {self$x <- a})
#'
#' # Build a `TEST` instance
#' test <- TEST$instantiate(a = 2)
#'
#' test$x
BASE$..init..

#' Build a new instance from a class or an instance
#'
#' @name BASE$..new..
#'
#' @description This function will copy all methods and attributes, except
#' the container, and the instantiate method. Then, the `..init_call..` attribute
#' will be set to the current system call, and the `..instantiated..` attribute
#' will be set to `TRUE`. Notice, the `..init..` method will not run.
#'
#' ## Usage
#' ```
#' BASE$..new..(env = new.env(parent = parent.frame()), init_call = sys.call())
#' ```
#'
#' @param env Environment. The instance environment.
#' @param init_call Call. Contents of the `..init_call..`. It is recommended to
#' leave it as default.
#' @return An instance environment.
#'
#' @examples
#'
#' BASE$..new..()
#'
#' # Inherit from BASE
#' TEST <- new_class(BASE, class_name = "TEST")
#'
#' TEST$..new..()
BASE$..new..


#' Instantiate method
#'
#' @name BASE$instantiate
#'
#' @description This function will new an instance using the `..new..` method,
#' then initialized the instance with the `..init..` method.
#'
#' ## Usage
#' ```
#' BASE$instantiate(
#'   ...,
#'   env = new.env(parent = parent.frame()),
#'   init_call = sys.call()
#' )
#' ```
#'
#' @param ... Arguments passed to `..init..` method.
#' @param env Environment. The instance environment.
#' @param init_call Call. Contents of the `..init_call..`. It is recommended to
#' leave it as default.
#' @return An instance environment.
#'
#' @examples
#'
#' BASE$..dir..()
#'
#' # Build an instance
#' base_instance <- BASE$instantiate()
#'
#' base_instance$..dir..()
BASE$instantiate
