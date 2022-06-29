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
#' @format An environment with S3 class `bandicoot_oop`.
#' @seealso Attributes: [BASE$..type..], [BASE$..class..], [BASE$..method_env..],
#' [BASE$..instantiated..]
#' \cr
#' \cr
#' Methods: [BASE$..dir..], [BASE$..str..], [BASE$..repr..], [BASE$..len..],
#' [BASE$has_attr], [BASE$get_attr], [BASE$set_attr], [BASE$del_attr],
#' [BASE$..methods..], [BASE$..init..], [BASE$..new..], [BASE$instantiate]
#' @export
BASE <- new.env()

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
#' @description This function lists all methods of a class or an instance
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
#' @param env Environment. The instance environment. Default is
#' `env = new.env(parent = parent.frame())`.
#' @param init_call Call. Contents of the `..init_call..`. It is recommended to
#' leave it as default. Default is `init_call = sys.call()`.
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
#' @param ... Arguments passed to `..init..` method.
#' @param env Environment. The instance environment.
#' @param init_call Call. Contents of the `..init_call..`. It is recommended to
#' leave it as default. Default is `init_call = sys.call()`.
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



# BASE2 <- class_BASE2()
