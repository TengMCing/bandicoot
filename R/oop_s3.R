
# Python dunder method ----------------------------------------------------

# Before implement this methods, check the documentation such that the behaviours are similar.

# a

# -[] __annotations__: this is the attribute for "type hints" in Python. A very promising attribute. Could be used in the future for type-checking system.
# -[] __anext__: Async related method. Will not be considered.
# -[] __add__: x + y. Could be implemented using `+.bandicoot`.
# -[] __and__: x & y. Could be implemented using `&.bandicoot`.
# -[] __abs__: abs(x). Could be implemented using `abs.bandicoot`.
# -[] __await__: Async related method. Will not be considered.
# -[] __aiter__: Async related method. Will not be considered.
# -[] __aenter__: Async related method. Will not be considered.
# -[] __aexit__: Async related method. Will not be considered.

# b

# -[] __bases__: A tuple containing the base classes, in the order of their occurrence in the base class list. Python read-only attributes. Will not be considered.
# -[] __bytes__: bytes(x). Could be implemented using `bytes.bandicoot`.
# -[] __bool__: bool(x). Could be implemented using `bool.bandicoot`.

# c

# -[] __code__: The code object representing the compiled function body. Will not be considered.
# -[] __closure__: Python read-only attribute. Will not be considered.
# -[] __call__: Not easy to implement in R.
# -[] __class__: The class of an instance. Currently implemented as a character vector.
# -[] __class_getitem__: Designed for type hints. Will not be considered.
# -[] __contains__: x in y. Could be implemented using `%contains%.bandicoot`.
# -[] __complex__: complex(x). Will not be considered.
# -[] __ceil__: ceil(x). Could be implemented using `complex.bandicoot`.
# -[] __doc__: The function's documentation string. Will not be considered.
# -[] __defaults__: Default arguments. Will not be considered.
# -[] __dict__: The namespace supporting arbitrary function attributes. Will only be implemented using `..dict..`.
# -


# print.bandicoot_oop -----------------------------------------------------

#' S3 method of printing `bandicoot_oop` object
#'
#' This function print the string representation of the object by using the
#' `..str..()` method.
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


# len.bandicoot_oop -------------------------------------------------------

#' Compute the length of the object
#'
#' @param x Object.
#' @param ... Additional arguments needed for computing the length.
#' @return An integer.
#'
#' @export
len <- function(x, ...) {
  UseMethod("len")
}

#' S3 method of computing the length of `bandicoot_oop` object
#'
#' This function computes the length of the object by using the `..len..()`
#' method. If it is not applicable, error will be raised.
#'
#' @param x `bandicoot_oop` object.
#' @param ... ignored.
#' @return An integer.
#' @export
len.bandicoot_oop <- function(x, ...) {
  if ("..len.." %in% names(x)) {
    return(x$..len..())
  } else {
    stop("Not Implemented!")
  }
}


# repr.bandicoot_oop ------------------------------------------------------

#' The "official" string representation of an object.
#'
#' The "official" string representation of an object. If at all possible,
#' this should look like a valid R expression that could be used to recreate
#' an object wit the same value (given an appropriate environment). This
#' description is copied from the python documentation.
#'
#' @param x Object.
#' @param ... Additional arguments needed for computing the string.
#' @return A string.
#'
#' @export
repr <- function(x, ...) {
  UseMethod("repr")
}

#' S3 method of computing the "official" string representation of a
#' `bandicoot_oop` object
#'
#' This function computes the "official" string representation of a
#' `bandicoot_oop` object using the `..repr..()` method.
#' If it is not applicable, error will be raised.
#'
#' @param x `bandicoot_oop` object.
#' @param ... ignored.
#' @return An integer.
#' @export
repr.bandicoot_oop <- function(x, ...) {
  if ("..repr.." %in% names(x)) {
    return(x$..repr..())
  } else {
    stop("Not Implemented!")
  }
}


# %lt%.bandicoot_oop ------------------------------------------------------

#' The less than operator
#'
#' @param x Object.
#' @param y Another object.
#' @return A Boolean value.
#'
#' @export
`%lt%` <- function(x, y) {
  UseMethod("%lt%")
}

#' S3 method of performing the less than operator of a
#' `bandicoot_oop` object
#'
#' This function performs the less than operator using the `..lt..()` method.
#' If it is not applicable, error will be raised.
#'
#' @param x `bandicoot_oop` object.
#' @param y Object.
#' @return A Boolean value.
#' @export
`%lt%.bandicoot_oop` <- function(x, y) {
  if ("..lt.." %in% names(x)) {
    return(x$..lt..(y))
  } else {
    stop("Not Implemented!")
  }
}

# %le%.bandicoot_oop ------------------------------------------------------

#' The less or equals to operator
#'
#' @param x Object.
#' @param y Another object.
#' @return A Boolean value.
#'
#' @export
`%le%` <- function(x, y) {
  UseMethod("%le%")
}

#' S3 method of performing the less or equals operator of a
#' `bandicoot_oop` object
#'
#' This function performs the less or equals operator using the `..le..()` method.
#' If it is not applicable, error will be raised.
#'
#' @param x `bandicoot_oop` object.
#' @param y Object.
#' @return A Boolean value.
#' @export
`%le%.bandicoot_oop` <- function(x, y) {
  if ("..le.." %in% names(x)) {
    return(x$..le..(y))
  } else {
    stop("Not Implemented!")
  }
}


# %gt%.bandicoot_oop ------------------------------------------------------

#' The greater than operator
#'
#' @param x Object.
#' @param y Another object.
#' @return A Boolean value.
#'
#' @export
`%gt%` <- function(x, y) {
  UseMethod("%gt%")
}

#' S3 method of performing the greater than operator of a
#' `bandicoot_oop` object
#'
#' This function performs the greater than operator using the `..gt..()` method.
#' If it is not applicable, error will be raised.
#'
#' @param x `bandicoot_oop` object.
#' @param y Object.
#' @return A Boolean value.
#' @export
`%gt%.bandicoot_oop` <- function(x, y) {
  if ("..gt.." %in% names(x)) {
    return(x$..gt..(y))
  } else {
    stop("Not Implemented!")
  }
}

# %ge%.bandicoot_oop ------------------------------------------------------

#' The greater or equals to operator
#'
#' @param x Object.
#' @param y Another object.
#' @return A Boolean value.
#'
#' @export
`%ge%` <- function(x, y) {
  UseMethod("%ge%")
}

#' S3 method of performing the greater or equals operator of a
#' `bandicoot_oop` object
#'
#' This function performs the greater or equals operator using the `..ge..()` method.
#' If it is not applicable, error will be raised.
#'
#' @param x `bandicoot_oop` object.
#' @param y Object.
#' @return A Boolean value.
#' @export
`%ge%.bandicoot_oop` <- function(x, y) {
  if ("..ge.." %in% names(x)) {
    return(x$..ge..(y))
  } else {
    stop("Not Implemented!")
  }
}


# %eq%.bandicoot_oop ------------------------------------------------------

#' The equals to operator
#'
#' @param x Object.
#' @param y Another object.
#' @return A Boolean value.
#'
#' @export
`%eq%` <- function(x, y) {
  UseMethod("%eq%")
}

#' S3 method of performing the equals to operator of a
#' `bandicoot_oop` object
#'
#' This function performs the equals to operator using the `..eq..()` method.
#' If it is not applicable, error will be raised.
#'
#' @param x `bandicoot_oop` object.
#' @param y Object.
#' @return A Boolean value.
#' @export
`%eq%.bandicoot_oop` <- function(x, y) {
  if ("..eq.." %in% names(x)) {
    return(x$..eq..(y))
  } else {
    stop("Not Implemented!")
  }
}


# %ne%.bandicoot_oop ------------------------------------------------------

#' The not equals to operator
#'
#' @param x Object.
#' @param y Another object.
#' @return A Boolean value.
#'
#' @export
`%ne%` <- function(x, y) {
  UseMethod("%eq%")
}

#' S3 method of performing the not equals to operator of a
#' `bandicoot_oop` object
#'
#' This function performs the not equals to operator using the `..ne..()` method.
#' If it is not applicable, error will be raised.
#'
#' @param x `bandicoot_oop` object.
#' @param y Object.
#' @return A Boolean value.
#' @export
`%ne%.bandicoot_oop` <- function(x, y) {
  if ("..ne.." %in% names(x)) {
    return(x$..ne..(y))
  } else {
    stop("Not Implemented!")
  }
}


# iter.bandicoot ----------------------------------------------------------

#' Build an iterator
#'
#' @param x Object.
#' @param ... Additional arguments needed for building an iterator.
#' @return An iterator.
#'
#' @export
iter <- function(x, ...) {
  UseMethod("iter")
}

#' S3 method of building an iterator of a
#' `bandicoot_oop` object
#'
#' This function builds an iterator using the `..iter..()` method.
#' If it is not applicable, error will be raised.
#'
#' @param x `bandicoot_oop` object.
#' @param ... Additional arguments needed for building an iterator.
#' @return An iterator.
#' @export
iter.bandicoot_oop <- function(x, ...) {
  if ("..iter.." %in% names(x)) {
    return(x$..iter..(...))
  } else {
    stop("Not Implemented!")
  }
}


# %contains%.bandicoot ----------------------------------------------------

#' Membership test operator
#'
#' @param x Object.
#' @param y Another object.
#' @return A Boolean value.
#'
#' @export
`%contains%` <- function(x, y) {
  UseMethod("%contains%")
}

#' S3 method of performing membership test operator of a
#' `bandicoot_oop` object
#'
#' This function performs the membership test operator using the
#' `..contains..()` method.
#' If it is not applicable, error will be raised.
#'
#' @param x `bandicoot_oop` object.
#' @param y Another object.
#' @return A Boolean value.
#' @export
`%contains%.bandicoot_oop` <- function(x, y) {
  if ("..contains.." %in% names(x)) {
    return(x$..contains..(y))
  } else {
    stop("Not Implemented!")
  }
}


# %+%.bandicoot -----------------------------------------------------------

#' Addition operator
#'
#' @param x Object.
#' @param y Another object.
#' @return Depends on the method.
#'
#' @export
`%+%` <- function(x, y) {
  UseMethod("%+%")
}

#' S3 method of addition operator of a
#' `bandicoot_oop` object
#'
#' This function performs the addition operator using the
#' `..add..()` method.
#' If it is not applicable, error will be raised.
#'
#' @param x `bandicoot_oop` object.
#' @param y Another object.
#' @return Depends on the method.
#' @export
`%+%.bandicoot_oop` <- function(x, y) {
  if ("..add.." %in% names(x)) {
    return(x$..add..(y))
  } else {
    stop("Not Implemented!")
  }
}

# %+=%.bandicoot ----------------------------------------------------------

#' In-place addition operator
#'
#' @param x Object.
#' @param y Another object.
#' @return Depends on the method.
#'
#' @export
`%+=%` <- function(x, y) {
  UseMethod("%+=%")
}

#' S3 method of in-place addition operator of a
#' `bandicoot_oop` object
#'
#' This function performs the in-place addition operator using the
#' `..iadd..()` method.
#' If it is not applicable, error will be raised.
#'
#' @param x `bandicoot_oop` object.
#' @param y Another object.
#' @return Depends on the method.
#' @export
`%+=%.bandicoot_oop` <- function(x, y) {
  if ("..iadd.." %in% names(x)) {
    return(x$..iadd..(y))
  } else {
    stop("Not Implemented!")
  }
}

# %-%.bandicoot -----------------------------------------------------------

#' Subtraction operator
#'
#' @param x Object.
#' @param y Another object.
#' @return Depends on the method.
#'
#' @export
`%-%` <- function(x, y) {
  UseMethod("%-%")
}

#' S3 method of subtraction operator of a
#' `bandicoot_oop` object
#'
#' This function performs the subtraction operator using the
#' `..sub..()` method.
#' If it is not applicable, error will be raised.
#'
#' @param x `bandicoot_oop` object.
#' @param y Another object.
#' @return Depends on the method.
#' @export
`%-%.bandicoot_oop` <- function(x, y) {
  if ("..sub.." %in% names(x)) {
    return(x$..sub..(y))
  } else {
    stop("Not Implemented!")
  }
}

# %-=%.bandicoot ----------------------------------------------------------

#' In-place subtraction operator
#'
#' @param x Object.
#' @param y Another object.
#' @return Depends on the method.
#'
#' @export
`%-=%` <- function(x, y) {
  UseMethod("%-=%")
}

#' S3 method of in-place subtraction operator of a
#' `bandicoot_oop` object
#'
#' This function performs the in-place subtraction operator using the
#' `..iadd..()` method.
#' If it is not applicable, error will be raised.
#'
#' @param x `bandicoot_oop` object.
#' @param y Another object.
#' @return Depends on the method.
#' @export
`%-=%.bandicoot_oop` <- function(x, y) {
  if ("..isub.." %in% names(x)) {
    return(x$..isub..(y))
  } else {
    stop("Not Implemented!")
  }
}
