# Create portals for class constructor

#' @name portal
#' @title Portals to class instantiate methods
#' @description These functions are the same as `class$instantiate`.
#' @param ... Arguments passed to `init` method.
#' @param env Environment. The instance environment.
NULL

#' @describeIn portal [BASE]
#' @export
base_ <- BASE$instantiate

#' @describeIn portal [RAND_VAR]
#' @export
rand_var <- RAND_VAR$instantiate

#' @describeIn portal [RAND_UNIFORM]
#' @export
rand_uniform <- RAND_UNIFORM$instantiate

#' @describeIn portal [RAND_UNIFORM_D]
#' @export
rand_uniform_d <- RAND_UNIFORM_D$instantiate

#' @describeIn portal [RAND_NORMAL]
#' @export
rand_normal <- RAND_NORMAL$instantiate

#' @describeIn portal [RAND_LOGNORMAL]
#' @export
rand_lognormal <- RAND_LOGNORMAL$instantiate

#' @describeIn portal [CLOSED_FORM]
#' @export
closed_form <- CLOSED_FORM$instantiate
