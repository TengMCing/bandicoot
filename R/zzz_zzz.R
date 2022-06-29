# Create portals for class constructor

#' @name portal
#' @title Portals to class instantiate methods
#' @description These functions are the same as `class$instantiate`.
#' @param ... Arguments passed to `init` method.
#' @param env Environment. The instance environment.
#' @param init_call Call. Contents of the `..init_call..`. It is recommended to
#' leave it as default. Default is `init_call = sys.call()`.
NULL

#' @describeIn portal [BASE]
#' @export
base_ <- function(..., env = new.env(parent = parent.frame()), init_call = sys.call()) {
  BASE$instantiate(..., env = env, init_call = init_call)
}

.onLoad <- function(libname, pkgname) {

  # BASE is an empty environment defined by new.env()
  # Build the BASE class at load-time
  class_BASE(BASE)
}
