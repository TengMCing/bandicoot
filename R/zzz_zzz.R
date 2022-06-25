# Create portals for class constructor

#' @name portal
#' @title Portals to class instantiate methods
#' @description These functions are the same as `class$instantiate`.
#' @param ... Arguments passed to `init` method.
#' @param env Environment. The instance environment.
NULL

#' @describeIn portal [BASE]
#' @export
base_ <- function(..., env = new.env(parent = parent.frame())) {
  BASE$instantiate(..., env = env)
}

.onLoad <- function(libname, pkgname) {

  # BASE is an empty environment defined by new.env()
  # Build the BASE class at load-time
  class_BASE(BASE)
}
