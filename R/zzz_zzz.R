.onLoad <- function(libname, pkgname) {

  # BASE is an empty environment defined by new.env()
  # Build the BASE class at load-time
  class_BASE(BASE)
}
