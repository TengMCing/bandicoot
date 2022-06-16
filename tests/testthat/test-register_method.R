test_that("register_method() can register one method and access self attribute", {
  a <- function() self$x
  e <- new.env()
  e$x <- 0
  register_method(e, a = a)

  expect_equal(e$a(), 0)
})

test_that("register_method() can register two methods and access self attribute", {
  a <- function() self$x
  b <- function() self$x + 1
  e <- new.env()
  e$x <- 0
  register_method(e, a = a, b = b)

  expect_equal(e$b(), 1)
})

test_that("register_method() can use container name other than ..method_env..", {
  a <- function() self$x
  e <- new.env()
  e$x <- 0
  register_method(e, a = a, container_name = "aa")

  expect_equal(e$a(), 0)
})


test_that("register_method() can detect ..method_env.. is not an environment", {
  a <- function() self$x
  e <- new.env()
  e$..method_env.. <- 1
  e$x <- 0

  expect_error(register_method(e, a = a),
               "..method_env.. exists, but it is not an environment! Consider remove it.",
               ignore.case = TRUE)
})

test_that("register_method() can detect ..method_env.. is not a child of the parent of the object environment", {
  a <- function() self$x
  e <- new.env()
  d <- new.env()
  e$..method_env.. <- new.env(parent = d)
  e$x <- 0

  expect_error(register_method(e, a = a),
               "..method_env.. exists, but it is not a child of the parent of the instance environment! Consider remove it.",
               ignore.case = TRUE)
})

test_that("register_method() can detect ..method_env.. contains variables other than self", {
  a <- function() self$x
  e <- new.env()
  e$..method_env.. <- new.env(parent = parent.env(e))
  e$..method_env..$b <- 1
  e$x <- 0

  expect_warning(register_method(e, a = a),
                 "The container is not empty!",
                 ignore.case = TRUE)
})

test_that("register_method() can use different self names (e.g. this)", {
  a <- function() this$x
  e <- new.env()
  e$x <- 0
  register_method(e, a = a, self_name = "this")

  expect_equal(e$a(), 0)
})

test_that("register_method can detect self does not point to the object environment", {
  a <- function() this$x
  e <- new.env()
  e$..method_env.. <- new.env(parent = parent.env(e))
  e$..method_env..$self <- new.env()
  e$x <- 0

  expect_error(register_method(e, a = a, self_name = "self"),
               "self exists, but it is not the same as the provided environment! Consider remove it.",
               ignore.case = TRUE)
})


