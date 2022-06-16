test_that("new_class() can create a class", {
  a <- new_class(class_name = "test")
  expect_equal(a$..str..(), "<test class>")
})

test_that("new_class() can create a class with manually specified BASE", {
  a <- new_class(BASE, class_name = "test")
  expect_equal(a$..str..(), "<test class>")
})

test_that("new_class() can create an empty class", {
  a <- new_class(class_name = "test", empty_class = TRUE)
  expect_false("..str.." %in% names(a))
})

test_that("new_class() can inherit from two classes and the left class will override the right class", {
  derived <- new_class(class_name = "derived")
  register_method(derived, ..init.. = function(){"test"})

  a <- new_class(derived, BASE, class_name = "test")
  expect_equal(a$..str..(), "<test class>")
  expect_equal(a$..init..(), "test")
  expect_equal(a$..class.., c("test", "derived", "BASE", "BASE"))
})

test_that("new_class() can detect no class name is provided", {
  a <- new.env()
  expect_error(new_class(BASE, env = a),
               "`class_name` is null!",
               ignore.case = TRUE)
})

