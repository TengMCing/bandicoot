test_that("BASE$..type.. is BASE", {
  expect_equal(BASE$..type.., "BASE")
})

test_that("BASE$..class.. is BASE", {
  expect_equal(BASE$..class.., "BASE")
})

test_that("BASE$..method_env.. is a child of parent of BASE", {
  expect_equal(parent.env(BASE$..method_env..), parent.env(BASE))
})

test_that("BASE$..method_env..$self points to BASE", {
  expect_equal(BASE$..method_env..$self, BASE)
})

test_that("BASE$..dict..() runs in ..method_env..", {
  expect_equal(environment(BASE$..dict..), BASE$..method_env..)
})

test_that("BASE$..dict..() returns all object names in the environment", {
  expect_equal(BASE$..dict..(), names(BASE))
})

test_that("BASE$..str..() returns the correct string", {
  expect_equal(BASE$..str..(), "<BASE class>")
})

test_that("BASE$..len..() returns NULL", {
  expect_equal(BASE$..len..(), NULL)
})

test_that("BASE$has_attr() works", {
  expect_equal(BASE$has_attr("..class.."), TRUE)
  expect_equal(BASE$has_attr("gggg"), FALSE)
})

test_that("BASE$get_attr() returns NULL if attr does not exist", {
  expect_equal(BASE$get_attr("..class.."), "BASE")
  expect_equal(BASE$get_attr("gggg"), NULL)
})

test_that("BASE$set_attr() works", {
  test <- base_()

  expect_equal(test$set_attr("test", 1)$test, 1)
})

test_that("BASE$del_attr() can delete attributes and will not crash if it does not exist", {
  test <- base_()

  expect_equal(test$set_attr("test", 1)$del_attr("test")$test, NULL)
  expect_equal(test$del_attr("..class..")$..class.., NULL)
  expect_equal(test$del_attr("x")$x, NULL)
})

test_that("BASE$..methods..() only returns function names", {
  expect_equal(all(unlist(lapply(BASE$..methods..(), function(x) {is.function(BASE[[x]])}))),
               TRUE)
})

test_that("BASE$..init.. returns self", {
  expect_equal(BASE$..init..(), BASE)
})

test_that("BASE$..new..() will not run ..init..() and record call correctly", {
  test <- new_class(class_name = "test")
  register_method(test, ..init.. = function() {self$x <- 1})

  test2 <- test$..new..()
  expect_equal(test2$x, NULL)
  expect_equal(test2$..repr..(), "test$..new..()")

  test2$..init..()
  expect_equal(test2$x, 1)
})

test_that("BASE$instantiate() runs ..new..() and ..init..()", {
  test <- new_class(class_name = "test")
  register_method(test, ..init.. = function() {self$x <- 1})

  test2 <- test$instantiate()
  expect_equal(test2$x, 1)
  expect_equal(test2$..repr..(), "test$instantiate()")
})
