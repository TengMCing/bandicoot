test_that("bandicoot::BASE$..type.. is BASE", {
  expect_equal(bandicoot::BASE$..type.., "BASE")
})

test_that("bandicoot::BASE$..class.. is BASE", {
  expect_equal(bandicoot::BASE$..class.., "BASE")
})

test_that("bandicoot::BASE$..method_env.. is a child of parent of BASE", {
  expect_equal(parent.env(bandicoot::BASE$..method_env..), parent.env(bandicoot::BASE))
})

test_that("bandicoot::BASE$..method_env..$self points to BASE", {
  expect_equal(bandicoot::BASE$..method_env..$self, bandicoot::BASE)
})

test_that("bandicoot::BASE$..dict..() runs in ..method_env..", {
  expect_equal(environment(bandicoot::BASE$..dict..), bandicoot::BASE$..method_env..)
})

test_that("bandicoot::BASE$..dict..() returns all object names in the environment", {
  expect_equal(bandicoot::BASE$..dict..(), names(bandicoot::BASE))
})

test_that("bandicoot::BASE$..str..() returns the correct string", {
  expect_equal(bandicoot::BASE$..str..(), "<BASE class>")
})

test_that("bandicoot::BASE$..len..() returns NULL", {
  expect_equal(bandicoot::BASE$..len..(), NULL)
})

test_that("bandicoot::BASE$has_attr() works", {
  expect_equal(bandicoot::BASE$has_attr("..class.."), TRUE)
  expect_equal(bandicoot::BASE$has_attr("gggg"), FALSE)
})

test_that("bandicoot::BASE$get_attr() returns NULL if attr does not exist", {
  expect_equal(bandicoot::BASE$get_attr("..class.."), "BASE")
  expect_equal(bandicoot::BASE$get_attr("gggg"), NULL)
})

test_that("bandicoot::BASE$set_attr() works", {
  test <- bandicoot::base_()

  expect_equal(test$set_attr("test", 1)$test, 1)
})

test_that("bandicoot::BASE$del_attr() can delete attributes and will not crash if it does not exist", {
  test <- bandicoot::base_()

  expect_equal(test$set_attr("test", 1)$del_attr("test")$test, NULL)
  expect_equal(test$del_attr("..class..")$..class.., NULL)
  expect_equal(test$del_attr("x")$x, NULL)
})

test_that("bandicoot::BASE$..methods..() only returns function names", {
  expect_equal(all(unlist(lapply(bandicoot::BASE$..methods..(), function(x) {is.function(bandicoot::BASE[[x]])}))),
               TRUE)
})

test_that("bandicoot::BASE$..init.. returns self", {
  expect_equal(bandicoot::BASE$..init..(), bandicoot::BASE)
})

test_that("bandicoot::BASE$..new..() will not run ..init..() and record call correctly", {
  test <- bandicoot::new_class(class_name = "test")
  bandicoot::register_method(test, ..init.. = function() {self$x <- 1})

  test2 <- test$..new..()
  expect_equal(test2$x, NULL)
  expect_equal(test2$..repr..(), "test$..new..()")

  test2$..init..()
  expect_equal(test2$x, 1)
})

test_that("bandicoot::BASE$instantiate() runs ..new..() and ..init..()", {
  test <- bandicoot::new_class(class_name = "test")
  bandicoot::register_method(test, ..init.. = function() {self$x <- 1})

  test2 <- test$instantiate()
  expect_equal(test2$x, 1)
  expect_equal(test2$..repr..(), "test$instantiate()")
})
