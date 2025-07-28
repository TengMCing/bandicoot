
test_that("make_instantiator() correctly make a wrapper function", {

  A <- new_class(class_name = "A")
  register_method(A, ..init.. = function(num = 123L) self$num <- num)
  a <- make_instantiator(A)

  expect_equal(a()$num, 123L)
  expect_equal(a(456L)$num, 456L)

  a_2 <- make_instantiator(A, new_defaults = alist(num = 456L))
  expect_equal(a_2()$num, 456L)

  expect_warning(make_instantiator(A, new_defaults = alist(nn = 123L)),
                 "Unmatched formal argument `nn` will be ignored!")

})
