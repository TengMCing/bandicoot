
test_that("super() can resolve the correct method resolution order 1", {
  # Define class O
  O <- new_class(class_name = "O")
  register_method(O, foo = function() {
    cat("O")
  })

  # Define class F
  F <- new_class(O, class_name = "F")
  register_method(F, foo = function() {
    cat("F")
    use_method(self, super()$foo)()
  })

  # Define class E
  E <- new_class(O, class_name = "E")
  register_method(E, foo = function() {
    cat("E")
    use_method(self, super()$foo)()
  })

  # Define class D
  D <- new_class(O, class_name = "D")
  register_method(D, foo = function() {
    cat("D")
    use_method(self, super()$foo)()
  })

  # Define class C
  C <- new_class(D, F, class_name = "C")
  register_method(C, foo = function() {
    cat("C")
    use_method(self, super()$foo)()
  })

  # Define class B
  B <- new_class(E, D, class_name = "B")
  register_method(B, foo = function() {
    cat("B")
    use_method(self, super()$foo)()
  })

  # Define class A
  A <- new_class(B, C, class_name = "A")
  register_method(A, foo = function() {
    cat("A")
    use_method(self, super()$foo)()
  })

  # To understand why the order is A, B, E, C, D, F, O,
  # please check [https://www.python.org/download/releases/2.3/mro/].
  a <- A$instantiate()
  a$my_name <- "a"
  expect_equal(capture.output(a$foo()),
               "ABECDFO")
})


test_that("super() can resolve the correct method resolution order 2", {
  # Define class O
  O <- new_class(class_name = "O")
  register_method(O, foo = function() {
    cat("O")
  })

  # Define class F
  F <- new_class(O, class_name = "F")
  register_method(F, foo = function() {
    cat("F")
    use_method(self, super()$foo)()
  })

  # Define class E
  E <- new_class(O, class_name = "E")
  register_method(E, foo = function() {
    cat("E")
    use_method(self, super()$foo)()
  })

  # Define class D
  D <- new_class(O, class_name = "D")
  register_method(D, foo = function() {
    cat("D")
    use_method(self, super()$foo)()
  })

  # Define class C
  C <- new_class(D, F, class_name = "C")
  register_method(C, foo = function() {
    cat("C")
    use_method(self, super()$foo)()
  })

  # Define class B
  B <- new_class(D, E, class_name = "B")
  register_method(B, foo = function() {
    cat("B")
    use_method(self, super()$foo)()
  })

  # Define class A
  A <- new_class(B, C, class_name = "A")
  register_method(A, foo = function() {
    cat("A")
    use_method(self, super()$foo)()
  })

  # To understand why the order is A, B, E, C, D, F, O,
  # please check [https://www.python.org/download/releases/2.3/mro/].
  a <- A$instantiate()
  a$my_name <- "a"
  expect_equal(capture.output(a$foo()),
               "ABCDEFO")
})
