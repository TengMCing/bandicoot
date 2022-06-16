test_that("bind_fn_2_env() works", {

  a <- 1
  b <- 2
  d <- 3
  f <- function() a + b + a + d
  f1 <- sub_fn_body_name(f, "a", "b")

  expect_equal(f1(), 9)
})
