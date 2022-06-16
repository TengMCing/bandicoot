test_that("bind_fn_2_env() works", {

  e <- new.env()
  e$x <- 1
  a <- function() x
  bind_fn_2_env(e, a)

  expect_equal(a(), 1)
})

test_that("bind_fn_2_env() accept env via named argument", {

  e <- new.env()
  e$x <- 1
  a <- function() x
  bind_fn_2_env(a, env = e)

  expect_equal(a(), 1)
})
