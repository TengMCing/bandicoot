# RAND_VAR ----------------------------------------------------------------

test_that("RAND_VAR$..type.. is RAND_VAR", {
  expect_equal(RAND_VAR$..type.., "RAND_VAR")
})

test_that("RAND_VAR$..class.. only contains RAND_VAR and BASE", {
  expect_equal(RAND_VAR$..class.., c("RAND_VAR", "BASE"))
})

test_that("RAND_VAR instance has attribute dist and prm", {
  expect_equal(rand_var()$has_attr(c("prm", "dist")), c(TRUE, TRUE))
})

test_that("RAND_VAR$..init..() can store any variables in prm", {
  test <- rand_var(dist = "abc", prm = list(a = 1, b = 2))

  expect_equal(test$dist, "abc")
  expect_equal(test$prm$a, 1)
  expect_equal(test$prm$b, 2)
})

test_that("RAND_VAR$..str..() produce correct string", {
  test <- rand_var(dist = "abc", prm = list(a = 1, b = 2))

  expect_equal(test$..str..(), "<RAND_VAR object>\n [a: 1, b: 2]")
  expect_equal(RAND_VAR$..str..(), "<RAND_VAR class>")
})

test_that("RAND_VAR$E() is a placeholder", {
  test <- rand_var(dist = "abc", prm = list(a = 1, b = 2))

  expect_equal(test$E(), NA)
})

test_that("RAND_VAR$Var() is a placeholder", {
  test <- rand_var(dist = "abc", prm = list(a = 1, b = 2))

  expect_equal(test$Var(), NA)
})

test_that("RAND_VAR$set_prm() can correctly set the prm", {
  test <- rand_var(dist = "abc", prm = list(a = 1, b = 2))
  test$set_prm("c", 1)
  test$set_prm("a", 3)

  expect_equal(test$prm$a, 3)
  expect_equal(test$prm$c, 1)
})


# RAND_NORMAL -------------------------------------------------------------

test_that("RAND_NORMAL$..type.. is RAND_NORMAL", {
  expect_equal(RAND_NORMAL$..type.., "RAND_NORMAL")
})

test_that("RAND_NORMAL$..class.. is RAND_NORMAL, RAND_VAR and BASE", {
  expect_equal(RAND_NORMAL$..class.., c("RAND_NORMAL", "RAND_VAR", "BASE"))
})

test_that("RAND_NORMAL instance has attribute dist and prm", {
  expect_equal(rand_normal()$has_attr(c("prm", "dist")), c(TRUE, TRUE))
})

test_that("RAND_NORMAL$..init..() capture prm correctly", {
  expect_equal(rand_normal(1, 2)$prm$mu, 1)
  expect_equal(rand_normal(1, 2)$prm$sigma, 2)
})

test_that("RAND_NORMAL$gen() can correctly produce values", {
  set.seed(10086)
  expect_lt(abs(mean(rand_normal(0, 1)$gen(100000))), 1)
  expect_lt(abs(sd(rand_normal(0, 1)$gen(100000)) - 1), 1)
})

test_that("RAND_NORMAL$gen() can correctly produce values when mu and sigma depends on observations", {
  set.seed(10086)
  expect_lt(abs(mean(rand_normal()$gen(10000, mu = c(rep(0, 5000), rep(10, 5000)))) - 5), 1)
  expect_lt(abs(sd(rand_normal()$gen(10000, sigma = c(rep(1, 5000), rep(10, 5000)))) - sqrt(0.5+0.5*10^2)), 1)
})

test_that("RAND_NORMAL$E() can produce correct value", {
  expect_equal(rand_normal()$E(), 0)
  expect_equal(rand_normal(1)$E(), 1)
})

test_that("RAND_NORMAL$Var() can produce correct value", {
  expect_equal(rand_normal()$Var(), 1)
  expect_equal(rand_normal(sigma = 2)$Var(), 4)
})

test_that("RAND_NORMAL$..str..() produce correct string", {
  test <- rand_normal()

  expect_equal(test$..str..(), "<RAND_NORMAL object>\n [mu: 0, sigma: 1]")
  expect_equal(RAND_NORMAL$..str..(), "<RAND_NORMAL class>")
})


# RAND_UNIFORM ------------------------------------------------------------

test_that("RAND_UNIFORM$..type.. is RAND_UNIFORM", {
  expect_equal(RAND_UNIFORM$..type.., "RAND_UNIFORM")
})

test_that("RAND_UNIFORM$..class.. is RAND_UNIFORM, RAND_VAR and BASE", {
  expect_equal(RAND_UNIFORM$..class.., c("RAND_UNIFORM", "RAND_VAR", "BASE"))
})

test_that("RAND_UNIFORM instance has attribute dist and prm", {
  expect_equal(rand_uniform()$has_attr(c("prm", "dist")), c(TRUE, TRUE))
})

test_that("RAND_UNIFORM$..init..() capture prm correctly", {
  expect_equal(rand_uniform(1, 2)$prm$a, 1)
  expect_equal(rand_uniform(1, 2)$prm$b, 2)
})

test_that("RAND_UNIFORM$gen() can correctly produce values", {
  set.seed(10086)
  expect_lt(abs(mean(rand_uniform(0, 1)$gen(100000)) - 0.5), 1)
})

test_that("RAND_UNIFORM$gen() can correctly produce values when a and b depends on observations", {
  set.seed(10086)
  expect_lt(abs(mean(rand_uniform()$gen(10000, a = c(rep(0, 5000), rep(1, 5000)), b = 2)) - 1.25), 1)
  expect_lt(abs(mean(rand_uniform()$gen(10000, b = c(rep(1, 5000), rep(2, 5000)))) - 0.75), 1)
})

test_that("RAND_UNIFORM$E() can produce correct value", {
  expect_equal(rand_uniform()$E(), 0.5)
  expect_equal(rand_uniform(1, 2)$E(), 1.5)
})

test_that("RAND_UNIFORM$Var() can produce correct value", {
  expect_equal(rand_uniform()$Var(), 1/12)
  expect_equal(rand_uniform(b = 2)$Var(), 1/3)
})

test_that("RAND_UNIFORM$..str..() produce correct string", {
  test <- rand_uniform()

  expect_equal(test$..str..(), "<RAND_UNIFORM object>\n [a: 0, b: 1]")
  expect_equal(RAND_UNIFORM$..str..(), "<RAND_UNIFORM class>")
})


# RAND_UNIFORM_D ----------------------------------------------------------

test_that("RAND_UNIFORM_D$..type.. is RAND_UNIFORM_D", {
  expect_equal(RAND_UNIFORM_D$..type.., "RAND_UNIFORM_D")
})

test_that("RAND_UNIFORM_D$..class.. is RAND_UNIFORM_D, RAND_VAR and BASE", {
  expect_equal(RAND_UNIFORM_D$..class.., c("RAND_UNIFORM_D", "RAND_VAR", "BASE"))
})

test_that("RAND_UNIFORM_D instance has attribute dist and prm", {
  expect_equal(rand_uniform_d()$has_attr(c("prm", "dist")), c(TRUE, TRUE))
})

test_that("RAND_UNIFORM_D$..init..() capture prm correctly", {
  expect_equal(rand_uniform_d(1, 2, 5, TRUE)$prm$a, 1)
  expect_equal(rand_uniform_d(1, 2, 5, TRUE)$prm$b, 2)
  expect_equal(rand_uniform_d(1, 2, 5, TRUE)$prm$k, 5)
  expect_equal(rand_uniform_d(1, 2, 5, TRUE)$prm$even, TRUE)
})

test_that("RAND_UNIFORM_D$gen() can correctly produce values", {
  set.seed(10086)
  expect_lt(abs(mean(rand_uniform_d(0, 1)$gen(100000)) - 0.5), 1)
  expect_equal(length(unique(rand_uniform_d()$gen(100000))), 5)
})

test_that("RAND_UNIFORM_D$gen() can correctly produce values when a, b and k depends on observations", {
  set.seed(10086)
  expect_lt(abs(mean(rand_uniform_d()$gen(10000, a = c(rep(0, 5000), rep(1, 5000)), b = 2)) - 1.25), 1)
  expect_lt(abs(mean(rand_uniform_d()$gen(10000, b = c(rep(1, 5000), rep(2, 5000)))) - 0.75), 1)
})

test_that("RAND_UNIFORM_D$E() can produce correct value", {
  expect_equal(rand_uniform_d()$E(), 0.5)
  expect_equal(rand_uniform_d(1, 2)$E(), 1.5)
})

test_that("RAND_UNIFORM_D$Var() can produce correct value", {
  expect_equal(rand_uniform_d()$Var(), 1/12)
  expect_equal(rand_uniform_d(b = 2)$Var(), 1/3)
})

test_that("RAND_UNIFORM_D$..str..() produce correct string", {
  test <- rand_uniform_d()

  expect_equal(test$..str..(), "<RAND_UNIFORM_D object>\n [a: 0, b: 1, k: 5, even: 0]")
  expect_equal(RAND_UNIFORM_D$..str..(), "<RAND_UNIFORM_D class>")
})


# RAND_LOGNORMAL ----------------------------------------------------------

test_that("RAND_LOGNORMAL$..type.. is RAND_LOGNORMAL", {
  expect_equal(RAND_LOGNORMAL$..type.., "RAND_LOGNORMAL")
})

test_that("RAND_LOGNORMAL$..class.. is RAND_LOGNORMAL, RAND_VAR and BASE", {
  expect_equal(RAND_LOGNORMAL$..class.., c("RAND_LOGNORMAL", "RAND_VAR", "BASE"))
})

test_that("RAND_LOGNORMAL instance has attribute dist and prm", {
  expect_equal(rand_lognormal()$has_attr(c("prm", "dist")), c(TRUE, TRUE))
})

test_that("RAND_LOGNORMAL$..init..() capture prm correctly", {
  expect_equal(rand_lognormal(1, 2)$prm$mu, 1)
  expect_equal(rand_lognormal(1, 2)$prm$sigma, 2)
})

test_that("RAND_LOGNORMAL$gen() can correctly produce values", {
  set.seed(10086)
  expect_lt(abs(mean(rand_lognormal(0, 1)$gen(100000)) - exp(0.5)), 1)
  expect_lt(abs(var(rand_lognormal(0, 1)$gen(100000)) - 4.670774), 1)
})

test_that("RAND_LOGNORMAL$gen() can correctly produce values when mu and sigma depends on observations", {
  set.seed(10086)
  expect_length(rand_lognormal()$gen(10000, mu = c(rep(0, 5000), rep(10, 5000))), 10000)
  expect_length(rand_lognormal()$gen(10000, sigma = c(rep(1, 5000), rep(2, 5000))), 10000)
})

test_that("RAND_LOGNORMAL$E() can produce correct value", {
  expect_equal(rand_lognormal()$E(), exp(0.5))
  expect_equal(rand_lognormal(1)$E(), exp(1.5))
})

test_that("RAND_LOGNORMAL$Var() can produce correct value", {
  expect_equal(rand_lognormal()$Var(), (exp(1) - 1) * exp(1))
  expect_equal(rand_lognormal(sigma = 2)$Var(), (exp(4) - 1) * exp(4))
})

test_that("RAND_LOGNORMAL$..str..() produce correct string", {
  test <- rand_lognormal()

  expect_equal(test$..str..(), "<RAND_LOGNORMAL object>\n [mu: 0, sigma: 1]")
  expect_equal(RAND_LOGNORMAL$..str..(), "<RAND_LOGNORMAL class>")
})
