test_that("CLOSED_FORM$..type.. is CLOSED_FORM", {
  expect_equal(CLOSED_FORM$..type.., "CLOSED_FORM")
})
#
# test_that("RAND_VAR$..type.. is RAND_VAR", {
#   expect_equal(RAND_VAR$..type.., "RAND_VAR")
# })
#
# test_that("RAND_VAR$..class.. only contains RAND_VAR and BASE", {
#   expect_equal(RAND_VAR$..class.., c("RAND_VAR", "BASE"))
# })
#
# test_that("RAND_VAR instance has attribute dist and prm", {
#   expect_equal(rand_var()$has_attr(c("prm", "dist")), c(TRUE, TRUE))
# })
#
# test_that("RAND_VAR$..init..() can store any variables in prm", {
#   test <- rand_var(dist = "abc", prm = list(a = 1, b = 2))
#
#   expect_equal(test$dist, "abc")
#   expect_equal(test$prm$a, 1)
#   expect_equal(test$prm$b, 2)
# })
#
# test_that("RAND_VAR$..str..() produce correct string", {
#   test <- rand_var(dist = "abc", prm = list(a = 1, b = 2))
#
#   expect_equal(test$..str..(), "<RAND_VAR object>\n [a: 1, b: 2]")
#   expect_equal(RAND_VAR$..str..(), "<RAND_VAR class>")
# })
#
# test_that("RAND_VAR$E() is a placeholder", {
#   test <- rand_var(dist = "abc", prm = list(a = 1, b = 2))
#
#   expect_equal(test$E(), NA)
# })
#
# test_that("RAND_VAR$Var() is a placeholder", {
#   test <- rand_var(dist = "abc", prm = list(a = 1, b = 2))
#
#   expect_equal(test$Var(), NA)
# })
#
# test_that("RAND_VAR$set_prm() can correctly set the prm", {
#   test <- rand_var(dist = "abc", prm = list(a = 1, b = 2))
#   test$set_prm("c", 1)
#   test$set_prm("a", 3)
#
#   expect_equal(test$prm$a, 3)
#   expect_equal(test$prm$c, 1)
# })
