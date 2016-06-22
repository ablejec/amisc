library(amisc)
context("Variance")

test_that("Returned value is biased variance", {
  expect_equal(Var(1:4), 1.25)
  expect_equal(Var(1:4),var(1:4)*3/4)
  expect_equal(Var(matrix(1:4,2,2)), 1.25)
})

test_that("na.rm removes missing values", {
  expect_equal(Var(c(1:4,NA),na.rm=TRUE), 1.25)
  expect_equal(Var(c(4,NA,3,NA,2,NA,1),na.rm=TRUE), 1.25)
})

test_that("Returns one value", {
  expect_equal(length(Var(c(1:4,NA),na.rm=TRUE)), 1)
  expect_equal(length(Var(c(4,NA,3,NA,2,NA,1),na.rm=TRUE)), 1)
  expect_equal(length(Var(matrix(1:4,2,2))), 1)
})

