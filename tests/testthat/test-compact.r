context("Testing compact")

test_that("'compact' filters NULL from lists", {
  expect_that( compact(list(1,NULL,2)), equals( list(1,2) ))
  expect_that( compact(list("",NULL,NA,list())), equals( list("",NA,list()) ))
  expect_that( compact(c(1,2)), equals( c(1,2) ))
})

test_that("'compactNA' filters only NA from lists and vectors", {
  expect_that(compactNA(list(1,NA,2)), equals( list(1,2) ))
  expect_that(compactNA(list("",NULL,NA,list())), equals( list("",NULL,list()) ))
  expect_that(compactNA(c(1,NA,2)), equals( c(1,2) ))
  expect_that(compactNA(integer()), equals( integer(0) ))
  expect_that(compactNA(NA), equals( logical(0) ))
  expect_that(compactNA(list(NA)), equals( list() ))
})

