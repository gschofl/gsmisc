context("Testing string utils")

str <- c("Lorem, ipsum, dolor", "consectetur, adipisicing, elit")

test_that("strsplitN: first and last element are returned", {
  firstWords <- c("Lorem", "consectetur")
  lastWords <- c("dolor", "elit")
  expect_that(strsplitN(str, ", ", 1), equals(firstWords)) 
  expect_that(strsplitN(str, ", ", 1, from="end"), equals(lastWords)) 
})

test_that("trim works as expected", {
  expect_equal(trim("  xxx\n\t"), "xxx")
})
