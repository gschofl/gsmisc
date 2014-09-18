context("Testing the null-coalescing operators")

test_that("%||% works as expected", {
  ## work with NULL
  expect_equal(NULL %||% 1, 1)
  expect_equal(1 %||% 2, 1)
  ## work with empty vectors
  expect_equal(numeric() %||% 1, 1)
  expect_equal(numeric(10) %||% 1, numeric(10))
  ## works with empty character strings
  expect_equal("" %||% 1, 1)
  expect_equal("x" %||% 1, "x")
  ## doesn't work on NAs
  expect_equal(NA %||% 1, NA)
  ## doesn't work on vectors
  expect_equal(c("x", "", "z") %||% "1", c("x", "", "z"))
})

test_that("%|% works as expected", {
  expect_equal(c("x", "", "z") %|% "1", c("x", "1", "z"))
  expect_equal(list("x", NULL, "z") %|% NA, list("x", NA, "z"))
  expect_equal(list("x", character(), "z") %|% NA, list("x", NA, "z"))
})

test_that("%|na|% works as expected", {
  expect_equal(NA %|na|% 1, 1)
  expect_equal("" %|na|% 1, "")
  expect_warning(c(NA, NA) %|na|% 1)
})

test_that("%|NA|% works as expected", {
  expect_equal(NA %|NA|% 1, 1)
  expect_equal("" %|NA|% 1, "")
  expect_equal(c(NA, NA) %|NA|% 1, c(1, 1))
})

test_that("%|null|% works as expected", {
  expect_equal(NULL %|null|% 1, 1)
  expect_equal("" %|null|% 1, "")
  expect_equal(numeric() %|null|% 1, numeric())
})


