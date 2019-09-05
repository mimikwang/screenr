# Normal
test_that("Normal", {
  expect_silent(screen_aov(mtcars, c("cyl", "mpg"), c("disp")))

  out <- screen_aov(mtcars, c("cyl"), c("disp", "mpg"))
  expect_s3_class(out, "screen_aov")
  expect_equal(nrow(out$results), 2)
  expect_equal(ncol(out$results), 10)
})

# Test for throwing errors
test_that("Test for Throwing Errors", {
  # Incorrect Input Type
  expect_error(screen_aov(1, 1, 1))
  expect_error(screen_aov(c("sdf"), mtcars, mtcars))

  # Responses and Factors not in DataFrame
  expect_error(screen_aov(mtcars, c("cyl", "mpg"), "asdf"))
})

# Test for Warnings
test_that("Test for Warnings", {
  # Shared responses and factors
  expect_warning(screen_aov(mtcars, c("cyl"), c("cyl", "mpg")))
})
