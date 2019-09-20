# Normal
test_that("Normal", {
  expect_silent(screen_lm(mtcars, c("cyl", "mpg"), c("disp")))

  out <- screen_lm(mtcars, c("cyl"), c("disp", "mpg"))
  expect_s3_class(out, "screen_lm")
  expect_equal(nrow(out$results), 2)
  expect_equal(ncol(out$results), 10)
  expect_error(screen_lm(data.frame(x = as.numeric(), y = as.numeric()), "x", "y"), NA)
  expect_error(print(screen_lm(mtcars, "cyl", "mpg")), NA)
})

# Test for throwing errors
test_that("Test for Throwing Errors", {
  # Incorrect Input Type
  expect_error(screen_lm(1, 1, 1))
  expect_error(screen_lm(c("sdf"), mtcars, mtcars))
  expect_error(screen_lm(mtcars, mtcars, "cyl"))
  expect_error(screen_lm(mtcars, "cyl", mtcars))

  # Responses and Factors not in DataFrame
  expect_error(screen_lm(mtcars, c("cyl", "mpg"), "asdf"))
  expect_error(screen_lm(mtcars, "asdf", c("cyl", "mpg")))
})

# Test for Warnings
test_that("Test for Warnings", {
  # Shared responses and factors
  expect_warning(screen_lm(mtcars, c("cyl"), c("cyl", "mpg")))
})
