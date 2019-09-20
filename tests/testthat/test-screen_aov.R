# Normal
test_that("Normal", {
  expect_silent(screen_aov(mtcars, c("cyl", "mpg"), c("disp")))

  out <- screen_aov(mtcars, c("cyl"), c("disp", "mpg"))
  expect_s3_class(out, "screen_aov")
  expect_equal(nrow(out$results), 2)
  expect_equal(ncol(out$results), 12)
  expect_error(screen_aov(data.frame(x = as.numeric(), y = as.numeric()), "x", "y"), NA)
  expect_error(print(screen_aov(mtcars, "cyl", "mpg")), NA)
})

# Test for throwing errors
test_that("Test for Throwing Errors", {
  # Incorrect Input Type
  expect_error(screen_aov(1, 1, 1))
  expect_error(screen_aov(c("sdf"), mtcars, mtcars))
  expect_error(screen_aov(mtcars, mtcars, "cyl"))
  expect_error(screen_aov(mtcars, "cyl", mtcars))

  # Responses and Factors not in DataFrame
  expect_error(screen_aov(mtcars, c("cyl", "mpg"), "asdf"))
  expect_error(screen_aov(mtcars, "asdf", c("cyl", "mpg")))
})

# Test for Warnings
test_that("Test for Warnings", {
  # Shared responses and factors
  expect_warning(screen_aov(mtcars, c("cyl"), c("cyl", "mpg")))
})
