
context("frequency")

# TODO: test for proportion (bin width x density)

# frequency values test ---------------------------------------------------

test_that(desc = "frequency output integer is ok", code = {

  # x <- vector(mode = "numeric", length = 100)
  x <- rep(c(5, 10, 15, 20, 25), each = 20)

  f <- Frequency(x, binWidth = 5)

  expect_equal(object = f[["counts"]], expected = rep(20, times = 5))

})

test_that(desc = "frequency output decimal is ok", code = {

  # x <- vector(mode = "numeric", length = 100)
  x <- seq(from = 0, to = 1, by = 0.01)

  f <- Frequency(x, binWidth = 0.1)

  expected_output <- c(rep(10, times = 10), 1)

  expect_equal(object = f[["counts"]], expected = expected_output)

})

# expected warnings -------------------------------------------------------

test_that(desc = "frequency warning is ok", code = {

  expect_warning(
    object = Frequency(c(5, 10, 15, 20, 25, NA), binWidth = 5),
    expected = "Removed 1 value(s) due to NA."
  )

})
