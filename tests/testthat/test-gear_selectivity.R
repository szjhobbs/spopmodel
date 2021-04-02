
context("gear-selectivity")

# TODO:

# frequency values test ---------------------------------------------------

test_that(desc = "start values for gear selectivity ok", code = {

  # note: be mindful that if `trammel_catch` changes these test will fail

  l <- trammel_catch[["FL"]]
  m <- trammel_catch[["MeshSize"]]

  # testing median
  expect_equal(
    object = StartVals(len = l, mesh = m)[, "Median"],
    expected = c(`6` = 83, `7` = 86, `8` = 103)
  )

  # testing mean
  expect_equal(
    object = signif(StartVals(len = l, mesh = m)[, "Mean"]),
    expected = c(`6` = 88.7986, `7` = 93.3196, `8` = 110.63)
  )

})

test_that(desc = "adjusted frequency ok", code = {

  # note: be mindful that if `trammel_catch` changes these test will fail

  norm_loc <- ApplyNetFit(
    data = trammel_catch,
    len = FL,
    mesh = MeshSize,
    meshUnit = "in",
    relPower = c(1, 1, 2)
  )$norm.loc

  rr <- RelativeRetention(norm_loc)

  adj_freq <- AdjustedFreq(rr)[1, "AdjFreq"]

  # testing median
  expect_equal(object = adj_freq, expected = 15)

})
