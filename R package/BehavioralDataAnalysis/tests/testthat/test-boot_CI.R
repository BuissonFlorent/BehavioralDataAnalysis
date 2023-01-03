


test_that("boot_CI returns an error when the number of Bootstrap simulations is too small in relation to the confidence level", {
  expect_error(boot_CI(df_num, function(df) mean(df$x), B = 2), "the number of Bootstrap simulations is too small in relation to the confidence level")
})
