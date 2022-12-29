test_that("multiplication works", {
  df1 <- data.frame(id = 1:6, x = c(1, 1.5, 5, 5.5, 10, 10.5), y = c(1, 1.5, 5, 5.5, 10, 10.5))
  expect_identical(pairing(df1, 'id'), list(c(2L,1L), c(4L, 3L), c(6L,5L)))
})
