test_that("stratified assignment works on numeric data", {
  strat_df <- strat_assign(df_num, id = 'id')
  expect_equal(sum(strat_df[1:2, 'grp']), 1)
  expect_equal(sum(strat_df[3:4, 'grp']), 1)
  expect_equal(sum(strat_df[5:6, 'grp']), 1)
})

test_that("stratified assignment works when the number of rows isn't divisible by the number of groups", {
  df1 <- data.frame(id = 1:7,
                    x = c(1, 1.5, 5, 5.5, 10, 10.5, 20),
                    y = c(1, 1.5, 5, 5.5, 10, 10.5, 20))
  strat_df <- strat_assign(df1, id = 'id')
  expect_equal(sum(strat_df[1:2, 'grp']), 1)
  expect_equal(sum(strat_df[3:4, 'grp']), 1)
  expect_equal(sum(strat_df[5:6, 'grp']), 1)
})

test_that("stratified assignment works with character data", {
  strat_df <- strat_assign(df_char, id = 'id')
  expect_equal(sum(strat_df[1:2, 'grp']), 1)
  expect_equal(sum(strat_df[3:4, 'grp']), 1)
  expect_equal(sum(strat_df[5:6, 'grp']), 1)
})
