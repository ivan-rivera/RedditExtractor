

test_that("list of data frames is being correctly stacked", {
  d1 <- data.frame(a=NULL)
  d2 <- data.frame(x=1, y=2)
  d3 <- data.frame(x=3, y=4)
  d4 <- data.frame(x=c(5,6), y=c(7,8))
  d5 <- data.frame(x=c(1,3,5,6), y=c(2,4,7,8))
  dl1 <- list(d1)
  dl2 <- list(d2, d3, d4)
  expect_equal(rbind_list(dl1), d1)
  expect_equal(rbind_list(dl2), d5)
})


test_that("duplicates are being removed from data frames", {
  d1 <- data.frame(a=c(1,2), b=c(1,2))
  d2 <- data.frame(a=c(1,1), b=c(1,2))
  d3 <- data.frame(a=c(1,1), b=c(1,1))
  d4 <- data.frame(a=1, b=1)
  expect_equal(dedup_df(d1), d1)
  expect_equal(dedup_df(d2), d2)
  expect_equal(dedup_df(d3), d4)
})


test_that("remove_na works", {
  expect_equal(remove_na(c(1,2,3)), c(1,2,3))
  expect_equal(remove_na(c(NA,1,2,3)), c(1,2,3))
  expect_equal(remove_na(c(1,2,3,NA)), c(1,2,3))
  expect_equal(remove_na(c(1,2,NA,3)), c(1,2,3))
  expect_equal(remove_na(list()), list())
})
