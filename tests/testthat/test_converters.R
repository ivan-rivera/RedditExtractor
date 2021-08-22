
test_that("UTF-8 format conversion works", {
  expect_equal(to_utf8("hi123"), "hi123")
  expect_equal(to_utf8("\xe4\xe5\xe6!"), "äåæ!")
})


test_that("space-to-plus conversion is correct", {
  expect_equal(space2plus("i like pies"), "i+like+pies")
})


test_that("timestamps are being parsed into correct dates", {
  expect_equal(timestamp_to_date(1609455600), "2020-12-31")
  expect_equal(timestamp_to_date(1609498800), "2021-01-01")
})


test_that("na2zero works", {
  expect_equal(na2zero(NA), 0)
  expect_equal(na2zero(1), 1)
  expect_equal(na2zero("a"), "a")
})


test_that("null2na works", {
  expect_equal(null2na(NULL), NA)
  expect_equal(null2na(1), 1)
  expect_equal(null2na("a"), "a")
})
