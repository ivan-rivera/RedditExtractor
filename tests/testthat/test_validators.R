
test_that("one_of validator works", {
  fail_category <- "D"
  success_category <- "A"
  categories <- c("A", "B", "C")
  expect_error(validate_one_of(success_category, categories), NA)
  expect_error(validate_one_of(fail_category, categories), "fail_category must be one of: A, B, C")
})
