
test_that("a regular user can be parsed correctly", {
  mockery::stub(parse_request_url, 'url_to_json', regular_user_json)
  results <- parse_request_url(
    request_url = "some_url",
    data_builder = user_data_builder
  )[[1]]
  expect_equal(names(results), c("comments", "threads"))
  expect_equal(results$comments, NA)
  expect_equal(ncol(results$threads), 11)
})


test_that("about user info works", {
  about_user_json |>
    build_user_info_list() |>
    length() |>
    expect_equal(8)
})
