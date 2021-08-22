
test_that("subreddit URL constructor works", {
  expect_equal(
    build_subreddit_search_url("hello world"),
    "https://www.reddit.com/subreddits/search.json?limit=100&q=hello+world"
  )
})


test_that("subreddit data builder works", {
  mockery::stub(parse_request_url, 'url_to_json', subreddit_json)
  parse_request_url(
    "some_url",
    data_builder = build_subreddit_df
  )[[1]] |>
    is.data.frame() |>
    expect_true()
})
