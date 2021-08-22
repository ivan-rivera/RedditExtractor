
JSON_URL_COUNT <- 61
all_are_strings <- function(s) all(sapply(s, function(z) is.character(z)))
all_are_numeric <- function(n) all(sapply(n, function(z) is.numeric(n)))


test_that("build_homepage_url works correctly", {
  expect_equal(
    build_homepage_url(sort_by="top", subreddit=NA, period="month"),
    "https://www.reddit.com/top.json?t=month&limit=100"
  )
  expect_equal(
    build_homepage_url(sort_by="rising", subreddit="cats", period="all"),
    "https://www.reddit.com/r/cats/rising.json?t=all&limit=100"
  )
  expect_error(
    build_homepage_url(sort_by="relevance", subreddit=NA, period="all"),
    "sort_by must be one of:"
  )
  expect_error(
    build_homepage_url(sort_by="hot", subreddit=NA, period="invalid"),
    "period must be one of:"
  )
})


test_that("build_keywords_url works correctly", {
  expect_equal(
    build_keywords_url("test", "relevance", NA, "month"),
    "https://www.reddit.com/search.json?q=test&sort=relevance&t=month&limit=100"
  )
  expect_equal(
    build_keywords_url("test", "relevance", "cats", "month"),
    "https://www.reddit.com/r/cats/search.json?restrict_sr=on&q=test&sort=relevance&t=month&limit=100"
  )
  expect_error(
    build_keywords_url("test", "invalid", NA, "month"),
    "sort_by must be one of:"
  )
  expect_error(
    build_keywords_url("test", "top", NA, "invalid"),
    "period must be one of:"
  )
})


test_that("JSON attributes extraction fetches the data as expected", {
  children <- get_children(thread_urls_json)

  # titles
  titles <- extract_json_attribute(children, "title")
  expect_true(all_are_strings(titles))
  expect_equal(length(titles), JSON_URL_COUNT)

  # created time
  created_time <- extract_json_attribute(children, "created_utc")
  expect_true(all_are_numeric(created_time))
  expect_equal(length(created_time), JSON_URL_COUNT)

  # subreddits
  subreddits <- extract_json_attribute(children, "subreddit")
  expect_true(all_are_strings(subreddits))
  expect_equal(length(subreddits), JSON_URL_COUNT)

  # number of comments
  num_comments <- extract_json_attribute(children, "num_comments")
  expect_true(all_are_numeric(num_comments))
  expect_equal(length(num_comments), JSON_URL_COUNT)

  # permalink
  permalinks <- extract_json_attribute(children, "permalink")
  expect_true(all_are_strings(permalinks))
  expect_equal(length(permalinks), JSON_URL_COUNT)

})


test_that("parse request url function returns a list of NA for empty thread requests", {
  mockery::stub(parse_request_url, 'url_to_json', empty_thread_urls_json)
  expect_equal(parse_request_url("some_url"), list(NA))
})


test_that("parse request URL for non-empty thread requests works", {
  mockery::stub(parse_request_url, 'url_to_json', thread_urls_json)
  parse_request_url("some_url", data_builder = build_thread_df) |>
    remove_na() |>
    length() |>
    expect_gt(0)
})
