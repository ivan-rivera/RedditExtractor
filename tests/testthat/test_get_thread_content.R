
test_that("thread URL parser works", {
  mockery::stub(parse_thread_url, 'url_to_json', thread_json)
  parsed_thread <- parse_thread_url("some_url")
  expect_equal(ncol(parsed_thread$comments), 10)
  expect_equal(ncol(parsed_thread$thread), 15)
})
