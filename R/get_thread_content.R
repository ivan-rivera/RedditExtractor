#' Get thread contents of Reddit URLs
#'
#' This function takes a collection of URLs and returns a list with 2 data frames:
#' 1. a data frame containing meta data describing each thread
#' 2. a data frame with comments found in all threads
#'
#' The URLs are being retained in both tables which would allow you to join them if needed
#'
#' @param urls A vector of strings pointing to a Reddit thread
#' @return A list with 2 data frames "threads" and "comments"
#' @export
get_thread_content <- function(urls){
  data <- lapply(urls, parse_thread_url)
  list(
    threads = lapply(data, function(z) z[["thread"]]) |> rbind_list(),
    comments = lapply(data, function(z) z[["comments"]]) |> remove_na() |> rbind_list()
  )
}


# Build a data frame with thread attributes of interest
build_thread_content_df <- function(json, request_url) data.frame(
  url = strip_json(request_url),
  author = json$author,
  date = timestamp_to_date(json$created_utc),
  timestamp = json$created_utc,
  title = json$title,
  text = json$selftext,
  subreddit = json$subreddit,
  score = json$score,
  upvotes = json$ups,
  downvotes = json$downs,
  up_ratio = json$upvote_ratio,
  total_awards_received = json$total_awards_received,
  golds = json$gilded,
  cross_posts = json$num_crossposts,
  comments = json$num_comments,
  stringsAsFactors = FALSE
)


# Build a data frame with comments and their attributes.
build_comments_content_df <- function(json, request_url) data.frame(
  url = strip_json(request_url),
  author = extract_comments_attributes(json, "author"),
  date = extract_comments_attributes(json, "created_utc") |> timestamp_to_date(),
  timestamp = extract_comments_attributes(json, "created_utc"),
  score = extract_comments_attributes(json, "score"),
  upvotes = extract_comments_attributes(json, "ups"),
  downvotes = extract_comments_attributes(json, "downs"),
  golds = extract_comments_attributes(json, "gilded"),
  comment = extract_comments_attributes(json, "body"),
  comment_id = build_comment_ids(json),
  stringsAsFactors = FALSE
)
