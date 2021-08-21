#' Find data relating to a Reddit user
#'
#' Given a valid Reddit user name, obtain a list consisting of
#' general information about the user, their comments and threads
#'
#' @param user A string representing a valid Reddit user name
#' @return A list with "about" (list), "comments" (data frame) and "threads" (data frame)
#' @examples
#' \dontrun{
#' get_user_content("memes")
#' }
#' @export
get_user_content <- function(user) {
  user_posts <- build_user_search_url(user) |> parse_request_url(data_builder = user_data_builder)
  list(
    about = build_user_info_url(user) |> url_to_json() |> build_user_info_list(),
    comments = concat_posts(user_posts, "comments"),
    threads = concat_posts(user_posts, "threads")
  )
}


# Concatenate nested pages of the same content type
concat_posts <- function(posts, content_type){
  lapply(posts, function(page) page[[content_type]]) |>
    remove_na() |>
    rbind_list() |>
    dedup_df()
}


# A builder function that takes a json object and returns a list with comments and threads data frames
user_data_builder <- function(json) list(
  comments = maybe_build_df(json, sapply(json, is_comment), build_user_comment_df),
  threads = maybe_build_df(json, sapply(json, is_thread), build_user_thread_df)
)


# Build user info list
build_user_info_list <- function(json) list(
  created_utc = json$data$created_utc |> timestamp_to_date(),
  name = json$data$name,
  is_employee = json$data$is_employee,
  is_mod = json$data$is_mod,
  is_gold = json$data$is_gold,
  thread_karma = json$data$link_karma,
  comment_karma = json$data$comment_karma
)


# Build thread data frame
build_user_thread_df <- function(json) {
  data.frame(
    url = extract_json_attribute(json, "url"),
    date_utc = extract_json_attribute(json, "created_utc") |> timestamp_to_date(),
    subreddit = extract_json_attribute(json, "subreddit"),
    author = extract_json_attribute(json, "author"),
    title = extract_json_attribute(json, "title"),
    text = extract_json_attribute(json, "selftext"),
    golds = extract_json_attribute(json, "gilded"),
    score = extract_json_attribute(json, "score"),
    ups = extract_json_attribute(json, "ups"),
    downs = extract_json_attribute(json, "downs"),
    stringsAsFactors = FALSE
  )
}


# Build comment data frame
build_user_comment_df <- function(json) {
  data.frame(
    url = extract_json_attribute(json, "link_permalink"),
    date_utc = extract_json_attribute(json, "created_utc") |> timestamp_to_date(),
    subreddit = extract_json_attribute(json, "subreddit"),
    thread_author = extract_json_attribute(json, "link_author"),
    comment_author = extract_json_attribute(json, "author"),
    thread_title = extract_json_attribute(json, "link_title"),
    comment = extract_json_attribute(json, "body"),
    score = extract_json_attribute(json, "score"),
    up = extract_json_attribute(json, "ups"),
    downs = extract_json_attribute(json, "downs"),
    golds = extract_json_attribute(json, "gilded"),
    stringsAsFactors = FALSE
  )
}


# Build base user URL
build_user_base_url <- function(user) REDDIT_URL %+% "/user/" %+% user


# Build a URL to search for user content
build_user_search_url <- function(user) build_user_base_url(user) %+% ".json?limit=100"


# Build a URL to get information about the user
build_user_info_url <- function(user) build_user_base_url(user) %+% "/about.json"


# Check if an entry is a comment
is_comment <- function(entry) entry$kind == "t1"


# Check if an entry is a thread (also referred to as a link)
is_thread <- function(entry) entry$kind == "t3"
